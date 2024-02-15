"""
# 6502 Assembler with Julia-based macros and expressions

3 passes:

1. compile macros
2. run macros, assign labels and variables
3. assemble code, including running Julia exprs
"""
#=
## SYNTAX

- inside asm strings (i.e. macros), \name and \(expr) are macro substitutions
- when a == "q", \(a)_suffix becomes q_suffix
- in ASM syntax, both $N and 0xN are hex numbers but 0x is preferred
- in Julia code, normal Julia syntax holds

comments start with ; and are considered whitespace in the syntax rules

```
PROGRAM: { DEF "\n" }

DEF: (LABEL | "*" | "+" | "-") [INSTRUCTION]
 | INSTRUCTION
 | ".include" JULIA_FILE
 | LABEL ".julia" JULIA_EXPR
 | LABEL ".macro" ["(" ARGLIST ")" "->" JULIA_EXPR]
 | LABEL ".value" JULIA_EXPR
 | LABEL ".fake" JULIA_EXPR
 | (LABEL | "*") "+" JULIA_EXPR

LABEL: IDENT

INSTRUCTION: OPCODE [OPARG]
 | MACROCALL

MACROCALL: "#" IDENT [ARGLIST]

ARGLIST: IDENT { "," IDENT }

OPCODE: ( "ADC" | ... )

OPARG: EXPR
 | "#" EXPR
 | "#" EXPR "," "x"
 | "#" EXPR "," "y"
 | "(" EXPR ")"
 | "(" EXPR "," "x" ")"
 | "(" EXPR ")" "," "y"

JULIA_EXPR: <Julia expression>
```

## How assembly works

The assembler:

1. Includes Julia source and compiles all macros
2. Runs pass 1 to compile all macros and compute label offsets
3. Runs pass 2 to assemble the code by running all the functions in the CodeContext's funs array
4. In Julia code, __CONTEXT__ holds the current CodeContext struct

## Reference

```
label   = EXPR  ; define a label -- labels are Julia consts

.macro  ; declares a Julia-based macro. This should return an Assembly object, which you can make
        ; with asm"..." or by calling asm(string). You can concatenate Assembly objects with `*`.
        ; macro arguments are strings and you can substitute them in with \NAME

joe     .macro (a, b)-> JULIA_EXPR # declares a Julia macro named joe, regular Julia syntax here

        .value JULIA_EXPR ; make JULIA_EXPR the offset value if a label is defined for this macro

fred    .macro(a, b)-> asm"""   ; use asm"..." to define a macro named fred
        STA a
q       STX b
        STY 0x3110
        .value q ;      makes callers of fred get q instead of the first address of the generated code
        """
floop   #macr 1, 2 ; call macro "macr", floop gets the macro's value (defaults to the first line's offset)

fred    .fake(a, b)-> JULIA CODE # declares a Julia-based fake subroutine

        .include file ; load Julia code -- can be called by macros or fake subroutines

 ; special labels
*       ; the "PC", i.e. the current output location
-       ; a temporary label, can be referenced by - backwards and + forwards
        ; Use more than one +/- to reference further, like --, ---, +++, and so on.
+       ; a temporary label
```
=#
module Asm
import ..Fake6502: K, M, G, rhex
using ..Fake6502m: addrsyms, opsyms
using ..AsmTools

const opcodes = Dict((opsyms[i+1], addrsyms[i+1]) => i for i = 0x00:0xFF)
const addrmodebytes = Dict(
    Iterators.flatten(
        Pair.(j, Ref(i)) for (i, j) in enumerate([
            (:acc, :imp),
            (:rel, :imm, :indx, :indy, :zp, :zpx, :zpy),
            (:abso, :absx, :absy, :ind),
        ])
    ),
)

const legal_ops = Set(string.(opsyms))
const branch_ops = Set([:bpl, :bmi, :bvc, :bvs, :bcc, :bcs, :bne, :beq])
const tok_pat =
    r";.*$|[\w.]+|\$[0-9a-fA-F]+|[0-9]+(?:\.[0-9]*)?+|[][(){},\"'#*]|[^\w\d\s(),]+"
const ident_pat = r"^\w+$"
const label_pat = r"^\w+$|^\*$"
const call_pat = r"#\w+"
const bracket_pat = r"[][(){},\"']"
const string_bracket_pat = r"\"|\$\("
const brackets =
    Dict("(" => ")", "[" => "]", "{" => "}", "\"" => "\"", "'" => "'", "\$(" => ")")
const close_brackets = ")]}"
const dot_cmds = Set([".data", ".julia", ".include", "=", ".macro", ".value", ".imm"])
const directives = Set([legal_ops..., dot_cmds...])
const macro_arg_sep = r"(?<!\\),"
const macro_ref_pat = r"\\\w+"
const CTX = :__CONTEXT__

matches(r, s) = !isnothing(match(r, s))

struct CodeChunk
    label_offsets::Dict{String,Int} # offsets for any labels this code defines
    code_func::Function             # code_func(ctx, args...) adds machine code to ctx.memory (64K UInt8 array)
end

struct Line
    number::Int
    line::String
    tokens::Vector{RegexMatch}
end

struct Macro
    argnames::Vector{Symbol}
    func::Function
end

@kwdef mutable struct CodeContext
    env::Module = eval(:(module $(Symbol(String(rand('a':'z', 16))))
    using ..AsmTools
    import ..Asm: @asm_str, @noasm_str
    ctxeval(expr) = eval(expr)
    end))
    macros::Dict{Symbol,Macro} = Dict{Symbol,Macro}()
    funcs::Vector{Function} = Function[]
    labels::Set{Symbol} = Set{Symbol}()
    vars::Set{Symbol} = Set{Symbol}([:(*)])
    lines::AbstractVector{Line} = Line[]
    line::Line = Line(0, "", RegexMatch[])
    toks::Vector{RegexMatch} = RegexMatch[]
    label::Union{Symbol,Nothing} = nothing
    assigned::Set{Symbol} = Set{Symbol}() # variables which have been assigned
    offset::UInt16 = 0 # next address to emit into
    memory::Vector{UInt8} = zeros(UInt8, 64K)         # 64K memory array
    pwd::String = pwd()
    macrovalue::UInt16 = 0
end

CodeContext(ctx::CodeContext) = CodeContext(;
    env = subenv(ctx),
    labels = Set(ctx.labels),
    assigned = Set(ctx.assigned),
    vars = Set(ctx.vars),
    ctx.offset,
    ctx.memory,
    ctx.pwd,
    macrovalue = ctx.offset,
)

function subenv(ctx::CodeContext)
    local subname = Symbol(String(rand('a':'z', 16)))
    local envnames = [n for n in names(ctx.env, all = true)
                                            if !isempty(string(n)) &&
                                                !startswith(string(n), '#') &&
                                                n ∉ (:eval, nameof(ctx.env), :include, CTX, :*) &&
                                                !eval(ctx, :($n isa Module))]
    local imports = :(import ...$(nameof(ctx.env)): x)
    pop!(imports.args[1].args)
    for n in envnames
        push!(imports.args[1].args, Expr(:., n))
    end
    local expr = :(module $subname
                using ....AsmTools
                import ....Asm
                import ....Asm: @asm_str, @noasm_str
                ctxeval(expr) = eval(expr)
                end)
    local mod = eval(ctx, expr)
    importall(mod, ctx.env)
    mod
end

function importall(child::Module, parent::Module)
    local envnames = [n for n in names(parent, all = true)
                          if !isempty(string(n)) &&
                              !startswith(string(n), '#') &&
                              n ∉ (:eval, nameof(parent), :include, CTX, :*, :ctxeval) &&
                              !invokelatest(parent.ctxeval, :($n isa Module))]
    local imports = :(import ...$(nameof(parent)): x)
    pop!(imports.args[1].args)
    for n in envnames
        push!(imports.args[1].args, Expr(:., n))
    end
    invokelatest(child.ctxeval, imports)
end

struct AssemblyCode
    pass1::Function
    pass2::Function
end

AssemblyCode() = AssemblyCode(()->nothing, ()->nothing)

function assemble(ctx::CodeContext, code::AssemblyCode)
    AsmTools.withcontext(ctx) do
        ctx.macrovalue = ctx.offset
        code.pass1(ctx)
        reset(ctx)
        code.pass2(ctx)
    end
end

Base.:(*)(code1::AssemblyCode, code2::AssemblyCode) = AssemblyCode(ctx -> begin
    code1.pass1(ctx)
    code2.pass1(ctx)
end, ctx -> begin
    code1.pass2(ctx)
    code2.pass2(ctx)
end)

function tokenize(n, line)
    tokens = [eachmatch(tok_pat, line)...]
    !isempty(tokens) &&
        startswith(tokens[end].match, ';') &&
        return Line(n, line, @view tokens[1:end-1])
    return Line(n, line, tokens)
end

numtoks(ctx::CodeContext) = length(ctx.toks)

hastoks(ctx::CodeContext) = !isempty(ctx.toks)

function eatline(ctx::CodeContext)
    isempty(ctx.lines) && return
    setlines(ctx, ctx.lines)
    ctx.line
end

function setlines(ctx::CodeContext, lines::AbstractVector{Line})
    setline(ctx, lines[1])
    ctx.lines = @view lines[2:end]
end

function setline(ctx::CodeContext, line::Line)
    ctx.line = line
    ctx.toks = line.tokens
end

eattok(ctx::CodeContext) = ctx.toks = @view ctx.toks[2:end]

tok(ctx::CodeContext) = ctx.toks[1].match

tokstr(ctx::CodeContext) = tokstr(ctx.line.line, ctx.toks[1], ctx.toks[end])

tokstr(str, tok1, tok2) = @view str[tok1.offset:tok2.offset+length(tok2.match)-1]

function lasttoks(ctx::CodeContext, items...)
    length(ctx.toks) < length(items) && return false
    for i = 0:length(items)-1
        lowercase(ctx.toks[end-i].match) != lowercase(items[end-i]) && return false
    end
    return true
end

isdirective(ctx::CodeContext) =
    hastoks(ctx) && (ctx.toks[1].match ∈ directives || matches(call_pat, tok(ctx)))

isop(ctx::CodeContext) = hastoks(ctx) && lowercase(tok(ctx)) ∈ legal_ops

iscall(ctx::CodeContext) =
    hastoks(ctx) &&
        matches(call_pat, tokstr(ctx))

is(ctx::CodeContext, str) = hastoks(ctx) && tok(ctx) == str

isident(ctx::CodeContext) = hastoks(ctx) && matches(ident_pat, tok(ctx))

islabel(ctx::CodeContext) = hastoks(ctx) && matches(label_pat, tok(ctx))

macro asm_str(str)
    local pos = 1
    local buf = []
    for match in eachmatch(macro_ref_pat, str)
        match.offset > pos && push!(buf, str[pos:match.offset-1])
        push!(buf, :(string($(Symbol(match.match[2:end])))))
        pos = match.offset + length(match.match)
    end
    pos <= length(str) && push!(buf, str[pos:end])
    esc(:($(asm)(join([$(buf...)]))))
end

macro noasm_str(str)
    :($AssemblyCode())
end

function asmfile(file)
    local ctx = CodeContext(; pwd = dirname(file))
    local assembly = asm(read(file, String))
    assemble(ctx, assembly)
    return ctx
end

"""
    produce a CodeChunk based on str
"""
function asm(str)
    local lines = [tokenize(n, line) for (n, line) in enumerate(split(str, "\n"))]
    AssemblyCode(
        function (ctx)
            println("ASSEMBLING, OFFSET: $(ctx.offset)")
            AsmTools.withcontext(ctx) do
                setvar(ctx, CTX, ctx)
                # pass 1
                #println("@@@\n@@@PASS 1\n###")
                remaining = compile_macros_pass1(ctx, lines)
                invokelatest() do
                    scan_asm_pass2(ctx, remaining)
                end
            end
            ctx
        end,
        function (ctx)
            # pass 2
            #println("@@@\n@@@PASS 2\n###")
            invokelatest() do
                AsmTools.withcontext(ctx) do
                    for func in ctx.funcs
                        func()
                    end
                end
            end
        end,
    )
end

getvar(ctx::CodeContext, var::Symbol) = ctx.env.eval(var)

eval(ctx::CodeContext, expr) = invokelatest(ctx.env.ctxeval, expr)

function setvar(ctx::CodeContext, var::Symbol, value)
    var == :(*) && return
    local argname = Symbol("_" * string(var))
    invokelatest(eval(ctx, :(function ($argname)
        global $var = $argname
    end)), value)
end

"""
Scan asm code, find label offsets and construct gen function
"""
function scan_asm_pass2(ctx::CodeContext, lines)
    ctx.lines = lines
    while !isempty(ctx.lines)
        local line = eatline(ctx)
        ctx.toks = line.tokens
        ctx.label = nothing
        !hastoks(ctx) && continue
        if !isdirective(ctx) && islabel(ctx)
            # a labeled statement -- statement decides whether this is a label or a variable
            ctx.label = Symbol(tok(ctx))
            eattok(ctx)
            # set the label to the current offset
            ctx.label != :* && !iscall(ctx) && !is(ctx, ".imm") && deflabel(ctx)
            !hastoks(ctx) && continue
        end
        asm_assign(ctx) && continue
        !isnothing(ctx.label) &&
            ctx.label ∈ ctx.labels &&
            lineerror(ctx, """Attempt to reassign label $(ctx.label)""")
        !isnothing(ctx.label) && push!(ctx.labels, ctx.label)
        for dir in [asm_op, asm_data, asm_julia, asm_include, asm_call, asm_value, asm_imm]
            dir(ctx) && @goto bottom
        end
        !isdirective(ctx) && lineerror(line, """Unknown directive, $(tokstr(ctx))""")
        lineerror(line, """Unimplemented directive: $(tok(ctx))""")
        @label bottom
    end
end

function deflabel(ctx, value = ctx.offset)
    println("@@ LABEL $(ctx.label): $value")
    eval(ctx, :(const $(ctx.label) = $value))
end

assembleif(func::Function, ctx::CodeContext, pred::AbstractString) =
    assembleif(func, ctx, ctx -> is(ctx, pred))

function assembleif(func, ctx, pred)
    !pred(ctx) && return false
    func()
    return true
end

asm_data(ctx::CodeContext) =
    assembleif(ctx, ".data") do
        eattok(ctx)
        println("DATA: $(tokstr(ctx))")
        local expr = Meta.parse(tokstr(ctx))
        expr isa Expr &&
            expr.head == :incomplete &&
            lineerror(ctx, """Incomplete expression: $(tokstr(ctx))""")
        local bytes = AsmTools.data(eval(ctx, expr))
        push!(ctx.funcs, () -> emitall(ctx, bytes))
        ctx.offset += length(bytes)
    end

asm_include(ctx::CodeContext) =
    assembleif(ctx, ".include") do
        eattok(ctx)
        try
            local expr = Meta.parse(tokstr(ctx))
            if expr isa Expr && expr.head == :incomplete
                error()
            end
            eval(ctx, :(include(joinpath($(ctx.pwd), $expr))))
        catch
            lineerror(ctx, """Error running .include directive: $(tokstr(ctx))""")
        end
    end

asm_julia(ctx::CodeContext) =
    assembleif(ctx, ".julia") do
        eattok(ctx)
        local expr = compile_julia(ctx)
        push!(ctx.funcs, () -> eval(ctx, expr))
    end

asm_imm(ctx::CodeContext) =
    assembleif(ctx, ".imm") do
        eattok(ctx)
        local expr = compile_julia(ctx)
        eval(ctx, expr)
        !isnothing(ctx.label) &&
            deflabel(ctx)
    end

asm_value(ctx::CodeContext) =
    assembleif(ctx, ".value") do
        eattok(ctx)
        !isnothing(ctx.label) && deflabel(ctx)
        local expr = compile_julia(ctx)
        try
            ctx.macrovalue = eval(ctx, expr)
            println("SET MACRO VALUE TO $(ctx.macrovalue)")
        catch
            lineerror(ctx, """Error calculating macro value""")
        end
    end

function compile_julia(ctx::CodeContext)
    local firstlines = ctx.lines
    local programtext = ctx.line.line[ctx.toks[1].offset:end]
    while true
        try
            local expr = Meta.parse(programtext)
            if expr isa Expr && expr.head == :incomplete
                local line = eatline(ctx)
                isnothing(line) && error()
                programtext *= '\n' * line.line
                continue
            end
            return expr
        catch
            setlines(ctx, firstlines)
            lineerror(ctx, """Error parsing Julia code:\n$programtext""")
            rethrow()
        end
    end
end

function assign_var(ctx::CodeContext, exprstr, wrapper = identity)
    local var = ctx.label
    local progress = "compiling expression"
    try
        local expr = Meta.parse(exprstr)
        expr isa Expr && expr.head == :incomplete && error()
        push!(ctx.assigned, var)
        println("VAR: $(repr(var)) $(var == :(*)) <- $exprstr")
        local func =
            var == :* ? wrapper(() -> begin
                ctx.offset = ctx.env.eval(expr)
                println("SET OFFSET TO $(rhex(ctx.offset))")
            end) : wrapper(() -> invokelatest(ctx.env.eval(:(function ()
                global $var = $expr
            end))))
        # push the func for pass 3
        push!(ctx.funcs, func)
        # execute the func for the rest of this pass
        progress = "executing expression"
        var == :* && func()
    catch
        lineerror(ctx, """Error $progress, $exprstr""")
    end
end

asm_call(ctx::CodeContext) =
    assembleif(ctx, iscall) do
        eattok(ctx)
        local macname = Symbol(tok(ctx))
        println("ASSEMBLING CALL $macname")
        eattok(ctx)
        !haskey(ctx.macros, macname) && lineerror(ctx, """Reference to unknown macro""")
        local mac = ctx.macros[macname]
        local args = macroargs(tokstr(ctx))
        length(args) != length(mac.argnames) && lineerror(
            ctx,
            """Wrong number of macro arguments to $macname, expecting $(length(mac.argnames)) but got $(length(args))""",
        )
        try
            println("ASSEMBLING $macname CALL")
            local tmpctx = CodeContext(ctx)
            local assembly = mac.func(args...)
            println("CALL MACRO, TMP OFFSET: $(tmpctx.offset)")
            assembly.pass1(tmpctx)
            tmpctx.offset = ctx.offset
            !isnothing(ctx.label) && deflabel(ctx, tmpctx.macrovalue)
            println("PUSHING $macname's FUNC")
            push!(ctx.funcs, function ()
                #local tmpctx = CodeContext(ctx)
                importall(tmpctx.env, ctx.env)
                println("CALLING $macname's FUNC")
                assembly.pass2(tmpctx)
                ctx.offset = tmpctx.offset
            end)
        catch err
            @error "$err" exception = (err, catch_backtrace())
            lineerror(ctx, """Error calling macro""")
        end
    end

"""
Macro args are like C macro args -- just simple splitting on commas, backslash escapes a comma
Juila evaluation on args doesn't happen until they are substituted into an ASM string
"""
macroargs(str) = [replace(s, "\\," => ",") for s in strip.(split(str, macro_arg_sep))]

asm_assign(ctx::CodeContext) =
    assembleif(ctx, "=") do
        ctx.label ∈ ctx.labels && lineerror(ctx, """Attempt to assign a label""")
        numtoks(ctx) == 1 && lineerror(ctx, """Assignment needs an expression""")
        eattok(ctx)
        local exprstr = tokstr(ctx)
        assign_var(ctx, exprstr)
        push!(ctx.vars, ctx.label)
    end

function reset(ctx::CodeContext)
    ctx.offset = 0
    setdiff!(ctx.assigned, ctx.vars)
    invokelatest(ctx.env.ctxeval, quote
        $((:($var = nothing) for var in ctx.vars)...)
    end)
end

function emit(ctx::CodeContext, bytes::UInt8...)
    local start = ctx.offset
    for byte in bytes
        ctx.memory[ctx.offset+1] = byte
        ctx.offset += 1
    end
    println("$(rhex(start)): $(join([rhex(byte) for byte in bytes], " "))")
end

function emitall(ctx::CodeContext, bytes::Vector{UInt8})
    println("EMIT $(rhex(ctx.offset)): $(length(bytes)) byte$(length(bytes) != 1 ? "s" : "")")
    copy!(@view(ctx.memory[ctx.offset+1:length(bytes)+ctx.offset]), bytes)
    ctx.offset += length(bytes)
end

function assemble(ctx::CodeContext, op, addr, exprs = :())
    local opcode = opcodes[(op, addr)]
    local bytes = addrmodebytes[addr]
    local args = eval(ctx, exprs)
    push!(
        ctx.funcs,
        () -> println(
            "assemble $(rhex(ctx.offset)) ($op $addr)$(isempty(args) ? "" : " " * exprs) ->  $(join([rhex(opcode), args...], " "))",
        ),
    )
    if bytes == 1
        push!(ctx.funcs, () -> emit(ctx, opcode))
    elseif bytes == 2
        local a1 = args[1]
        push!(ctx.funcs, () -> emit(ctx, opcode, a1))
    elseif bytes == 3
        local a1 = args[1]
        local a2 = args[2]
        push!(ctx.funcs, () -> emit(ctx, opcode, a1, a2))
    end
    ctx.offset += bytes
    true
end

asm_op(ctx::CodeContext) =
    assembleif(ctx, isop) do
        local op = Symbol(lowercase(tok(ctx)))
        local opname = uppercase(string(op))
        local modes = keys(opcodes)
        local args = strip(tokstr(ctx))
        if !isnothing(ctx.label)
            ctx.label ∈ ctx.assigned && lineerror(ctx, """Redeclaring label $(ctx.label)""")
            push!(ctx.labels, ctx.label)
            push!(ctx.assigned, ctx.label)
        end
        eattok(ctx)
        !hastoks(ctx) && (op, :imp) ∈ modes && return assemble(ctx, op, :imp)
        !hastoks(ctx) && (op, :acc) ∈ modes && return assemble(ctx, op, :acc)
        !hastoks(ctx) && lineerror(ctx, """No arguments for opcode $opname""")
        op ∈ branch_ops && return assemble(ctx, op, :rel, args)
        is(ctx, "#") && return assemble(ctx, op, :imm, args)
        is(ctx, "(") &&
            lasttoks(ctx, ")", ",", "y") &&
            return assemble(ctx, op, :indy, args)
        is(ctx, "(") &&
            lasttoks(ctx, ",", "x", ")") &&
            return assemble(ctx, op, :indx, args)
        #check for zpx as opposed to absx -- skip for now and assemble ,x as absolute,x
        lasttoks(ctx, ",", "y") && return assemble(ctx, op, :absy, args)
        lasttoks(ctx, ",", "x") && return assemble(ctx, op, :absx, args)
        is(ctx, "(") &&
            lasttoks(ctx, ")") &&
            op == :jmp &&
            return assemble(ctx, op, :ind, args)
        is(ctx, "(") &&
            lasttoks(ctx, ")") &&
            lineerror(ctx, """Only JMP can use indirect addressing but this is $opname""")
        is(ctx, "(") && lineerror(ctx, """Incomplete indirect expression, $(tokstr(ctx))""")
        # must be absolute
        assemble(ctx, op, :abso, args)
    end

"""
Extract macros from assembly code, returning the compiled macros and the rest of the code
"""
function compile_macros_pass1(ctx::CodeContext, lines)
    local l = 1
    local remain = Line[]
    while l <= length(lines)
        local line = lines[l]
        if ismacrodecl(line)
            setlines(ctx, @view lines[l:end])
            compilemacro(ctx)
            lines = ctx.lines
            l = 1
        else
            push!(remain, line)
            l += 1
        end
    end
    return remain
end

ismacrodecl(line::Line) = !isnothing(findfirst(m -> m.match == ".macro", line.tokens))

isfakedecl(line::Line) = !isnothing(findfirst(m -> m.match == ".fake", line.tokens))

lineerror(ctx::CodeContext, msg) = lineerror(ctx.line, msg)

lineerror(line::Line, msg) = error("Error on line $(line.number), $msg: $(line.line)")

function compilemacro(ctx::CodeContext)
    local firstline = ctx.line
    local label = Symbol(ctx.toks[1].match)
    string(label) == ".macro" && lineerror(ctx, """Attempt to define a nameless macro""")
    label ∈ keys(ctx.macros) && lineerror(ctx, """Attempt to redefine macro $label""")
    eattok(ctx)
    eattok(ctx)
    local expr = compile_julia(ctx)
    expr.head != :-> &&
        lineerror(firstline, """Expected an arrow function ( (args)-> ... )""")
    println("MACRO $label: $expr")
    local macargs = expr.args[1]
    ctx.macros[label] =
        Macro(macargs isa Symbol ? [macargs] : [expr.args[1].args...], eval(ctx, expr))
end

test() = asmfile(joinpath(dirname(dirname(@__FILE__)), "examples", "simple.jas"))

end # module Asm
