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
 | ".include" JAS_FILE
 | LABEL ".data" JULIA_EXPR
 | LABEL ".imm" JULIA_EXPR
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
-- can include rel"+++---" and label"LABEL"
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
using Printf, Dates
using ...Fake6502: K, M, G, hex, rhex, register, asmerr, asmwarn, matches, log
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
    r";.*$|[\w.]+|\$[0-9a-fA-F]+|[0-9]+(?:\.[0-9]*)?+|[][(){},\"'#*+\-]|[^\w\d\s(),]+"
const ident_pat = r"^\w+$"
const label_pat = r"^\s*\w+$|^\s*[-+*]$"
const call_pat = r"#\w+"
const bracket_pat = r"[][(){},\"']"
const string_bracket_pat = r"\"|\$\("
const brackets =
    Dict("(" => ")", "[" => "]", "{" => "}", "\"" => "\"", "'" => "'", "\$(" => ")")
const close_brackets = ")]}"
const dot_cmds =
    Set([".data", ".julia", ".include", "=", ".macro", ".value", ".imm", ".fake"])
const directives = Set([legal_ops..., dot_cmds...])
const macro_arg_sep = r"(?<!\\),"
const macro_ref_pat = r"\\\w+"
const CTX = :__CONTEXT__
const OptSym = Union{Symbol,Nothing}

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

struct ListingLine
    type::Symbol
    addr::UInt16
    label::OptSym
    bytes::Int
    line::Line
    extra_lines::Vector{Line}
end

@kwdef mutable struct CodeContext
    env::Module = eval(:(module $(Symbol(String(rand('a':'z', 16))))
    using ...Fake6502: Fake6502, C64, Asm, AsmTools
    using ..AsmTools
    import ..Asm: @asm_str, @noasm_str
    import ...Fake6502: rhex
    end))
    macros::Dict{Symbol,Macro} = Dict{Symbol,Macro}()
    pass2_funcs::Vector{Function} = Function[]
    pass3_funcs::Vector{Function} = Function[]
    pass4_funcs::Vector{Function} = Function[]
    pass3_stability_funcs::Vector{Function} = Function[]
    labels::Set{Symbol} = Set{Symbol}()
    relative_labels::Vector{UInt16} = UInt16[]
    vars::Set{Symbol} = Set{Symbol}([:(*)])
    lines::AbstractVector{Line} = Line[]
    line::Line = Line(0, "", RegexMatch[])
    toks::Vector{RegexMatch} = RegexMatch[]
    label::OptSym = nothing
    assigned::Set{Symbol} = Set{Symbol}() # variables which have been assigned
    offset::UInt16 = 0 # next address to emit into
    memory::Vector{UInt8} = zeros(UInt8, 64K)         # 64K memory array
    pwd::String = pwd()
    macrovalue::UInt16 = 0
    macrolabel::OptSym = nothing
    fakes::Dict{Symbol,Tuple{Int,Function}} = Dict{Symbol,Tuple{Int,Function}}()
    pass::Int = 0
    min::Int = 64K
    max::Int = 0
    listing::Vector{ListingLine} = ListingLine[]
    verbose::Bool = false
    unstable::Vector = []
    stability::Int = 0
end

Base.show(io::IO, ::CodeContext) = print(io, "CodeContext")

@kwdef struct ListingContext
    ctx::CodeContext
    lines::AbstractVector{Line} = Line[]
    line::Line = Line(0, "", RegexMatch[])
    toks::Vector{RegexMatch} = RegexMatch[]
    label::OptSym = nothing
    listing::Vector{ListingLine} = ListingLine[]
end

ListingContext(ctx::CodeContext) =
    ListingContext(; ctx, ctx.lines, ctx.line, ctx.toks, ctx.label, ctx.listing)

subcontext(ctx::CodeContext) = CodeContext(;
    env = subenv(ctx),
    labels = Set(ctx.labels),
    assigned = Set(ctx.assigned),
    vars = Set(ctx.vars),
    ctx.offset,
    ctx.memory,
    ctx.pwd,
    macrovalue = ctx.offset,
    ctx.listing,
    ctx.label,
    ctx.line,
)

function subenv(ctx::CodeContext)
    local subname = Symbol(String(rand('a':'z', 16)))
    local envnames = [
        n for n in names(ctx.env, all = true) if !isempty(string(n)) &&
        !startswith(string(n), '#') &&
        n ∉ (:eval, nameof(ctx.env), :include, CTX, :*) &&
        !eval(ctx, :($n isa Module))
    ]
    local expr = :(module $subname
    using  .....Fake6502: Fake6502, C64, Asm, AsmTools
    using  ....AsmTools
    import .....Fake6502: rhex
    import ....Asm: @asm_str, @noasm_str
    end)
    local mod = eval(ctx, expr)
    importall(mod, ctx.env)
    mod
end

function importall(child::Module, parent::Module)
    local envnames = [
        n for n in names(parent, all = true) if !isempty(string(n)) &&
        !startswith(string(n), '#') &&
        n ∉ (:eval, nameof(parent), :include, CTX, :*) &&
        !Core.eval(parent, :($n isa Module))
    ]
    local imports = :(import ...$(nameof(parent)): x)
    pop!(imports.args[1].args)
    for n in envnames
        push!(imports.args[1].args, Expr(:., n))
    end
    Core.eval(child, imports)
end

struct AssemblyCode
    # scan the file
    pass1::Function
    # calculate labels
    pass2::Function
    # populate data
    pass3::Function
    # repopulate data in case of instability
    pass3_stability::Function
    # assemble the ops
    pass4::Function
end

AssemblyCode() = AssemblyCode(() -> nothing, () -> nothing, () -> nothing, () -> nothing)

function pushfunc!(func, ctx, passes...)
    for pass in passes
        local line = ctx.line
        local lines = ctx.lines
        local toks = ctx.toks
        local pass_funcs =
            pass == :stability ? ctx.pass3_stability_funcs :
            [ctx.pass2_funcs, ctx.pass3_funcs, ctx.pass4_funcs][pass-1]

        push!(pass_funcs, () -> begin
            ctx.line = line
            ctx.lines = lines
            ctx.toks = toks
            try
                func()
            catch err
                lineerror(line, err, err)
                rethrow()
            end
        end)
    end
end

function incoffset(ctx::CodeContext, off; track = false)
    local oldmin = ctx.min
    local oldmax = ctx.max
    if track
        ctx.min = min(ctx.min, ctx.offset)
    end
    ctx.offset += off
    if track
        ctx.max = max(ctx.max, ctx.offset - 1)
    end
    if ctx.verbose && (oldmin != ctx.min || oldmax != ctx.max)
        asmwarn(
            ctx,
            "Range length: $(ctx.max - ctx.min + 1), changed" *
            (oldmin != ctx.min ? " min $(rhex(oldmin)) -> $(rhex(ctx.min))" : "") *
            (oldmax != ctx.max ? " max $(rhex(oldmax)) -> $(rhex(ctx.max))" : ""),
        )
    end
end

function setoffset(ctx::CodeContext, off)
    ctx.offset = off
end

function assemble(ctx::CodeContext, code::AssemblyCode)
    AsmTools.withcontext(ctx) do
        ctx.macrovalue = ctx.offset
        println("\n@@@\n@@@PASS 1: SCAN CODE\n@@@")
        ctx.pass = 1
        code.pass1(ctx)
        reset(ctx)
        eval(ctx, quote
            $((:($var = 0x0000) for var in ctx.labels)...)
        end)
        # save listing in case of instability
        local listing = [ctx.listing...]
        println("\n@@@\n@@@PASS 2: CALCULATE LABELS\n@@@")
        empty!(ctx.relative_labels)
        ctx.pass = 2
        code.pass2(ctx)
        println("\n@@@\n@@@PASS 3: POPULATE DATA\n@@@")
        reset(ctx)
        ctx.pass = 3
        code.pass3(ctx)
        if !isempty(ctx.unstable)
            println("RE-EXECUTING PASS 3 BECCAUSE OF UNSTABLE CODE:")
            println("\n  $(join((sprint(io->print_listing(io, ctx, line))
                                     for line in ctx.unstable), "\n  "))")
            # execute stability pass twice, once to update counts, once to generate code
            for stability = 1:2
                stability == 1 && empty!(ctx.relative_labels)
                println("\n@@@\n@@@PASS 3 STABILITY PASS $stability: POPULATE DATA\n@@@")
                # restore listing and clear out unstable
                empty!(ctx.listing)
                append!(ctx.listing, listing)
                empty!(ctx.unstable)
                reset(ctx)
                ctx.pass = 3
                ctx.stability = stability
                code.pass3_stability(ctx)
            end
            !isempty(ctx.unstable) &&
                error("unstable code:\n  $(join((sprint(io->print_listing(io, ctx, line))
                                                for line in ctx.unstable), "\n  "))")
        end
        println("\n@@@\n@@@PASS 4: ASSEMBLE OPERATIONS\n@@@")
        reset(ctx)
        ctx.pass = 4
        code.pass4(ctx)
    end
end

Base.:(*)(code1::AssemblyCode, code2::AssemblyCode) = AssemblyCode(
    ctx -> begin
        code1.pass1(ctx)
        code2.pass1(ctx)
    end,
    ctx -> begin
        code1.pass2(ctx)
        code2.pass2(ctx)
    end,
    ctx -> begin
        code1.pass3(ctx)
        code2.pass3(ctx)
    end,
    ctx -> begin
        code1.pass3_stability(ctx)
        code2.pass3_stability(ctx)
    end,
    ctx -> begin
        code1.pass4(ctx)
        code2.pass4(ctx)
    end,
)

function tokenize(n, line)
    tokens = [eachmatch(tok_pat, line)...]
    !isempty(tokens) &&
        startswith(tokens[end].match, ';') &&
        return Line(n, line, @view tokens[1:end-1])
    return Line(n, line, tokens)
end

numtoks(ctx::CodeContext) = length(ctx.toks)

hastoks(ctx::CodeContext) = !isempty(ctx.toks)

hastoks(line::Line) = !isempty(line.tokens)

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

tok(line::Line) = line.tokens[1].match

tokstr(ctx::CodeContext) = tokstr(ctx, ctx.toks)

tokstr(ctx::CodeContext, toks::RegexMatch...) = tokstr(ctx.line.line, toks...)

tokstr(line::Line) = tokstr(line.line, line.tokens)

tokstr(line::Line, toks::RegexMatch...) = tokstr(line.line, toks...)

tokstr(line::ListingLine, toks::RegexMatch...) = tokstr(line.line.line, toks...)

tokstr(thing, toks::Vector{RegexMatch}) = tokstr(thing, toks[1], toks[end])

tokstr(str::AbstractString, tok1, tok2) =
    @view str[tok1.offset:tok2.offset+length(tok2.match)-1]

function lasttoks(ctx::CodeContext, items...)
    length(ctx.toks) < length(items) && return false
    for i = 0:length(items)-1
        lowercase(ctx.toks[end-i].match) != lowercase(items[end-i]) && return false
    end
    return true
end

function isincomplete(body::String)
    try
        body != "" && isincomplete(Meta.parse(body))
    catch err
        true
    end
end

isincomplete(expr) = expr isa Expr && expr.head == :incomplete

const Toks = Union{CodeContext,Line}

isdirective(toks::Toks) = is(toks, ∈(directives), call_pat)

tok_is(thing) = (toks) -> is(toks, thing)

is(toks::Toks) = (thing) -> is(toks, thing)

is(toks::Toks, things...) = any(is(toks).(things))

is(toks::Toks, str::AbstractString) = hastoks(toks) && lowercase(tok(toks)) == str

is(toks::Toks, pred::Function) = hastoks(toks) && pred(lowercase(tok(toks)))

is(toks::Toks, pat::Regex) = hastoks(toks) && matches(pat, lowercase(tok(toks)))

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

function asmfile(
    file;
    list::Bool = false,
    output = replace(file, r".jas$" => ""),
    verbose = false,
)
    local ctx = CodeContext(; pwd = dirname(file), verbose)
    local assembly = asm(read(file, String))

    assemble(ctx, assembly)
    list && listings(ctx, "$output.list")
    if !isnothing(output)
        write("$output.prg", prgbytes(ctx))
    end
    return ctx
end

function prgbytes(ctx::CodeContext)
    local len = ctx.max - ctx.min + 1
    local bytes = zeros(UInt8, len + 2)

    bytes[1] = UInt8(ctx.min & 0xFF)
    bytes[2] = UInt8(ctx.min >> 8 & 0xFF)
    bytes[3:end] .= @views ctx.memory[ctx.min+1:ctx.max+1]
    bytes
end

"""
    produce a CodeChunk based on str
"""
function asm(str)
    local lines = [tokenize(n, line) for (n, line) in enumerate(split(str, "\n"))]

    AssemblyCode(
        function (ctx::CodeContext)
            println("ASSEMBLING, OFFSET: $(ctx.offset)")
            AsmTools.withcontext(ctx) do
                setvar(ctx, CTX, ctx)
                # pass 1
                #println("@@@\n@@@PASS 1\n###")
                local remaining = compile_macros_pass1(ctx, lines)
                invokelatest() do
                    scan_asm(ctx, remaining)
                end
            end
            ctx
        end,
        function (ctx::CodeContext)
            # pass 2
            #println("@@@\n@@@PASS 2\n###")
            invokelatest() do
                AsmTools.withcontext(ctx) do
                    for func in ctx.pass2_funcs
                        func()
                    end
                end
            end
        end,
        function (ctx::CodeContext)
            # pass 3
            #println("@@@\n@@@PASS 3\n###")
            invokelatest() do
                AsmTools.withcontext(ctx) do
                    for func in ctx.pass3_funcs
                        func()
                    end
                end
            end
        end,
        function (ctx::CodeContext)
            # stability pass
            #println("@@@\n@@@PASS 3, stability\n###")
            invokelatest() do
                AsmTools.withcontext(ctx) do
                    for func in ctx.pass3_stability_funcs
                        func()
                    end
                end
            end
        end,
        function (ctx::CodeContext)
            # pass 4
            #println("@@@\n@@@PASS 3\n###")
            invokelatest() do
                AsmTools.withcontext(ctx) do
                    for func in ctx.pass4_funcs
                        func()
                    end
                end
            end
        end,
    )
end

eval(ctx::CodeContext, expr) = Core.eval(ctx.env, expr)

getvar(ctx::CodeContext, var::Symbol) = eval(ctx, var)

function setvar(ctx::CodeContext, var::Symbol, value)
    var == :* && return
    local argname = Symbol("_" * string(var))
    invokelatest(eval(ctx, :(function ($argname,)
        global $var = $argname
    end)), value)
end

"""
Scan asm code, find label offsets and construct gen function
"""
function scan_asm(ctx::CodeContext, lines)
    ctx.lines = lines
    while !isempty(ctx.lines)
        local line = eatline(ctx)
        ctx.toks = line.tokens
        ctx.label = nothing
        !hastoks(ctx) && continue
        if !isdirective(ctx) && is(ctx, label_pat)
            # a labeled statement -- statement decides whether this is a label or a variable
            ctx.label = Symbol(tok(ctx))
            eattok(ctx)
            println("@@@ GOT LABEL $(ctx.label)")
            # set the label to the current offset for operations except .imm and .fake
            !(ctx.label == :* || is(ctx, ".imm", ".fake", call_pat)) && deflabel(ctx)
            if !hastoks(ctx)
                if ctx.label != :*
                    add_listing(ctx, :def)
                end
                continue
            end
        end
        asm_assign(ctx) && continue
        !isnothing(ctx.label) &&
            ctx.label ∈ ctx.labels &&
            lineerror(ctx, """Attempt to reassign label $(ctx.label)""")
        !isnothing(ctx.label) && push!(ctx.labels, ctx.label)
        for dir in [asm_op, asm_data, asm_julia, asm_call, asm_value, asm_imm, asm_fake]
            dir(ctx) && @goto bottom
        end
        !isdirective(ctx) && lineerror(line, """Unknown directive, $(tokstr(ctx))""")
        lineerror(line, """Unimplemented directive: $(tok(ctx))""")
        @label bottom
    end
    sort!(ctx.relative_labels)
end

# called on pass 1
# defines the label as 0
# adds a pass 2 code block to assign the real value
# usable on pass 3 or in pass 2 for previously defined labels
function deflabel(ctx::CodeContext, value = nothing; label = ctx.label)
    isnothing(label) && return
    println("DEF LABEL: $(label)")
    local wasnothing = isnothing(value)
    if wasnothing
        # recalculate this on pass 2
        value = ctx.offset
    end
    if label ∈ [:-, :+] && (ctx.pass == 2 || ctx.stability == 1)
        println("@@ RELATIVE LABEL: $(hex(value))")
        push!(ctx.relative_labels, ctx.offset)
    else
        println("@@ LABEL $(label): $(hex(value))")
        eval(ctx, :($(label) = $value))
    end
    ctx.pass == 1 && pushfunc!(ctx, 2, :stability) do
        deflabel(ctx, wasnothing ? nothing : value; label)
    end
    pushfunc!(ctx, 4) do
        println("LABEL $label: $(hex(value))")
    end
end

assembleif(func::Function, ctx, pred::AbstractString) =
    assembleif(func, ctx, ctx -> is(ctx, pred))

assembleif(func::Function, ctx, pred::Bool) = pred && (func(); true)

assembleif(func::Function, ctx, pred) = pred(ctx) && (func(); true)

function asm_data(ctx::CodeContext)
    local lctx = ListingContext(ctx)

    assembleif(ctx, is(ctx, ".data")) do
        eattok(ctx)
        println("DATA: $(tokstr(ctx))")
        local expr = Meta.parse(tokstr(ctx))
        isincomplete(expr) &&
            lineerror(ctx, """Incomplete expression: $(tokstr(ctx))\n  EXPR: '$expr'""")
        # fake_bytes is just to calculate size for label offsets
        local data_len = 0
        pushfunc!(ctx, 2, 4) do
            data_len = length(AsmTools.data(eval(ctx, expr)))
            incoffset(ctx, data_len; track = true)
        end
        pushfunc!(ctx, 3, :stability) do
            #local bytes = AsmTools.data(eval(ctx, expr))
            println("@@@ DATA EXPR: $expr")
            local value = eval(ctx, expr)
            local bytes = AsmTools.data(value)
            println(
                "@@@\n@@@ DATA VALUE PASS $(ctx.stability == 0 ? 3 : ctx.stability == 1 ? "3 STABILITY PASS 1" : "3 STABILITY PASS 2"): $value\n@@@",
            )
            add_listing(lctx, :data, length(bytes))
            if ctx.stability == 1
                # update data_len on stability pass #1
                data_len = length(bytes)
            elseif data_len != length(bytes)
                push!(ctx.unstable, ctx.listing[end])
            end
            println("EMITTING $(length(bytes)) DATA BYTES")
            emitall(ctx, bytes)
        end
    end
end

asm_julia(ctx::CodeContext) =
    assembleif(ctx, ".julia") do
        eattok(ctx)
        local expr, extra = parse_julia(ctx)
        add_listing(ctx, :julia, extra)
        pushfunc!(ctx, 4) do
            eval(ctx, expr)
        end
    end

asm_imm(ctx::CodeContext) =
    assembleif(ctx, ".imm") do
        eattok(ctx)
        local expr, extra = parse_julia(ctx)
        eval(ctx, expr)
        deflabel(ctx)
        add_listing(ctx, :imm, extra)
    end

asm_fake(ctx::CodeContext) =
    assembleif(ctx, ".fake") do
        eattok(ctx)
        local expr, extra = parse_julia(ctx)
        expr.head ∈ (:function, :->) && length(expr.args[1].args) != 2 &&
            lineerror(ctx, """.fake expects a 2-argument function""")
        isnothing(ctx.label) && lineerror(ctx, """.fake with no label""")
        local index = length(ctx.fakes)
        local label = ctx.label
        deflabel(ctx, index)
        add_listing(ctx, :fake, extra)
        ctx.fakes[ctx.label] = (index, (mach) -> nothing)
        pushfunc!(ctx, 4) do
            ctx.fakes[label] = (index, eval(ctx, expr))
            #ctx.min = min(ctx.min, index)
        end
    end

asm_value(ctx::CodeContext) =
    assembleif(ctx, ".value") do
        eattok(ctx)
        deflabel(ctx)
        local expr, extra = parse_julia(ctx)
        isnothing(ctx.macrolabel) && return
        try
            ctx.macrovalue = eval(ctx, expr)
            if !isnothing(ctx.macrolabel)
                add_listing(ctx, :value, ctx.macrovalue, ctx.macrolabel, 0, ctx.line)
            end
            println("SET MACRO VALUE TO $(ctx.macrovalue)")
        catch err
            @error "Error assembling .value" exception = (err, catch_backtrace())
            lineerror(ctx, """Error calculating macro value""")
        end
    end

function parse_julia(ctx::CodeContext)
    local firstlines = ctx.lines
    local extra = Line[]
    local programtext = ctx.line.line[ctx.toks[1].offset:end]
    while true
        try
            local expr = Meta.parse(programtext)
            if isincomplete(expr)
                local line = eatline(ctx)
                push!(extra, line)
                isnothing(line) && error()
                programtext *= '\n' * line.line
                continue
            end
            return expr, extra
        catch err
            @error "Error parsing julia" exception = (err, catch_backtrace())
            setlines(ctx, firstlines)
            lineerror(ctx, """Error parsing Julia code:\n$programtext""")
            rethrow()
        end
    end
end

function assign_var(lctx::ListingContext, exprstr)
    local progress = "compiling expression"
    local var = lctx.label
    local line = lctx.line
    try
        local expr = Meta.parse(exprstr)
        isincomplete(expr) && error()
        push!(lctx.ctx.assigned, var)
        println("VAR: $(repr(var)) $(var ∈ [:*, :-, :+]) <- $exprstr")
        if var == :*
            setoffset(lctx.ctx, lctx.ctx.env.eval(expr))
            println("SET OFFSET TO $(rhex(lctx.ctx.offset))")
        else
            lctx.ctx.pass == 4 && add_listing(lctx, :assign, var, 0, line)
            invokelatest(lctx.ctx.env.eval(:(function ()
                global $var = $expr
            end)))
        end
    catch err
        lineerror(lctx, """Error $progress, $exprstr""", err)
        @error "ERROR ASSIGNING VARIABLE" exception = (err, catch_backtrace())
    end
end

asm_call(ctx::CodeContext) =
    assembleif(ctx, tok_is(call_pat)) do
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
            local lctx = ListingContext(ctx)
            local tmpctx = subcontext(ctx)
            local assembly = mac.func(args...)
            tmpctx.macrolabel = ctx.label
            println("CALL MACRO, TMP OFFSET: $(tmpctx.offset)")
            tmpctx.pass = 1
            assembly.pass1(tmpctx)
            importall(tmpctx.env, ctx.env)
            setoffset(ctx, tmpctx.offset)
            tmpctx.offset = ctx.offset
            deflabel(ctx, tmpctx.macrovalue)
            println("PUSHING $macname's FUNC")
            pushfunc!(ctx, 2) do
                tmpctx.pass = 2
                setoffset(tmpctx, ctx.offset)
                assembly.pass2(tmpctx)
                setoffset(ctx, tmpctx.offset)
                importall(tmpctx.env, ctx.env)
            end
            pushfunc!(ctx, 3, :stability) do
                if tmpctx.macrovalue != 0
                    add_listing(lctx, :call; remove_label = true)
                else
                    add_listing(lctx, :call)
                end
                importall(tmpctx.env, ctx.env)
                println("CALLING $macname's FUNC")
                tmpctx.pass = 3
                setoffset(tmpctx, ctx.offset)
                assembly.pass3(tmpctx)
                setoffset(ctx, tmpctx.offset)
                importall(tmpctx.env, ctx.env)
            end
            pushfunc!(ctx, 4) do
                tmpctx.pass = 4
                setoffset(tmpctx, ctx.offset)
                assembly.pass4(tmpctx)
                setoffset(ctx, tmpctx.offset)
                importall(tmpctx.env, ctx.env)
            end
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
        local lctx = ListingContext(ctx)

        ctx.label ∈ ctx.labels && lineerror(ctx, """Attempt to assign a label""")
        numtoks(ctx) == 1 && lineerror(ctx, """Assignment needs an expression""")
        eattok(ctx)
        local exprstr = tokstr(ctx)
        ctx.label != :* && push!(ctx.vars, ctx.label)
        println("@@@\n@@@ ASSIGN $(ctx.label)\n@@@")
        assign_var(lctx, exprstr)
        pushfunc!(ctx, 2, 3, 4, :stability) do
            println("@@@\n@@@ ASSIGN $(ctx.label)\n@@@")
            assign_var(lctx, exprstr)
        end
    end

function reset(ctx::CodeContext)
    ctx.offset = 0
    ctx.min = 64K
    ctx.max = 0
    setdiff!(ctx.assigned, ctx.vars)
    eval(ctx, quote
        $((:($var = nothing) for var in ctx.vars)...)
    end)
end

emit(ctx::CodeContext, bytes::UInt8...) = emitall(ctx, collect(bytes))

function emitall(ctx::CodeContext, bytes::AbstractVector{UInt8})
    isempty(bytes) && return
    println(
        "EMIT $(rhex(ctx.offset)): $(length(bytes)) byte$(length(bytes) != 1 ? "s" : "")",
        #"$(ctx.line.line)",
        "$(bytes)",
    )
    @view(ctx.memory[ctx.offset+1:ctx.offset+length(bytes)]) .= bytes
    incoffset(ctx, length(bytes); track = true)
end

function assemble(ctx::CodeContext, op, addr, exprstr = "()")
    local lctx = ListingContext(ctx)
    local opcode = get(opcodes, (op, addr)) do
        lineerror(ctx, """Illegal $op does not support addressing mode $addr""")
    end
    local bytes = addrmodebytes[addr]
    local expr = try
        Meta.parse(exprstr)
    catch err
        @error "Error assembling opcode" exception = (err, catch_backtrace())
        lineerror(ctx, """Could not parse argument for $op, '$exprstr'""")
    end
    isincomplete(expr) && lineerror(ctx, """Incomplete argument for $op, '$exprstr'""")
    pushfunc!(ctx, 2, 3, :stability) do
        if addr == :abso
            local arg = try
                eval(ctx, expr)
            catch
                nothing
            end
            if arg isa UInt8 || arg isa Int8
                addr == :zp
                bytes = 2
            end
        end
        incoffset(ctx, bytes; track = true)
    end
    #local line = ctx.line
    pushfunc!(ctx, 4) do
        local arg = eval(ctx, expr)
        #log("line: $line '$expr' = $arg")
        #log(
        #    "assemble $(rhex(ctx.offset)) ($op $addr)$(exprstr == "()" ? "" : " $expr") ->  $(join([rhex(opcode), arg...], " ")), line: $(ctx.line)"
        #)
        add_listing(lctx, :opcode, bytes)
        if bytes == 1
            emit(ctx, opcode)
        elseif bytes == 2
            local a = if addr != :rel
                UInt8(arg)
            elseif arg isa Union{UInt8,Int8}
                reinterpret(UInt8, arg)
            else
                #log("BRANCH FROM  $(ctx.offset) TO $arg: $(reinterpret(UInt8, Int8(Int(arg) - Int(ctx.offset) + 2)))")
                reinterpret(UInt8, Int8(Int(arg) - Int(ctx.offset) - 2))
            end
            emit(ctx, opcode, a)
        elseif bytes == 3
            local a = UInt16(arg)
            emit(ctx, opcode, UInt8(a & 0xFF), UInt8(a >> 8))
        end
    end
    incoffset(ctx, bytes)
    true
end

asm_op(ctx::CodeContext) =
    assembleif(ctx, tok_is(∈(legal_ops))) do
        local op = Symbol(lowercase(tok(ctx)))
        local opname = uppercase(string(op))
        local modes = keys(opcodes)
        eattok(ctx)
        if !isnothing(ctx.label)
            ctx.label ∈ ctx.assigned && lineerror(ctx, """Redeclaring label $(ctx.label)""")
            push!(ctx.labels, ctx.label)
            push!(ctx.assigned, ctx.label)
        end
        !hastoks(ctx) && (op, :imp) ∈ modes && return assemble(ctx, op, :imp)
        !hastoks(ctx) && (op, :acc) ∈ modes && return assemble(ctx, op, :acc)
        !hastoks(ctx) && lineerror(ctx, """No arguments for opcode $opname""")
        op ∈ branch_ops && return assemble(ctx, op, :rel, tokstr(ctx))
        is(ctx, "#") && return assemble(ctx, op, :imm, tokstr(ctx, ctx.toks[2:end]))
        is(ctx, "(") &&
            lasttoks(ctx, ")", ",", "y") &&
            return assemble(ctx, op, :indy, tokstr(ctx, ctx.toks[2:end-3]))
        is(ctx, "(") &&
            lasttoks(ctx, ",", "x", ")") &&
            return assemble(ctx, op, :indx, tokstr(ctx, ctx.toks[2:end-3]))
        #check for zpx as opposed to absx -- skip for now and assemble ,x as absolute,x
        lasttoks(ctx, ",", "y") &&
            return assemble(ctx, op, :absy, tokstr(ctx, ctx.toks[1:end-2]))
        if lasttoks(ctx, ",", "x")
            #log("abs,x: toks: $(ctx.toks)")
            return assemble(ctx, op, :absx, tokstr(ctx, ctx.toks[1:end-2]))
        end
        is(ctx, "(") &&
            lasttoks(ctx, ")") &&
            op == :jmp &&
            return assemble(ctx, op, :ind, tokstr(ctx, ctx.toks[2:end-1]))
        is(ctx, "(") &&
            lasttoks(ctx, ")") &&
            lineerror(ctx, """Only JMP can use indirect addressing but this is $opname""")
        is(ctx, "(") && lineerror(ctx, """Incomplete indirect expression, $(tokstr(ctx))""")
        # must be absolute
        assemble(ctx, op, :abso, tokstr(ctx))
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
            continue
        elseif !asm_include(ctx, line, remain)
            push!(remain, line)
        end
        l += 1
    end
    return remain
end

"""
include a file during pass 1
"""
asm_include(ctx::CodeContext, line::Line, lines::Vector{Line}) =
    assembleif(line, ".include") do
        try
            add_listing(ctx, :include)
            local name = tokstr(line.line, line.tokens[2], line.tokens[end])
            println("INCLUDE $name")
            if endswith(name, ".jl")
                local file = joinpath(ctx.pwd, name)
                eval(ctx, :(include($file)))
            elseif endswith(name, ".jas")
                local file = joinpath(ctx.pwd, name)
                for (n, line) in enumerate(readlines(open(file, "r")))
                    push!(lines, tokenize(n, line))
                end
            end
        catch err
            @error "Error running .include directive: $(tokstr(line))" exception =
                (err, catch_backtrace())
            lineerror(ctx, """Error running .include directive: $(tokstr(line))""")
        end
    end

ismacrodecl(line::Line) = !isnothing(findfirst(m -> m.match == ".macro", line.tokens))

lineerror(ctx::Union{ListingContext,CodeContext}, msg, err = nothing) = lineerror(ctx.line, msg, err)

function lineerror(line::Line, msg, err = nothing)
    log("Error on line $(line.number), $msg: $(line.line)")
    !isnothing(err) && log(err)
    error("Error on line $(line.number), $msg: $(line.line)")
end

function compilemacro(ctx::CodeContext)
    local firstline = ctx.line
    local label = Symbol(ctx.toks[1].match)
    string(label) == ".macro" && lineerror(ctx, """Attempt to define a nameless macro""")
    label ∈ keys(ctx.macros) && lineerror(ctx, """Attempt to redefine macro $label""")
    eattok(ctx)
    eattok(ctx)
    local expr, extra = parse_julia(ctx)
    expr.head != :-> &&
        lineerror(firstline, """Expected an arrow function ( (args)-> ... )""")
    println("MACRO $label: $expr")
    add_listing(ctx, :macro, extra)
    local macargs = expr.args[1]
    ctx.macros[label] =
        Macro(macargs isa Symbol ? [macargs] : [expr.args[1].args...], eval(ctx, expr))
end

add_listing(ctx::CodeContext, args...; kw...) =
    add_listing(ListingContext(ctx), args...; kw...)

add_listing(lctx::ListingContext, type::Symbol, bytes = 0; remove_label = false) =
    add_listing(lctx, type, lctx.label, bytes, lctx.line; remove_label)

add_listing(lctx::ListingContext, type::Symbol, extra::Vector{Line}; remove_label = false) =
    add_listing(lctx, type, lctx.label, 0, lctx.line, extra; remove_label)

function add_listing(
    lctx::ListingContext,
    type::Symbol,
    label::OptSym,
    bytes,
    line::Line,
    extra = ListingLine[];
    remove_label = false,
)
    if remove_label && !isnothing(label)
        label = nothing
        line =
            isempty(line.tokens) ? Line(line.number, "", RegexMatch[]) :
            tokenize(line.number, tokstr(line, line.tokens[2], line.tokens[end]))
    end
    #if type == :def && !isnothing(ctx.label) && ctx.macrovalue != 0
    #    addr = ctx.macrovalue
    #end
    listing = ListingLine(type, lctx.ctx.offset, label, bytes, line, extra)
    push!(lctx.listing, listing)
    if type == :opcode
        println("ADD OPCODE LISTING $listing")
        for line in extra
            println("           LISTING $line")
        end
    end
end

trimlines(str) = join((strip(line) for line in split(str, "\n")), "\n")

function print_listing(io, ctx::CodeContext, line::ListingLine)
    local label = string(something(line.label, ""))
    local toks = line.line.tokens
    toks = toks[(isempty(label) ? 1 : 2):end]
    #local text = isempty(toks) ? line.line.line : tokstr(line, toks)
    local text = line.line.line
    local bytes = @view(ctx.memory[line.addr+1:line.addr+line.bytes])
    local hexbytes = join(rhex.(bytes), " ")

    if !isempty(label)
        local lab = match(tok_pat, text)
        text = strip(@view text[lab.offset+length(lab.match):end])
    end
    local type, first = if line.type == :def
        text = ""
        "=", ""
    elseif line.type == :data
        ">", hexbytes
    elseif line.type == :julia
        "@", ""
    elseif line.type == :macro
        "@", ""
    elseif line.type == :imm
        "@", ""
    elseif line.type == :fake
        "@", ""
    elseif line.type == :value
        "=", ""
    elseif line.type == :assign
        "=", ""
    elseif line.type == :call
        "#", ""
    elseif line.type == :opcode
        #local op = line.line.tokens[isempty(label) ? 1 : 2].match * " "
        local op = uppercase("$(opsyms[bytes[1] + 1]) ")

        if addrsyms[bytes[1]+1] == :imm
            op *= "#"
        end
        #println("OP\n  LINE: $line\n  BYTES: $hexbytes")
        if length(bytes) == 2
            op *= "$(hex(bytes[2]))"
        elseif length(bytes) == 3
            op *= "$(hex(bytes[2] | (UInt16(bytes[3]) << 8)))"
        end
        ".", @sprintf("%-15s%-17s", hexbytes, op)
    elseif line.type == :include
        "@", ""
    end
    @printf(io, "%s%04X  %-32s%-12s%s\n", type, line.addr, first, label, text)
end

function listings(ctx::CodeContext, file)
    println("LISTINGS FOR $file[$(hex(ctx.min)):$(hex(ctx.max))]")
    open(file, "w") do io
        listings(io, ctx)
    end
end

function listings(io::IO, ctx::CodeContext)
    println(io, trimlines("""
            ; JAS Julia-based 6502 Assembler
            ; jas $(join(ARGS, " "))
            ; $(now())
        """))
    for line in sort(ctx.listing, by = l -> l.addr)
        print_listing(io, ctx, line)
    end
end

test() = asmfile(joinpath(dirname(dirname(@__FILE__)), "examples", "simple.jas"))

end # module Asm
