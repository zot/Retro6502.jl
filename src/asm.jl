"""
# 6502 Assembler with Julia-based macros and expressions

3 passes:

1. compile macros
2. run macros, assign labels and variables
3. assemble code, including running Julia exprs
"""
#=
## SYNTAX

-- inside asm strings (i.e. macros), \name and \(expr) are macro substitutions
-- when a == "q", \(a)_suffix becomes q_suffix
-- in ASM syntax, both $N and 0xN are hex numbers but 0x is preferred
-- in Julia code, normal Julia syntax holds

comments start with ; and are considered whitespace in the syntax rules

PROGRAM: { DEF "\n" }

DEF: (LABEL | "*" | "+" | "-") [INSTRUCTION]
 | INSTRUCTION
 | ".include" JULIAFILE
 | LABEL ".macro" ["(" ARGLIST ")"]
 | ".endmacro"
 | LABEL ".jmacro" "(" [ ARGLIST ] ")"
 | ".endjulia"
 | LABEL ".fake" "(" [ ARGLIST ] ")"
 | ".endfake"
 | ".if" EXPR
 | ".else"
 | ".elseif" EXPR
 | ".endif"
 | (LABEL | "*") LABLEOP EXPR

LABEL: IDENT

LABELOP: "=" | "+=" | "-=" | "*=" | "/=" | "&=" | "|=" | "⊻="

INSTRUCTION: OPCODE [OPARG]
 | "#" IDENT [ARGLIST]

ARGLIST: IDENT { "," IDENT }

OPCODE: ( "ADC" | ... )

OPARG: EXPR
 | "#" EXPR
 | "(" EXPR ")"
 | "(" EXPR "," "x" ")"
 | "(" EXPR ")" "," "y"

EXPR: <Julia expression>

JULIADEF: EXPR
 | "begin" JULIACODE "end"

## How assembly works

struct CodeChunk
    label_offsets::Dict{String,Int} # offsets for any labels this code defines
    code_func::Function             # code_func(ctx, args...) adds machine code to ctx.memory (64K UInt8 array)
end

struct CodeContext
    labels::Dict{String,Int}    # all labels
    chunks::Vector{CodeChunk}
    memory::Vector{UInt8}       # 64K memory array
    changed::Vector{Bool}       # record of each memory cell that has been changed
end

The assembler:

1. Includes Julia source and compiles all macros
2. Runs the ASM code, during which asm"..." strings add a CodeChunk to CONTEXT.chunks,
3. Processes the CODE array to generate machine code,
4. Macro calls get the CodeContext and the current label being defined (or nothing) as arguments.

## Reference

        .if EXPR        ; conditionally output code until .else, .elseif, or .end
        .else           ; conditionally output code until .else, .elseif, or .end
        .elseif EXPR    ; conditionally output code until .else, .elseif, or .end
        .endif          ; end a .if section
; .julia declares a Julia-based macro. Can manually add your own CodeChunk to the CODE array if you like
joe     .julia(a, b)-> JULIA CODE # declares a Julia macro named joe, regular Julia syntax here
mary    .julia(a, b)-> begin # declares a multi-line Julia macro named mary
        for i in 1:b    # output b copies of the following assembly
            asm"        ; output ASM code -- $ substitutes in Julia values
            STA \a
            STX \(a + 1)
            "
        end
fred    .macro(a, b) ; shorthand for .julia(a, b)-> asm"..."
        STA a
q       STX b
        STY 0x3110
        .return q ; makes fred get q instead of the first address of the macro
        .end
floop   #macr 1, 2 ; calls macro named "macr", floop gets the returned value of the macro
fred    .fake(a, b)-> JULIA CODE # declares a Julia-based fake subroutine which runs at runtime, not asm time
        .include file ; load Julia code -- can be called by macros or fake subroutines

 ; special labels
*       ; the current output location
-       ; a temporary label, can be referenced by - backwards and + forwards
        ; Use more than one +/- to reference further, like --, ---, +++, and so on.
+       ; a temporary label

 ; label operations
label   = EXPR  ; change the value of a label -- code following this line will see EXPR as the value of label
=#
module Asm
using ..Fake6502m: addrsyms, opsyms
import ..Fake6502: K, M, G

const opcodes = Dict((opsyms[i + 1], addrsyms[i + 1]) => i for i in 0x00:0xFF)
const legal_ops = Set(string.(opsyms))
#const opcode_addrmodes = foldl((d, (op, addr))-> push!(d[op], addr), keys(addrmodes);
#                               init = Dict{Symbol,Set}(op => Set() for op in legal_ops))
const branch_ops = Set([:bpl,:bmi,:bvc,:bvs,:bcc,:bcs,:bne,:beq])
const tok_pat = r";.*$|[\w.]+|\$[0-9a-fA-F]+|[0-9]+(?:\.[0-9]*)?+|[][(){},\"'#*]|[^\w\d\s(),]+"
const ident_pat = r"^\w+$"
const label_pat = r"^\w+$|^\*$"
const call_pat = r"#\w+"
const bracket_pat = r"[][(){},\"']"
const string_bracket_pat = r"\"|\$\("
const brackets = Dict("("=>")", "["=>"]", "{"=>"}", "\""=>"\"", "'"=>"'", "\$("=>")")
const close_brackets = ")]}"
const assignments = Set(["=", "+=", "-=", "|=", "&="])
const dot_cmds = Set([".var", ".data", ".macro", ".endmacro", ".jmacro", ".endjmacro",
                      ".fake", ".endfake", ".if", ".else", ".elseif", ".endif"])
const directives = Set([legal_ops..., assignments..., dot_cmds...])

matches(r, s) = !isnothing(match(r, s))

struct CodeChunk
    label_offsets::Dict{String,Int} # offsets for any labels this code defines
    code_func::Function             # code_func(ctx, args...) adds machine code to ctx.memory (64K UInt8 array)
end

@kwdef mutable struct CodeContext
    env::Module = eval(:(module $(Symbol(String(rand('a':'z', 16)))) ctxeval(expr) = eval(expr) end))
    macros::Dict{Symbol,Function} = Dict{Symbol,Function}()
    funcs::Vector{Function} = Function[]
    labels::Set{Symbol} = Set{Symbol}()
    vars::Set{Symbol} = Set{Symbol}()
    offset::Int = 0
    line::String = ""
    toks::Vector{RegexMatch} = RegexMatch[]
    label::Union{Symbol,Nothing} = nothing
    assigned::Set{Symbol} = Set{Symbol}() # variables which have been assigned
    output::Int = 0
    memory::Vector{UInt8} = zeros(UInt8, 64K)         # 64K memory array
    #address::Ref{UInt16} = 0x0000
    #labels::Dict{String,Int} = Dict{String,Int}()
    #relative_labels::Set{Int} = Set{Int}()
    #chunks::Vector{CodeChunk} = CodeChunk[]
    #changed::Vector{Bool} = Bool[]          # record of each memory cell that has been changed
    #macrostack::Vector{Symbol} = Symbol[]
    #macrovalue::Any = nothing
end

struct Line
    number::Int
    line::String
    tokens::Vector{RegexMatch}
end

function tokenize(n, line)
    tokens = [eachmatch(tok_pat, line)...]
    !isempty(tokens) && startswith(tokens[end].match, ';') &&
        return Line(n, line, @view tokens[1:end-1])
    return Line(n, line, tokens)
end
const CTX = :__CONTEXT__

numtoks(ctx::CodeContext) = length(ctx.toks)

hastoks(ctx::CodeContext) = !isempty(ctx.toks)

eattok(ctx::CodeContext) = ctx.toks = @view ctx.toks[2:end]

tok(ctx::CodeContext) = ctx.toks[1].match

tokstr(ctx::CodeContext) = tokstr(ctx.line, ctx.toks[1], ctx.toks[end])

tokstr(str, tok1, tok2) = @view str[tok1.offset:tok2.offset + length(tok2.match)]

function lasttoks(ctx::CodeContext, items...)
    length(ctx.toks) < length(items) &&
        return false
    for i in 0:length(items)-1
        lowercase(ctx.toks[end-i].match) != lowercase(items[end-i]) &&
            return false
    end
    return true
end

isdirective(ctx::CodeContext) = hastoks(ctx) &&
    (ctx.toks[1].match ∈ directives || matches(call_pat, tok(ctx)))

isop(ctx::CodeContext) = hastoks(ctx) && tok(ctx) ∈ legal_ops

iscall(ctx::CodeContext) = hastoks(ctx) && matches(call_pat, tok(ctx))

is(ctx::CodeContext, str) = hastoks(ctx) && tok(ctx) == str

isassign(ctx::CodeContext) = hastoks(ctx) && 0 < length(tok(ctx)) < 3 && tok(ctx)[end] == "="

isident(ctx::CodeContext) = hastoks(ctx) && matches(ident_pat, tok(ctx))

islabel(ctx::CodeContext) = hastoks(ctx) && matches(label_pat, tok(ctx))

"""
    produce a CodeChunk based on str
"""
function asm(str)
    local lines = [tokenize(n, line)
                   for (n, line) in enumerate(split(str, "\n"))]
    # pass 1
    local macros::Dict, remaining = compile_macros_pass1(lines)
    local ctx = CodeContext(; macros)
    # pass 2
    invokelatest() do
        scan_asm_pass2(ctx, remaining)
        # pass 3
        invokelatest() do
            reset(ctx)
            for func in ctx.funcs
                func()
            end
        end
    end
    ctx
end

getvar(ctx::CodeContext, var::Symbol) = ctx.env.eval(var)

function setvar(ctx::CodeContext, var::Symbol, value)
    local argname = Symbol("_" * string(var))
    println("CTX ENV EVAL: ", ctx.env.ctxeval)
    invokelatest(ctx.env.ctxeval((:(function($argname) global $var = $argname; end))), value)
end

"""
Scan asm code, find label offsets and construct gen function
"""
function scan_asm_pass2(ctx, lines)
    for line in lines
        ctx.line = line.line
        ctx.toks = line.tokens
        ctx.label = nothing
        !hastoks(ctx) &&
            continue
        if !isdirective(ctx) && islabel(ctx)
            # a labeled statement -- statement decides whether this is a label or a variable
            ctx.label = Symbol(tok(ctx))
            eattok(ctx)
            asm_var(ctx) &&
                continue
            push!(ctx.labels, ctx.label)
            asm_call(ctx) &&
                # run the macro, determine the label offset and add code the pass
                continue
            # set the label to the current offset
            setvar(ctx, ctx.label, ctx.offset)
            !hastoks(ctx) &&
                continue
        end
        (asm_assign(ctx) ||
            asm_op(ctx)) &&
            continue
        !isdirective(ctx) &&
            lineerror(line, """Unknown directive, $(tokstr(ctx))""")
        #local label = nothing
        #if toks[1].match ∉ legal_ops
        #    label = toks[1].match
        #    toks = @view toks[2:end]
        #end
        #if isempty(toks)
        #    if !isnothing(label)
        #        push!(funcs, (ctx)-> ctx.labels[label] = ctx.address)
        #    end
        #elseif isassign()
        #    continue
        #elseif isop(ctx)
        #elseif iscall(toks)
        #    local macroname = Symbol(toks[1].match[2:end])
        ##    local args = numtoks(ctx) == 1 ? [] :
        ##        macroargs(@view line[toks[2].offset:length(toks[end].match) + toks[end].offset])
        ##    !haskey(macros, macroname) &&
        ##        lineerror(line, "Unknown macro $macroname")
        ##    if !isnothing(label)
        ##        push!(funcs, Meta.eval(quote
        ##                                   $CTX.labels[$(QuoteNode(label))] = callmacro($CTX, args)
        ##                               end))
        ##    else
        ##        push!(funcs, Meta.eval(quote
        ##                                   #todo gather macro args
        ##                                   error("gathering macro args not implemented")
        ##                                   $(QuoteNode(macroname)) ∉ $CTX.macros &&
        ##                                       lineerror(line, "Unknown macro " * $(string(macroname)))
        ##                                   callmacro($CTX, args)
        ##                               end))
        ##    end
        ##elseif is(toks, ".return")
        ##    length(toks) == 1 &&
        ##        lineerror(line, """.return with no value""")
        #end
        #!isempty(toks) && toks[1].match ∉ legal_ops &&
        #    lineerror(line, """Unknown directive""")
        #push!(funcs, defop(@view toks[:]))
        #!isnothing(label) &&
        #    push!(funcs, deflabel(label))
        #isempty(toks) &&
        #    continue
    end
end

function asm_var(ctx::CodeContext)
    local var = ctx.label
    !is(ctx, ".var") &&
        return false
    var ∈ ctx.labels &&
        lineerror(ctx, """Attempt to declare $(ctx.label) as a variable but it is already a label""")
    var ∈ ctx.vars &&
        lineerror(ctx, """Attempt to redeclare $(ctx.label) as a variable""")
    # save the var for resetting after this pass
    push!(ctx.vars, ctx.label)
    assign_var(ctx, numtoks(ctx) == 1 ? "0" : tokstr(ctx.line, ctx.toks[2], ctx.toks[end]))
    true
end

function assign_var(ctx::CodeContext, exprstr, wrapper = identity)
    local var = ctx.label
    local progress = "compiling expression"
    try
        local expr = Meta.parse(exprstr)
        expr isa Expr && expr.head == :incomplete &&
            error()
        push!(ctx.assigned, var)
        local func = wrapper(ctx.env.eval(:(function() global $var = $expr end)))
        # push the func for pass 3
        push!(ctx.funcs, func)
        # execute the func for the rest of this pass
        progress = "executing expression"
        func()
    catch
        lineerror(ctx, """Error $progress, $exprstr""")
    end
end

#TODO
function asm_call(ctx::CodeContext)
    !iscall(ctx) &&
        return false
    error("MACRO CALLS NOT IMPLEMENTED")
end

function asm_assign(ctx::CodeContext)
    !isassign(ctx) &&
        return false
    ctx.label ∈ ctx.labels &&
        lineerror(ctx, """Attempt to assign a label""")
    numtoks(ctx) == 1 &&
        lineerror(ctx, """Assignment needs an expression""")
    local op = tok(ctx)
    eattok(ctx)
    local exprstr = tokstr(ctx)
    local var = ctx.label
    if op == "="
        assign_var(ctx, exprstr, (setfunc)-> begin
                       var ∈ ctx.assigned &&
                           lineerror(ctx, """Attempt to reassign label, $var""")
                       setfunc
                   end)
        push!(ctx.labels, var)
    elseif op == ":="
        assign_var(ctx, exprstr)
        push!(ctx.vars, var)
    elseif op == ":?="
        assign_var(ctx, exprstr, (setfunc)-> !haskey(ctx.assigned, var) && setfunc())
    else
        ctx.label ∉ ctx.vars &&
            lineerror(ctx, """Attempt to assign $var but it is not a variable""")
        if op == "+="
            assign_var(ctx, "$var + ($exprstr)")
        elseif op == "-="
            assign_var(ctx, "$var - ($exprstr)")
        elseif op == "|="
            assign_var(ctx, "$var | ($exprstr)")
        elseif op == "&="
            assign_var(ctx, "$var & ($exprstr)")
        end
    end
    true
end

function reset(ctx::CodeContext)
    ctx.output = 0
    setdiff!(ctx.assigned, ctx.vars)
    ctx.env.eval(
        quote
            $((:($var = nothing) for var in ctx.vars)...)
        end
    )
end

function emit(ctx::CodeContext, byte::UInt8)
    ctx.output += 1
    ctx.memory[ctx.output] = byte
end

function assemble(ctx::CodeContext, op, addr, expr = nothing)
    #local opcode = opcodes((op, addr))
    #(addr == :acc || addr == :imp) &&
    #    push!(ctx.funcs, ()-> emit(ctx, opcode))
    #local func = function(ctx)
    #    
    #end
    ##push!(ctx.funcs, ()-> assemble_func(ctx, op, addr, expr))
    #push!(ctx.funcs, ()-> println("assemble $(addrmodes[(op, addr)]) ($op $addr)$(isnothing(expr) ? "" : " " * expr)"))
    true
end

function asm_op(ctx::CodeContext)
    !isop(ctx) &&
        return false
    local op = Symbol(lowercase(tok(ctx)))
    local opname = uppercase(string(op))
    local modes = keys(opcodes)
    local args = strip(tokstr(ctx))
    if !isnothing(ctx.label)
        ctx.label ∈ ctx.assigned &&
            lineerror(ctx, """Redeclaring label $(ctx.label)""")
        push!(ctx.labels, ctx.label)
        push!(ctx.assigned, ctx.label)
    end
    eattok(ctx)
    # consume one address for the opcode
    ctx.offset += 1
    !hastoks(ctx) && (op, :imp) ∈ modes &&
        return assemble(ctx, op, :imp)
    !hastoks(ctx) && (op, :acc) ∈ modes &&
        return assemble(ctx, op, :acc)
    !hastoks(ctx) &&
        lineerror(ctx, """No arguments for opcode $opname""")
    # consume one address for the opcode's argument byte
    ctx.offset += 1
    op ∈ branch_ops &&
        return assemble(ctx, op, :rel, args)
    is(ctx, "#") &&
        return assemble(ctx, op, :imm, args)
    is(ctx, "(") && lasttoks(ctx, ")", ",", "y") &&
        return assemble(ctx, op, :indy, args)
    is(ctx, "(") && lasttoks(ctx, ",", "x", ")") &&
        return assemble(ctx, op, :indx, args)
    #check for zpx as opposed to absx -- skip for now and assemble ,x as absolute,x
    # consume one address for the opcode's second argument byte
    ctx.offset += 1
    lasttoks(ctx, ",", "y") &&
        return assemble(ctx, op, :absy, args)
    lasttoks(ctx, ",", "x") &&
        return assemble(ctx, op, :absx, args)
    is(ctx, "(") && lasttoks(ctx, ")") && op == :jmp &&
        return assemble(ctx, op, :ind, args)
    is(ctx, "(") && lasttoks(ctx, ")") &&
        lineerror(ctx, """Only JMP can use indirect addressing but this is $opname""")
    is(ctx, "(") &&
        lineerror(ctx, """Incomplete indirect expression, $(tokstr(ctx))""")
    # must be absolute
    return assemble(ctx, op, :abso, args)
end

function macroargs(line, str)
    local remain = str
    local args = []
    local cur = []
    local bracket = []
    local inquote = false
    local next
    local tok
    offset() = length(str) - length(remain)
    endtok() = isnothing(next) ? length(str) : next.offset + length(next.match) - 1
    usetok() = push!(cur, @view remain[1:endtok()])
    function openbracket()
        usetok()
        push!(bracket, (; offset = offset(), close = brackets[tok], inquote, bracket=tok))
        inquote = false
    end
    function closebracket()
        isempty(bracket) &&
            lineerror(line, """Close bracket without open, '$tok'""")
        tok != bracket[end].close &&
            lineerror(line, """Unbalanced brackets, $(str[bracket[end].offset:endtok()])""")
        usetok()
        inquote = bracket[end].inquote
        pop!(bracket)
    end
    while !isempty(remain)
        next = match((inquote ? string_bracket_pat : bracket_pat), remain)
        if isnothing(next)
            push!(cur, remain)
            break
        end
        println("MATCH $next")
        tok = next.match
        if inquote
            if tok == "\$("
                openbracket()
            elseif tok == "\""
                closebracket()
            else
                usetok()
            end
        elseif tok == ","
            if isempty(bracket)
                push!(cur, @view remain[1:next.offset-1])
                push!(args, strip(join(cur)))
                empty!(cur)
            else
                usetok()
            end
        elseif tok == "\""
            if inquote
                closebracket()
            else
                openbracket()
                inquote = true
            end
        elseif haskey(brackets, tok)
            openbracket()
        elseif occursin(tok, close_brackets)
            closebracket()
        end
        remain = @view remain[endtok() + 1:end]
    end
    !isempty(bracket) &&
        lineerror(line, """Unmatched open bracket: $(str[bracket[end].offset:endtok()])""")
    !isempty(cur) &&
        push!(args, strip(join(cur)))
    return args
end

# TODO
#function callmacro(ctx, args)
#    
#end

function deflabel(tok::RegexMatch)
#    local label = tok.match
#    (ctx)-> begin
#        ctx.
#            end
end

"""
Extract macros from assembly code, returning the compiled macros and the rest of the code
"""
function compile_macros_pass1(lines)
    local l = 1
    local macros = Dict{Symbol,Function}()
    local remain = Line[]
    while l <= length(lines)
        local line = lines[l]
        if ismacrodecl(line)
            local label, func, next_l = compilemacro(lines, l)
            label ∈ keys(macros) &&
                lineerror(l[1], """Macro name "$label" already in use""")
            macros[label] = func
            l = next_l
        else
            push!(remain, line)
            l += 1
        end
    end
    return macros, remain
end

ismacrodecl(line::Line) = !isnothing(findfirst(m-> m.match == ".macro", line.tokens))

isjmacrodecl(line::Line) = !isnothing(findfirst(m-> m.match == ".jmacro", line.tokens))

isfakedecl(line::Line) = !isnothing(findfirst(m-> m.match == ".fake", line.tokens))

lineerror(ctx::CodeContext, msg) = lineerror(ctx.line, msg)

lineerror(line::Line, msg) =
    error("Error on line $(line.number), $msg: $(line.line)")

function compilemacro(lines, l)
    local argnames = []
    local tokens = lines[l].tokens
    (length(tokens) == 1 || tokens[2].match != ".macro" || startswith(tokens[1].match, '.')) &&
        lineerror(lines[l], """Expected LABEL ".macro" ["(" [ IDENT { "," IDENT } ] ")"] """)
    local label = Symbol(tokens[1].match)
    if length(tokens) > 2
        eatargs(argnames, lines[l], @view(tokens[3:end]),
                """Expected LABEL ".macro" ["(" [ IDENT { "," IDENT } ] ")"] """)
    end
    l += 1
    #... generate function that splices arg refs into the macro's string
    local buf = []
    while true
        l > length(lines) &&
            lineerror(lines[1], "missing .endmacro")
        local line = lines[l]
        !isempty(line.tokens) && line.tokens[1].match == ".endmacro" &&
            break
        push!(buf, line.line, "\n")
        l += 1
    end
    local str = join(buf)
    local macr = []
    while !isempty(str)
        local idx = findfirst('\\', str)
        if isnothing(idx)
            push!(macr, str)
            break
        end
        length(str) == idx && # str ends in \
            lineerror(lines[l], """Bad macro syntax, expected \\NAME or \\(EXPR)""")
        push!(macr, @view str[1:idx-1])
        local expr, nstr = eatexpr(lines[l], @view str[idx+1:end])
        push!(macr, expr)
        str = nstr
    end
    return label, eval(
        quote
            function(ctx, $(argnames...))
                try
                    join([$(macr...)])
                catch err
                    @error ("Error in macro " * $label) exception=(err,catch_backtrace())
                end
            end
        end
    ), l
end

function eatargs(argnames, line, tokens, syntax)
    if !isempty(tokens) && (tokens[1].match != "(" || tokens[end].match != ")")
        lineerror(line, """Expected $syntax""")
    end
    length(tokens) < 3 &&
        return
    tokens = @view tokens[2:end-1]
    !matches(ident_pat, tokens[1].match) &&
        lineerror(line, """Expected $syntax""")
    push!(argnames, Symbol(tokens[1].match))
    tokens = @view tokens[2:end]
    # there should be an even number of tokens left (alternating "," and name)
    length(tokens) % 2 != 0 &&
        lineerror(line, """Expected $syntax""")
    for (comma, ident) in Iterators.partition(tokens, 2)
        (comma.match != "," || !match(ident_pat, ident.match)) &&
            lineerror(line, """Expected $syntax""")
        push!(argnames, Symbol(ident.match))
    end
end

function eatexpr(line, str)
    local orig = str
    if str[1] != '('
        local m = match(ident_pat, str)
        (isnothing(m) || m.offset != 1) &&
            lineerror(line, """Bad macro reference""")
        return Symbol(m.match), @view str[length(m.match) + 1:end]
    end
    local len = 0
    while !isempty(str)
        local idx = findfirst(')', str)
        isnothing(idx) &&
            break
        try
            println("PARSING: $(@view orig[1:len + idx])")
            local expr = Meta.parse(@view orig[1:len + idx])
            println("PARSED: $expr")
            (!(expr isa Expr) || expr.head != :incomplete) &&
                return expr, @view str[idx+1:end]
        catch
        end
        len += idx
        str = @view str[idx+1:end]
    end
    lineerror(line, """Could not parse Julia expression: $orig""")
end

function test()
    local loc = joinpath(dirname(dirname(@__FILE__)), "examples", "simple.jsm")
    asm(read(loc, String))
end

end # module Asm
