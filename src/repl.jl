"""
    AsmRepl

6502 REPL commands
You can type assembly directly, which adds it to an in-memory program.
There are additional directives:
.help                show documentation on REPL directives
.clear               erase memory and local program
.load FILE           load FILE as local program (see .edit and .save)
.save FILE           save local program to FILE (see .edit and .load)
.list                list the current program
.edit                run ENV["EDITOR"] on local program and reload afterwards (see .load and .save)
.clear               delete current program
.status              print the current machine status
.diag                show status and list all break and watch points
.run [LOCATION]      assemble local program and continue (optionally from LOCATION)
.reset               clear machine state so that the next .run will start fresh
.break RANGE [EXPR]  break when execution enters RANGE if optional EXPR is true
.watch RANGE [EXPR]  break whenever RANGE changes if optional EXPR is true
.del RANGE           delete break and watch points at RANGE
.show                show the UI
.step [LOCATION]     perform one step (optionally from LOCATION)
.next [LOCATION]     perform one step (optiaonlly from LOCATION) -- if it's a JSR, break upon return
CTRL-C               pause current execution

LOCATION is a Julia expr which returns an integer
RANGE is a Julia expr which can be an integer or collection

The REPL manages its own UI along with workers for assembly and execution.
"""
module AsmRepl

using FileWatching
using ..Asm: Asm, Line, dot_cmds, isincomplete
using ..Fake6502m: opsyms
using REPL
using REPL: LineEdit, CompletionProvider
using ReplMaker

const COMMENT_PAT = r"^\s*;.*"
const DIRECTIVE_PAT = r"^\s*(?:([a-z0-9_]+)\s+)?(\.?[a-z]+)\b"si
const LOAD_PAT = r"^\s*(\+w\s+)?(\S.*)?$"
const SAVE_PAT = r"^\s*(\-f\s+)?(\S.*)?$"

last_load = ""
last_save = ""

function cmd_break() end
function cmd_clear() end
function cmd_del() end
function cmd_diag() end
function cmd_edit() end
function cmd_help() end
function cmd_list() end
function cmd_load() end
function cmd_next() end
function cmd_run() end
function cmd_reset() end
function cmd_save() end
function cmd_show() end
function cmd_status() end
function cmd_step() end
function cmd_watch() end
const ASM_CMDS = Set(string.(opsyms))
const REPL_CMDS = Dict(
    ".break" => cmd_break,
    ".clear" => cmd_clear,
    ".del" => cmd_del,
    ".diag" => cmd_diag,
    ".edit" => cmd_edit,
    ".help" => cmd_help,
    ".list" => cmd_list,
    ".load" => cmd_load,
    ".next" => cmd_next,
    ".run" => cmd_run,
    ".reset" => cmd_reset,
    ".save" => cmd_save,
    ".show" => cmd_show,
    ".status" => cmd_status,
    ".step" => cmd_step,
    ".watch" => cmd_watch,
)
const DIRECTIVES = sort([keys(REPL_CMDS)..., dot_cmds...])
const ALL_CMDS = sort!([
    ASM_CMDS...,
    DIRECTIVES...,
])

mutable struct ReplContext{Specialization} <: CompletionProvider
    lines::Vector{String}
    cmd_handler::Function
    pending_asm_prefix::String
    pending_asm_expr::String
    mode::REPL.LineEdit.Prompt
    input_file::Union{String,Nothing}
    input_file_channel::Union{Channel{Nothing},Nothing}
    ReplContext{T}() where T = new{T}()
end

function repl(specialization::Type = Nothing)
    ctx = ReplContext{specialization}()
    ctx.lines = String[]
    ctx.cmd_handler = handle_command
    ctx.pending_asm_prefix = ""
    ctx.pending_asm_expr = ""
    ctx.input_file = ""
    ctx.input_file_channel = nothing
    ctx.mode = initrepl(
        s-> Base.invokelatest(ctx.cmd_handler, ctx, s);
        prompt_text="6502> ",
        prompt_color=:light_yellow,
        start_key='}',
        mode_name="ASM_mode",
        completion_provider=ctx,
    )
end

function LineEdit.complete_line(::ReplContext, state)
    local str = LineEdit.input_string(state)
    return [dir for dir in ALL_CMDS if startswith(dir, str)], str, true
end

function handle_command(ctx::ReplContext, line)
    if !isnothing(match(COMMENT_PAT, line))
        push!(ctx.lines, line)
        return nothing
    end
    local m = match(DIRECTIVE_PAT, line)
    local toks = [t for t in something(m, []) if !isnothing(t) && !isempty(t)]
    local label, cmd = if length(toks) == 2
        toks
    else
        nothing, toks[1]
    end
    if isnothing(cmd) || string(cmd) ∉ ALL_CMDS
        isnothing(m) && println("NOT A COMMAND")
        !isnothing(m) && println("UNRECOGNIZED COMMAND $line")
        println("ASM REPL expected an assembly instruction or a directive: $(join([DIRECTIVES..., dot_cmds...], ", "))")
        return nothing
    end
    local prefix = line[1:cmd.offset + length(cmd)]
    local body = line[length(prefix) + 1:end]
    if haskey(REPL_CMDS, cmd)
        println("REPL CMD: $cmd")
        REPL_CMDS[cmd](ctx, cmd, strip(body))
    else
        cmd_asm(ctx, label, cmd, prefix, body, line)
    end
    nothing
end

function handle_asm_chunk(ctx::ReplContext, line)
    ctx.pending_asm_expr *= line * '\n'
    if !isincomplete(ctx.pending_asm_expr)
        println("$(ctx.pending_asm_expr) is complete")
        push!(ctx.lines, string.(split(ctx.pending_asm_prefix * ctx.pending_asm_expr, "\n"))...)
        ctx.pending_asm_expr = ""
        ctx.pending_asm_prefix = ""
        ctx.cmd_handler = handle_command
        ctx.mode.prompt = "6502> "
    end
    nothing
end

function cmd_break(ctx::ReplContext, cmd, args)
    println("BREAK $args")
end

function cmd_clear(ctx::ReplContext, cmd, args)
    println("CLEAR $args")
end

function cmd_del(ctx::ReplContext, cmd, args)
    println("DEL $args")
end

function cmd_diag(ctx::ReplContext, cmd, args)
    println("DIAG $args")
end

function cmd_edit(ctx::ReplContext, cmd, args)
    println("EDIT $args")
end

function cmd_help(ctx::ReplContext, cmd, args)
    println("HELP $args")
end

function cmd_list(ctx::ReplContext, _, _)
    println(join(ctx.lines, "\n"))
end

cmderror(msg) = @error msg _module=Main _file="REPL" _line="1"

function cmd_load(ctx::ReplContext, cmd, args)
    println("LOAD $args")
    local nowatch, file = match(LOAD_PAT, args)
    println("nowatch: $(!isempty(something(nowatch, ""))), file: $file")
    isempty(something(file, "")) && isempty(last_load) &&
        return cmderror("No file to load")
    local path = try
        realpath(file)
    catch
        nothing
    end
    !isnothing(path) && !isfile(path) &&
        return cmderror("No file $path")
    try
        ctx.lines = readlines(path)
        if !isempty(ctx.input_file) && ctx.input_file != path
            stop_watching(ctx)
        end
        if isnothing(ctx.input_file_channel)
            start_watching(ctx, path)
        end
    catch
        cmderror("Could not read file $file")
    end
end

function start_watching(ctx::ReplContext, file::AbstractString)
    ctx.input_file = string(file)
    ctx.input_file_channel = Channel{Nothing}()
    @async begin
        println("WATCHING $file FOR CHANGES")
        while true
            if isready(ctx.input_file_channel)
                take!(ctx.input_file_channel)
                close(ctx.input_file_channel)
                ctx.input_file_channel = nothing
                ctx.input_file = nothing
                println("NO LONGER WATCHING $(ctx.input_file) FOR CHANGES")
                break
            end
            local change = watch_file(ctx.input_file, 1)
            if !change.timedout
                input_file_changed(ctx, change)
            end
        end
    end
end

function input_file_changed(ctx::ReplContext, change)
    cmderror("INPUT FILE CHANGED BUT THERE IS NO HANDLER FOR THAT")
end

function stop_watching(ctx::ReplContext)
    !isnothing(ctx.input_file_channel) &&
        put!(ctx.input_file_channel, nothing)
end

function cmd_save(ctx::ReplContext, cmd, args)
    isempty(ctx.lines) &&
        return cmderror("No program in memory")
    local force, file = match(LOAD_PAT, args)
    isempty(something(file, "")) && isempty(last_load)
        return cmderror("No file to save to")
    local path = realpath(file)
    !isdir(dirname(path)) &&
        error("No directory $(dirname(path))")
    isfile(path) && isempty(something(force, "")) &&
        error("File $path already exists")
    open(path, "w") do io
        write(io, join(ctx.lines, "\n"))
    end
    println("Wrote program to $path")
end

function cmd_next(ctx::ReplContext, cmd, args)
    println("NEXT $args")
end

function cmd_run(ctx::ReplContext, cmd, args)
    println("RUN $args")
end

function cmd_reset(ctx::ReplContext, cmd, args)
    println("RESET $args")
end

function cmd_show(ctx::ReplContext, cmd, args)
    println("SHOW $args")
end

function cmd_status(ctx::ReplContext, cmd, args)
    println("STATUS $args")
end

function cmd_step(ctx::ReplContext, cmd, args)
    println("STEP $args")
end

function cmd_watch(ctx::ReplContext, cmd, args)
    println("WATCH $args")
end

function cmd_asm(ctx::ReplContext, label, cmd, prefix, expr, line)
    println("ASM CMD: [$label] [$cmd] / [$prefix] [$expr]")
    if isincomplete(expr)
        ctx.pending_asm_prefix = prefix
        ctx.pending_asm_expr = expr * '\n'
        ctx.cmd_handler = handle_asm_chunk
        ctx.mode.prompt = "6502...> "
    else
        push!(ctx.lines, line)
    end
end

function create_machine()
end

end
