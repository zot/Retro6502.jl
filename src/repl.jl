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
.prg                 Assembler output for current program
.edit                run ENV["EDITOR"] on local program and reload afterwards (see .load and .save)
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

using Unicode, SharedArrays
using TerminalUserInterfaces:
    TerminalUserInterfaces,
    CrosstermTerminal,
    tui,
    move_cursor,
    TERMINAL,
    show_cursor,
    Layout,
    Paragraph,
    Block,
    Constraint,
    make_words
using TerminalUserInterfaces.Crayons
const TUI = TerminalUserInterfaces
import TerminalUserInterfaces: render, set, view, update!, init!, should_quit, Rect, Buffer
using Printf
using FileWatching
using ..Asm: Asm, Line, dot_cmds, isincomplete
using ...Fake6502: Fake6502, matches, display_chars, intrange, screen, log, BankSettings
using ..Fake6502m: opsyms, Cpu
using ..C64
using ..Workers: Workers, Worker, add_worker
using REPL
using REPL: LineEdit, CompletionProvider
using Crossterm
using Mmap
import Base: print, println

include("replbase.jl")
include("screen.jl")
include("private_replmaker.jl")
using .PrivateReplMaker

const COMMENT_PAT = r"^\s*;.*"
const DIRECTIVE_PAT = r"^\s*(?:([a-z0-9_]+)\s+)?(\.?[a-z]+)\b|^\s*(\.)\s*$"si
const LOAD_PAT = r"^\s*(\+w\s+)?(\S.*)?$"
const SAVE_PAT = r"^\s*(\-f\s+)?(\S.*)?$"
const DEFAULT_SETTINGS = (; maxticks = 150,)

last_load = ""
last_save = ""

function cmd_asm() end
function cmd_break() end
function cmd_clear() end
function cmd_del() end
function cmd_diag() end
function cmd_edit() end
function cmd_help() end
function cmd_list() end
function cmd_load() end
function cmd_next() end
function cmd_prg() end
function cmd_run() end
function cmd_reset() end
function cmd_save() end
function cmd_screen() end
function cmd_set() end
function cmd_show() end
function cmd_status() end
function cmd_step() end
function cmd_watch() end
const ASM_CMDS = Set(string.(opsyms))
const DEFS = [
    ".asm" => (cmd_asm, "", "Assemble the current program", ".a"),
    ".break" => (
        cmd_break,
        "RANGE [EXPR]",
        "break when execution enters RANGE if optional EXPR is true",
        ".b",
    ),
    ".clear" => (cmd_clear, "", "erase memory and local program"),
    ".del" => (cmd_del, "", "delete break and watch points at RANGE", ".d"),
    ".diag" => (cmd_diag, "", "show status and list all break and watch points"),
    ".edit" => (
        cmd_edit,
        "",
        "run ENV[\"EDITOR\"] on local program and reload afterwards (see .load and .save)",
        ".e",
    ),
    ".help" => (cmd_help, "", "show documentation on REPL directives", "."),
    ".list" => (cmd_list, "", "list the current program", ".l"),
    ".load" => (cmd_load, "FILE", "load FILE as local program (see .edit and .save)"),
    ".next" => (
        cmd_next,
        "[LOCATION]",
        "perform one step (optiaonlly from LOCATION) -- if it's a JSR, break upon return",
        ".n",
    ),
    ".prg" => (cmd_prg, "", "Show assembler output (if available)"),
    ".run" => (
        cmd_run,
        "[LOCATION]",
        "assemble local program and continue (optionally from LOCATION)",
        ".r",
    ),
    ".reset" =>
        (cmd_reset, "", "clear machine state so that the next .run will start fresh"),
    ".save" => (cmd_save, "FILE", "save local program to FILE (see .edit and .load)"),
    ".screen" => (cmd_screen, "", "show the screen"),
    ".set" => (cmd_set, "SETTING [VALUE]", "view SETTING or set it to VALUE"),
    ".show" => (cmd_show, "", "show the UI"),
    ".status" => (cmd_status, "", "print the current machine status"),
    ".step" =>
        (cmd_step, "[LOCATION]", "perform one step (optionally from LOCATION)", ".s"),
    ".watch" => (
        cmd_watch,
        "RANGE [EXPR]",
        "break whenever RANGE changes if optional EXPR is true",
        ".w",
    ),
]
const REPL_CMDS = Dict(
    Iterators.flatten(
        (length(d) == 3 ? [k => (k, d...)] : [k => (k, d...), last(d) => (k, d...)]) for
        (k, d) in DEFS
    ),
)
const DIRECTIVES = sort([keys(REPL_CMDS)..., dot_cmds...])
const ALL_CMDS = sort!([ASM_CMDS..., DIRECTIVES...])

println(ctx::Repl, args...) = println(ctx.screen, args...)
print(ctx::Repl, args...) = print(ctx.screen, args...)

function repl(specialization = Nothing)
    try
        ctx = Repl{specialization}()
        global _REPL = ctx
        ctx.asmlines = String[]
        ctx.asmlist = ""
        ctx.cmd_handler = handle_command
        ctx.pending_asm_prefix = ""
        ctx.pending_asm_expr = ""
        ctx.input_file = ""
        ctx.input_file_channel = nothing
        ctx.input_file_changed = false
        ctx.screen = Screen{specialization}(; diag = Ref(true), repl = ctx)
        ctx.banksettings = BankSettings()
        ctx.state = (;)
        ctx.runid = 1
        bind_keys(ctx.screen)
        local mistate = nothing
        local mode = nothing
        ctx.mode = initrepl(
            s -> Base.invokelatest(ctx.cmd_handler, ctx, s);
            prompt_text = "6502> ",
            #prompt_text = "",
            prompt_color = :light_yellow,
            start_key = '}',
            enter = function (s)
                mistate = Base.active_repl.mistate
                mode = LineEdit.mode(mistate)
                #println(ctx, "6502...")
            end,
            entered = s -> begin
                TUI.app(ctx.screen)
                REPL.LineEdit.transition(() -> nothing, mistate, mode)
            end,
            mode_name = "ASM_mode",
            completion_provider = Completer(),
        )
        #ctx.mode.on_done = (_...)->(println("DONE"); nothing)
        ctx.settings = Dict(pairs(DEFAULT_SETTINGS)...)
        ctx.dirty = false
        ctx
    catch err
        @info "Error initializing REPL" exception = (err, catch_backtrace())
    end
end

function takeover_repl(mistate, mode, screen, s::LineEdit.MIState)
    TUI.app(screen)
    REPL.LineEdit.transition(() -> nothing, mistate, mode)
end

function LineEdit.complete_line(::Repl, state)
    local str = LineEdit.input_string(state)
    return [dir for dir in ALL_CMDS if startswith(dir, str)], str, true
end

function handle_command(ctx::Repl, line)
    log("Handle command: $line")
    if matches(COMMENT_PAT, line)
        push!(ctx.asmlines, line)
        return nothing
    end
    local m = match(DIRECTIVE_PAT, line)
    local toks = [t for t in something(m, []) if !isnothing(t) && !isempty(t)]
    local label, cmd = if length(toks) == 2
        toks
    else
        nothing, isempty(toks) ? nothing : toks[1]
    end
    if isnothing(cmd) || string(cmd) ∉ ALL_CMDS
        isnothing(m) && println(ctx, "NOT A COMMAND")
        !isnothing(m) && println(ctx, "UNRECOGNIZED COMMAND $line")
        println(
            ctx,
            "ASM REPL expected an assembly instruction or a directive: $(join([DIRECTIVES..., dot_cmds...], ", "))",
        )
        return nothing
    end
    local prefix = line[1:cmd.offset+length(cmd)]
    local body = line[length(prefix)+1:end]
    if haskey(REPL_CMDS, cmd)
        println(ctx, "REPL CMD: $cmd")
        local (cmd, func) = REPL_CMDS[cmd]
        func(ctx, cmd, strip(body))
    else
        cmd_assemble(ctx, label, cmd, prefix, body, line)
    end
    nothing
end

function handle_asm_chunk(ctx::Repl, line)
    ctx.pending_asm_expr *= line * '\n'
    if !isincomplete(ctx.pending_asm_expr)
        println(ctx, "$(ctx.pending_asm_expr) is complete")
        push!(
            ctx.asmlines,
            string.(split(ctx.pending_asm_prefix * ctx.pending_asm_expr, "\n"))...,
        )
        ctx.dirty = true
        ctx.pending_asm_expr = ""
        ctx.pending_asm_prefix = ""
        ctx.cmd_handler = handle_command
        ctx.mode.prompt = "6502> "
    end
    nothing
end

function cmd_asm(ctx::Repl, cmd, args)
    !ctx.dirty && return
    if !isdefined(ctx, :worker)
        println(ctx, "ADDING 6502 WORKER...")
        ctx.worker = add_worker()
        println(ctx, "DONE")
    else
        Workers.clear(ctx.worker)
    end
    mktemp() do path, io
        for line in ctx.asmlines
            println(io, line)
        end
        close(io)
        ctx.labels, ctx.asmlist = Workers.asmfile(ctx.worker, path)
    end
    ctx.dirty = false
end

function cmd_prg(ctx::Repl, cmd, args)
    println(ctx, ctx.asmlist)
end

function cmd_break(ctx::Repl, cmd, args)
    println(ctx, "BREAK $args")
end

function cmd_clear(ctx::Repl, cmd, args)
    empty!(ctx.asmlines)
    ctx.dirty = true
    println(ctx, "cleared program")
end

function cmd_del(ctx::Repl, cmd, args)
    println(ctx, "DEL $args")
end

function cmd_diag(ctx::Repl, cmd, args)
    println(ctx, "DIAG $args")
end

function cmd_edit(ctx::Repl, cmd, args)
    println(ctx, "EDIT $args")
end

function cmd_help(ctx::Repl, cmd, args)
    local lines = ["COMMAND               ALIAS  DESCRIPTION"]

    for (cmd, def) in DEFS
        push!(
            lines,
            @sprintf "%-23s%-5s%s" cmd * " " * def[2] (length(def) == 3 ? "" : def[4]) def[3]
        )
    end
    push!(lines, "CTRL-C                      pause current execution")
    println(ctx, join(lines, "\n"))
end

function cmd_list(ctx::Repl, _, _)
    println(ctx, join(ctx.asmlines, "\n"))
end

cmderror(msg) = @error msg _module = Main _file = "REPL" _line = "1"

function cmd_load(ctx::Repl, cmd, args)
    println(ctx, "LOAD $args")
    local nowatch, file = match(LOAD_PAT, args)
    println(ctx, "nowatch: $(!isempty(something(nowatch, ""))), file: $file")
    isempty(something(file, "")) && isempty(last_load) && return cmderror("No file to load")
    load_file(ctx, file)
end

function load_file(ctx::Repl, path)
    !isnothing(path) && !isfile(path) && return cmderror("No file $path")
    path = realpath(path)
    try
        ctx.asmlines = readlines(path)
        ctx.dirty = true
        if !isempty(ctx.input_file) && ctx.input_file != path
            stop_watching(ctx)
        end
        if isnothing(ctx.input_file_channel)
            start_watching(ctx, path)
        end
    catch
        cmderror("Could not read file $path")
    end
end

function start_watching(ctx::Repl, file::AbstractString)
    ctx.input_file = string(file)
    ctx.input_file_channel = Channel{Nothing}()
    @async begin
        println(ctx, "WATCHING $file FOR CHANGES")
        while true
            if isready(ctx.input_file_channel)
                take!(ctx.input_file_channel)
                close(ctx.input_file_channel)
                ctx.input_file_channel = nothing
                ctx.input_file = nothing
                break
            end
            local change = watch_file(ctx.input_file, 1)
            if !change.timedout
                input_file_changed(ctx, change)
            end
        end
    end
end

function input_file_changed(ctx::Repl, change)
    !change.changed && return
    println(ctx, "$(ctx.input_file) changed, reloading...")
    load_file(ctx, ctx.input_file)
end

function stop_watching(ctx::Repl)
    !isnothing(ctx.input_file_channel) && put!(ctx.input_file_channel, nothing)
end

function cmd_save(ctx::Repl, cmd, args)
    isempty(ctx.asmlines) && return cmderror("No program in memory")
    local force, file = match(LOAD_PAT, args)
    isempty(something(file, "")) && isempty(last_load)
    return cmderror("No file to save to")
    local path = realpath(file)
    !isdir(dirname(path)) && error("No directory $(dirname(path))")
    isfile(path) && isempty(something(force, "")) && error("File $path already exists")
    open(path, "w") do io
        write(io, join(ctx.asmlines, "\n"))
    end
    println(ctx, "Wrote program to $path")
end

function cmd_set(ctx::Repl, cmd, args)
    println(ctx, "SET $args")
end

function cmd_next(ctx::Repl, cmd, args)
    println(ctx, "NEXT $args")
end

function cmd_run(ctx::Repl, cmd, args)
    cmd_asm(ctx, cmd, args)
    ctx.state = Workers.exec(
        ctx.worker,
        :main;
        tickcount = ctx.settings[:maxticks],
        bs = ctx.banksettings,
    )
    ctx.runid += 1
end

function cmd_reset(ctx::Repl, cmd, args)
    println(ctx, "RESET $args")
end

function cmd_screen(ctx::Repl, cmd, args)
    if !isdefined(ctx, :worker)
        println(ctx, "No program running")
        return
    end
    display_chars(ctx, @view ctx.worker.memory[intrange(screen)]) do c
        C64.SCREEN_CODES[c+1]
    end
end

function cmd_show(ctx::Repl, cmd, args)
    println(ctx, "SHOW $args")
end

function cmd_status(ctx::Repl, cmd, args)
    println(ctx, "STATUS $args")
end

function cmd_step(ctx::Repl, cmd, args)
    println(ctx, "STEP $args")
end

function cmd_watch(ctx::Repl, cmd, args)
    println(ctx, "WATCH $args")
end

function cmd_assemble(ctx::Repl, label, cmd, prefix, expr, line)
    println(ctx, "ASM CMD: [$label] [$cmd] / [$prefix] [$expr]")
    if isincomplete(expr)
        ctx.pending_asm_prefix = prefix
        ctx.pending_asm_expr = expr * '\n'
        ctx.cmd_handler = handle_asm_chunk
        ctx.mode.prompt = "6502...> "
    else
        push!(ctx.asmlines, line)
        ctx.dirty = true
    end
end

end
