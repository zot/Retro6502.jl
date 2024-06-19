using Unicode, SharedArrays
using TerminalUserInterfaces:
    TextWrap,
    Layout,
    Paragraph,
    Word,
    make_words,
    Buffer,
    Rect,
    right,
    bottom,
    Block,
    Constraint,
    Length,
    Min,
    inner,
    BorderTypeArc,
    BorderTop,
    BorderLeft,
    BorderRight,
    BorderBottom,
    TerminalBackend,
    BorderType,
    BOX_DRAWINGS_LIGHT_VERTICAL,
    BOX_DRAWINGS_LIGHT_HORIZONTAL,
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT,
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT,
    BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT,
    BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT,
    BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT,
    BOX_DRAWINGS_LIGHT_ARC_UP_AND_LEFT
using .TextWrap

import TerminalUserInterfaces:
    vertical, horizontal, top_left, top_right, bottom_left, bottom_right, app
using TerminalUserInterfaces.Crayons

const GSTART = "\e_G"
const GEND = "\e\\"
const screen_style = Crayon(; foreground = color2int(FG), background = color2int(BG))
const RETURN_RIGHT = "⮑"
const NEWLINE_RIGHT = "⮓"
const MOD_NAMES = (;
    SHIFT = "Shift",
    CONTROL = "Ctrl",
    ALT = "Alt",
)
const KEYMAP = Dict{String,Function}()

"""
Screen structure, parameterized to allow customization
"""
@kwdef struct Screen{T} <: TUI.Model
    regs::Ref{String} = Ref("")
    monitor::Paragraph =
        Paragraph(Block(; title = "", border = 0), make_words("C64 state", screen_style), 1)
    #repl::Page = Page()
    status::Ref{AbstractString} = "CTRL-Q: QUIT"
    layout::Layout = Layout(; widgets = [], constraints = Constraint[])
    quit::Ref{Bool} = Ref(false)
    cursor::Ref{Tuple{UInt16,UInt16}} = Ref((0x0000, 0x0000)) # row, col
    screenloc::Ref{Tuple{UInt16,UInt16}} = Ref((0x0000, 0x0000)) # row, col
    image::SharedVector{UInt8} = SharedVector(fill(0xFF, 40 * 8 * 25 * 8 * 3))
    repllines::Vector{String} = ["READY!", "asdf"]
    wrappedlines::Vector{Vector{String}} = [["READY!"], ["asdf"]]
    screenwidth::Ref{Int} = Ref(0)
    replarea::Ref{Rect} = Ref(Rect())
    diag::Ref{Bool} = Ref(false)
    context::ReplContext
end

setcursor(scr::Screen, y, x) = scr.cursor[] = UInt16.((y, x))

setscreenloc(scr::Screen, y, x) = scr.screenloc[] = UInt16.((y, x))

function render(scr::Screen, area::Rect, buffer::Buffer)
    scr.screenwidth[] = area.width
    set(buffer, area, screen_style)
    render(scr.layout, area, buffer)
    move_cursor(TERMINAL[], scr.cursor[]...)
end

const LTR = BorderLeft | BorderTop | BorderRight
const LTRB = LTR | BorderBottom

log(msg) =
    open("/tmp/log", "a") do io
        println(io, msg)
    end

vertical(::Union{BorderType{:TOP},BorderType{:MID},BorderType{:BOT}}) =
    BOX_DRAWINGS_LIGHT_VERTICAL
horizontal(::Union{BorderType{:TOP},BorderType{:MID},BorderType{:BOT}}) =
    BOX_DRAWINGS_LIGHT_HORIZONTAL
top_left(::Union{BorderType{:MID},BorderType{:BOT}}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
top_right(::Union{BorderType{:MID},BorderType{:BOT}}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
bottom_left(::Union{BorderType{:TOP},BorderType{:MID}}) =
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
bottom_right(::Union{BorderType{:TOP},BorderType{:MID}}) =
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT

top_left(::BorderType{:TOP}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT
top_right(::BorderType{:TOP}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT

bottom_left(::BorderType{:BOT}) = BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT
bottom_right(::BorderType{:BOT}) = BOX_DRAWINGS_LIGHT_ARC_UP_AND_LEFT

function screenarea(widget; title = "", border = LTR, border_type = BorderType{:MID}())
    screenarea(; title, border, border_type) do area, buffer
        render(widget, area, buffer)
    end
end

function screenarea(
    func::Function;
    title = "",
    border = LTR,
    border_type = BorderType{:MID}(),
)
    local block = Block(;
        title,
        border,
        border_type,
        border_style = Crayon(; foreground = color2int(BORDER)),
    )

    FWidget() do area, buffer
        func(inner(block, area), buffer)
        render(block, area, buffer)
    end
end

set(buffer::Buffer, area::Rect, line::AbstractString, style = nothing) = set(buffer, area, [line], style)

function set(
    buffer::Buffer,
    area::Rect,
    lines::Vector{T},
    style = nothing,
) where {T<:AbstractString}
    local row = area.y
    for line in lines
        local chars = [graphemes(line)...]
        for i = 1:min(area.width, length(chars))
            local col = area.x - 1 + i
            local s = !isnothing(style) ? style : buffer.content[row, col].style
            set(buffer, col, row, chars[i], s)
        end
        row += 1
    end
end

function configure(scr::Screen; regs = true, monitor = true, c64screen = true, repl = true)
    empty!(scr.layout.widgets)
    empty!(scr.layout.constraints)
    setcursor(scr, 0x0000, 0x0000)
    setscreenloc(scr, 0x0000, 0x0000)
    if regs
        push!(
            scr.layout.widgets, screenarea(; title = "Registers", border_type = BorderType{:TOP}()) do area, buf
                set(buf, area, ["C64 REGISTERS"])
            end
        )
        push!(scr.layout.constraints, Length(2))
    end
    if monitor
        push!(scr.layout.widgets, screenarea(scr.monitor; title = "Monitor"))
        push!(scr.layout.constraints, Length(9))
    end
    if c64screen
        push!(scr.layout.widgets, screenarea(; title = "Screen") do area, buf
            setscreenloc(scr, area.y, area.x)
            set(buf, area, split("this\nis\nthe\nscreen", '\n'))
        end)
        push!(scr.layout.constraints, Length(27))
    end
    if repl
    #push!(scr.layout.widgets, screenarea(scr.repl; title = "Repl"))
        push!(scr.layout.widgets, screenarea(; title = "Repl") do area, buf
            drawrepl(scr, area, buf)
        end)
        push!(scr.layout.constraints, Min(2))
    end
    push!(
        scr.layout.widgets,
        screenarea(
            title = "Status",
            border = LTRB,
            border_type = BorderType{:BOT}(),
        ) do area, buf
            drawstatus(scr, area, buf)
        end,
    )
    push!(scr.layout.constraints, Length(3))
end

function cursorend(scr::Screen)
    local lines = [Iterators.flatten(scr.wrappedlines)...]
    local area = scr.replarea[]

    setcursor(scr, min(bottom(area), area.y + length(lines) - 1), area.x + length(lines[end]))
end

function drawrepl(scr::Screen, area::Rect, buf::Buffer)
    local lines = [Iterators.flatten(scr.wrappedlines)...]

    scr.replarea[] = area
    if scr.cursor[] == (0x0000, 0x0000)
        cursorend(scr)
    end
    set(buf, area, lines)
end

function drawstatus(scr::Screen, area::Rect, buffer::Buffer)
    set(buffer, area, split(scr.status[], '\n'))
end

TUI.should_quit(scr::Screen) = scr.quit[]

function TUI.init!(scr::Screen, ::TerminalBackend)
    configure(scr)
    scr.quit[] = false
    show_cursor(TERMINAL[])
end

TUI.view(scr::Screen) = scr

function TUI.update!(scr::Screen, evt::TUI.KeyEvent)
    local mod = TUI.keymodifier(evt)
    local key = TUI.keycode(evt)
    local keycode = join((v for (k,v) in pairs(MOD_NAMES) if string(k) ∈ mod), "-")
    local row, col = scr.cursor[]
    
    keycode = isempty(keycode) ? key : keycode * "-" * key
    scr.diag[] && setstatus(scr, "[$row,$col] $keycode $mod $key $(evt.data.kind) $evt")
    evt.data.kind ∉ ["Press", "Repeat"] &&
        return
    get(KEYMAP, keycode, (a...)->nothing)(scr, evt, scr.cursor[]...)
end

function setstatus(scr::Screen, msg...)
    scr.status[] = "CTRL-Q: QUIT $(string(msg...))"
end

function app(m::Screen; wait = 1 / 30)
    tui() do
        @debug "Creating terminal"
        t = TUI.Terminal(; wait)
        TUI.init!(m, t)
        log("STARTING APP")
        while !should_quit(m)
            @debug "Getting event"
            evt = TUI.try_get_event(t)
            !isnothing(evt) && @debug "Got event" event = evt
            @debug "Updating model"
            TUI.update!(m, evt)
            @debug "Rendering model"
            TUI.render(t, m)
            @debug "Drawing model"
            TUI.draw(t, m)
        end
        @debug "End"
    end
end

TUI.draw(t::CrosstermTerminal, ::Any) = TUI.draw(t)

function TUI.draw(t::CrosstermTerminal, screen::Screen)
    TUI.draw(t)
    screen.cursor[] == (0x0000, 0x0000) &&
        return
    TUI.save_cursor(t)
    move_cursor(TERMINAL[], screen.cursor[]...)
    log("OUTPUT $(length(screen.image)) bytes: $(GSTART)f=24,t=s,s=$(40 * 8),v=$(25 * 8),q=2;$(screen.image.segname[2:end])$GEND")
    #TUI.put("$(GSTART)f=24,t=s,s=$(40 * 8),v=$(25 * 8);$(screen.image.segname)$GEND")
    TUI.put("\e[80C$(GSTART)f=24,t=s,s=$(40 * 8),v=$(25 * 8),q=2;$(screen.image.segname[2:end])$GEND")
    TUI.restore_cursor(t)
end

function trimlines(scr::Screen)
    local height = scr.replarea[].height
    local lines = scr.wrappedlines

    while sum(length.(scr.wrappedlines)) > height
        splice!(length(lines[1]) > 1 ? lines[1] : lines, 1, [])
    end
end

function key_self_insert(scr::Screen, evt::TUI.KeyEvent, y, x)
    scr.repllines[end] *= TUI.keypress(evt)
    scr.wrappedlines[end] = split(wrap(scr.repllines[end]; width=scr.screenwidth[], replace_whitespace=false), "\n")
    trimlines(scr)
    cursorend(scr)
end

function key_accept(scr::Screen, ::TUI.KeyEvent, y, x)
    push!(scr.repllines, "")
    push!(scr.wrappedlines, [""])
    local oldlen = sum(length.(scr.wrappedlines))
    trimlines(scr)
    if oldlen == sum(length.(scr.wrappedlines))
        y += 1
    end
    setcursor(scr, y, scr.replarea[].x)
    handle_command(scr.context, scr.repllines[end - 1])
end

function key_forward(scr::Screen, ::TUI.KeyEvent, y, x)
    setcursor(scr, y, min(scr.replarea[].x + length(scr.repllines[end]), x + 1))
end

function key_back(scr::Screen, ::TUI.KeyEvent, y, x)
    setcursor(scr, y, max(scr.replarea[].x, x - 1))
end

function key_start_line(scr::Screen, ::TUI.KeyEvent, y, x)
    setcursor(scr, y, scr.replarea[].x)
end

function key_end_line(scr::Screen, ::TUI.KeyEvent, y, x)
    local line = scr.repllines[end]

    setcursor(scr, y, min(right(scr.replarea[]), scr.replarea[].x + length(line)))
end

function key_backspace(scr::Screen, ::TUI.KeyEvent, y, x)
    local line = scr.repllines[end]
    x -= 1
    local off = x - scr.replarea[].x + 1

    if off < 1
        scr.quit[] = true
    else
        setcursor(scr, y, x)
        if length(line) >= off
            scr.repllines[end] = line[1:off - 1] * line[off + 1:end]
            scr.wrappedlines[end] = split(wrap(scr.repllines[end]; width=scr.screenwidth[], replace_whitespace=false), "\n")
            trimlines(scr)
        end
    end
end

function key_kill(scr::Screen, ::TUI.KeyEvent, y, x)
    local line = scr.repllines[end]
    local off = x - scr.replarea[].x + 1

    if length(line) >= off
        scr.repllines[end] = line[1:off - 1]
        scr.wrappedlines[end] = split(wrap(scr.repllines[end]; width=scr.screenwidth[], replace_whitespace=false), "\n")
        trimlines(scr)
    end
end

function key_quit(scr::Screen, ::TUI.KeyEvent, y, x)
    scr.quit[] = true
end

function bind_keys(::Screen)
    for c in 'a':'z'
        KEYMAP[string(c)] = key_self_insert
        KEYMAP[string(uppercase(c))] = key_self_insert
    end
    KEYMAP[" "] = key_self_insert
    merge!(KEYMAP, Dict(
        "Enter" => key_accept,
        "Home" => key_start_line,
        "Ctrl-a" => key_start_line,
        "End" => key_end_line,
        "Ctrl-e" => key_end_line,
        "Left" => key_back,
        "Ctrl-b" => key_back,
        "Right" => key_forward,
        "Ctrl-f" => key_forward,
        "Backspace" => key_backspace,
        "Ctrl-k" => key_kill,
        "Ctrl-q" => key_quit,
    ))
end
