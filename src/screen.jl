using TerminalUserInterfaces:
    TextWrap,
    Word,
    right,
    bottom,
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
    BOX_DRAWINGS_LIGHT_ARC_UP_AND_LEFT,
    previous_buffer
using .TextWrap
using ..Fake6502: C64, Workers, screen, char_defs
using .C64: C64_PALETTE, BG0, COLOR_MEM
using .Workers: updatememory
using Mmap: sync!
using Base64

import TerminalUserInterfaces:
    vertical, horizontal, top_left, top_right, bottom_left, bottom_right, app

const GSTART = "\e_G"
const GEND = "\e\\"
const RETURN_RIGHT = "⮑"
const NEWLINE_RIGHT = "⮓"
const MOD_NAMES = (;
    SHIFT = "Shift",
    CONTROL = "Ctrl",
    ALT = "Alt",
)
const KEYMAP = Dict{String,Function}()

setcursor(scr::Screen, x::Int) = scr.cursor[] = x

function showcursor(scr::Screen)
    local xoff = scr.cursor[]
    local area = scr.replarea[]
    local yoff = sum([area.y, length.(scr.wrappedlines[1:end-1])...])

    for line in scr.wrappedlines[end]
        # eol is length(line) + 1
        if length(line) + 1 < xoff
            xoff -= length(line)
            yoff += 1
        end
    end
    move_cursor(TERMINAL[], yoff, area.x + xoff - 1)
end

setscreenloc(scr::Screen, y, x) = scr.screenloc[] = UInt16.((y, x))

function render(scr::Screen, area::Rect, buffer::Buffer)
    set(buffer, area, screen_style)
    render(scr.layout, area, buffer)
    !scr.diag[] &&
        return
    local evt = scr.lastevt[]
    local mod = TUI.keymodifier(evt)
    local key = TUI.keycode(evt)
    local keycode = join((v for (k,v) in pairs(MOD_NAMES) if string(k) ∈ mod), "-")
    local col = scr.cursor[]
    setstatus(scr, "[$col] $keycode $mod $key $(evt.data.kind) $evt $(scr.lastcmd[])")
end

const LTR = BorderLeft | BorderTop | BorderRight
const LTRB = LTR | BorderBottom

vertical(::Union{BorderType{:TOP},BorderType{:MID},BorderType{:BOT}}) =
    BOX_DRAWINGS_LIGHT_VERTICAL
horizontal(::Union{BorderType{:TOP},BorderType{:MID},BorderType{:BOT}}) =
    BOX_DRAWINGS_LIGHT_HORIZONTAL

top_left(::Union{BorderType{:MID},BorderType{:BOT}}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
top_right(::Union{BorderType{:MID},BorderType{:BOT}}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT

bottom_left(::Union{BorderType{:TOP},BorderType{:MID}}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
bottom_right(::Union{BorderType{:TOP},BorderType{:MID}}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
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
    global border_style
    local block = Block(;
        title,
        title_style = border_style,
        border,
        border_type,
        border_style,
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

function configure(scr::Screen; regs = scr.showcpu[], monitor = regs, c64screen = regs, repl = true)
    local bordertype = :TOP
    function bt()
        local t = bordertype
        bordertype = :MID
        return BorderType{t}()
    end
    TUI.put("$(GSTART)a=d,d=a$GEND")
    empty!(scr.layout.widgets)
    empty!(scr.layout.constraints)
    if regs
        push!(
            scr.layout.widgets, screenarea(; title = "Registers", border_type = bt()) do area, buf
                set(buf, area, ["C64 REGISTERS"])
            end
        )
        push!(scr.layout.constraints, Length(2))
    end
    if monitor
        push!(scr.layout.widgets, screenarea(scr.monitor; title = "Monitor", border_type = bt()))
        push!(scr.layout.constraints, Length(9))
    end
    if c64screen
        push!(scr.layout.widgets, screenarea(; title = "Screen", border_type = bt()) do area, buf
            setscreenloc(scr, area.y, area.x)
            set(buf, area, split("this\nis\nthe\nscreen", '\n'))
        end)
        push!(scr.layout.constraints, Length(27))
    end
    if repl
        push!(scr.layout.widgets, screenarea(; title = "Repl", border_type = bt()) do area, buf
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

cursorend(scr::Screen) = setcursor(scr, length(scr.repllines[end]) + 1)

function drawrepl(scr::Screen, area::Rect, buf::Buffer)
    if scr.replarea != area
        local oldy = scr.replarea[].y
        scr.replarea[] = area
        empty!(scr.wrappedlines)
        for line in scr.repllines
            wrapline(scr, line)
        end
    end
    trimlines(scr)
    scr.cursor[] == 0 &&
        cursorend(scr)
    set(buf, area, [Iterators.flatten(scr.wrappedlines)...])
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
    
    keycode = isempty(keycode) ? key : keycode * "-" * key
    scr.lastevt[] = evt
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
            evt = TUI.try_get_event(t; wait = 1/60)
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

function TUI.draw(t::CrosstermTerminal, scr::Screen)
    TUI.draw(t)
    scr.cursor[] == 0 &&
        return
    if scr.showcpu[] && scr.dirty[]
        drawscreen(scr)
        move_cursor(TERMINAL[], scr.screenloc[]...)
        local screenfile = base64encode(scr.screenfile[1])
        local cmd = "$(GSTART)a=T,f=24,t=f,s=$(40 * 8),v=$(25 * 8),q=2;$screenfile$GEND"

        log("OUTPUT $(length(scr.image)) bytes: $cmd")
        #TUI.put("\e[80C$(GSTART)a=T,i=1,f=24,t=f,s=$(40 * 8),v=$(25 * 8),q=2;$screenfile$GEND")
        #TUI.put("$(GSTART)a=T,i=1,f=24,t=f,s=$(40 * 8),v=$(25 * 8),q=2;$screenfile$GEND")
        #TUI.put("$(GSTART)a=T,f=24,t=f,s=$(40 * 8),v=$(25 * 8),q=2;$screenfile$GEND")
        TUI.put(cmd)
    end
    showcursor(scr)
end

function drawscreen(scr::Screen)
    local image = scr.image
    local mem = scr.repl.worker.memory
    local screenmem = @view mem[intrange(screen)]
    local chardefs = @view mem[intrange(char_defs)]

    for col = 0:39
        for row = 0:24
            i = row * 40 + col
            char = screenmem[1+i]
            for pixel = 0:7
                x = col * 8 + pixel
                bg = C64_PALETTE[1+mem[BG0.value]]
                fg = C64_PALETTE[1+mem[COLOR_MEM.value + i]]
                for rowbyte = 0:7
                    y = row * 8 + rowbyte
                    offset = y * 320 * 3 + x * 3
                    pixels = chardefs[1+8*char+rowbyte]
                    color = (pixels >> (7 - pixel)) & 1 == 1 ? fg : bg
                    image[offset + 1] = color[1]
                    image[offset + 2] = color[2]
                    image[offset + 3] = color[3]
                end
            end
        end
    end
    sync!(image)
end

function delete_image(t::CrosstermTerminal, id::String)
    id == "" &&
        return
    TUI.put("$(GSTART)a=d,d=i,i=$id$GEND")
end

function trimlines(scr::Screen)
    local height = scr.replarea[].height
    local lines = scr.wrappedlines
    local overage = sum(length.(scr.wrappedlines)) - height

    if overage > 0
        local o = overage
        while length(lines[1]) <= o
            o -= length(lines[1])
            splice!(lines, 1, [])
        end
        o > 0 && splice!(lines[1], o, [])
    end
end

function rewrap(scr::Screen)
    pop!(scr.wrappedlines)
    wrapline(scr)
end

function wrapline(scr::Screen, line::String = scr.repllines[end])
    local wrapped = split(wrap(line; width=scr.replarea[].width, replace_whitespace=false), "\n")

    push!(scr.wrappedlines, wrapped)
end

function key_self_insert(scr::Screen, evt::TUI.KeyEvent, x)
    local line = scr.repllines[end]
    scr.repllines[end] = line[1:x-1] * TUI.keycode(evt) * line[x:end]
    setcursor(scr, x + 1)
    rewrap(scr)
end

function key_accept(scr::Screen, ::TUI.KeyEvent, x)
    push!(scr.repllines, "")
    push!(scr.wrappedlines, [""])
    trimlines(scr)
    cursorend(scr)
    scr.lastcmd[] = scr.repllines[end - 1]
    handle_command(scr.repl, scr.repllines[end - 1])
end

function key_forward(scr::Screen, ::TUI.KeyEvent, x)
    setcursor(scr, min(length(scr.repllines[end]), x) + 1)
end

function key_back(scr::Screen, ::TUI.KeyEvent, x)
    setcursor(scr, max(1, x - 1))
end

function key_start_line(scr::Screen, ::TUI.KeyEvent, x)
    setcursor(scr, 1)
end

function key_end_line(scr::Screen, ::TUI.KeyEvent, x)
    setcursor(scr, length(scr.repllines[end]) + 1)
end

function key_backspace(scr::Screen, ::TUI.KeyEvent, x)
    local line = scr.repllines[end]
    x -= 1
    local off = x - scr.replarea[].x + 1

    if off < 1
        scr.quit[] = true
    else
        setcursor(scr, x)
        if length(line) >= off
            scr.repllines[end] = line[1:x-1] * line[x+1:end]
            rewrap(scr)
        end
    end
end

function key_kill(scr::Screen, ::TUI.KeyEvent, x)
    local line = scr.repllines[end]
    local off = x - scr.replarea[].x + 1

    if length(line) >= off
        scr.repllines[end] = line[1:off - 1]
        rewrap(scr)
    end
end

function key_quit(scr::Screen, ::TUI.KeyEvent, x)
    scr.quit[] = true
end

function key_layout(scr::Screen, ::TUI.KeyEvent, x)
    scr.showcpu[] = !scr.showcpu[]
    configure(scr)
end

function key_refresh(::Screen, ::TUI.KeyEvent, x)
    TerminalUserInterfaces.reset(previous_buffer())
end

function bind_keys(::Screen)
    for c in filter(isprint, Char(0):Char(255))
        KEYMAP[string(c)] = key_self_insert
        KEYMAP[string(uppercase(c))] = key_self_insert
    end
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
        "Ctrl-l" => key_layout,
        "Ctrl-r" => key_refresh,
    ))
end

println(scr::Screen, args...) = print(scr, args..., "\n")

function print(scr::Screen, args...)
    scr.cursor[] == 0 &&
        error("Attempt to print to an uninitialized screen")
    local content = sprint() do io
        print(io, args...)
    end
    log("PRINT $content")
    local lines = split(content, "\n")
    scr.repllines[end] *= lines[1]
    rewrap(scr)
    for line in lines[2:end]
        push!(scr.repllines, line)
        wrapline(scr)
    end
    trimlines(scr)
    cursorend(scr)
end
