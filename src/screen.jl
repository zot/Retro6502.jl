using Revise
using TerminalUserInterfaces:
    TextWrap,
    Word,
    Rect,
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
    BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL,
    BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL,
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT,
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT,
    BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT,
    BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT,
    BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT,
    BOX_DRAWINGS_LIGHT_ARC_UP_AND_LEFT,
    Cell,
    current_buffer,
    previous_buffer
using TerminalUserInterfaces.Crossterm.LibCrossterm.libcrossterm_jll
using .TextWrap
using ..Fake6502: C64, Workers, Fake6502m, screen, char_defs, status, screenmem, charmem, ASCII_TO_SCREEN,
    A, screen2ascii, rhex
using .C64: C64_PALETTE, BG0, COLOR_MEM, KEYBOARD_INPUTS, KEY_CODES, UNUSED_KEYS, ASCII_TO_KEY_CODE,
    SHIFT, CTRL, SHIFTED, ASCII_CODES
using .Workers: OFFSET_KEY, OFFSET_CHR
using Mmap: sync!
using Base64

import TerminalUserInterfaces:
    vertical,
    horizontal,
    top_left,
    top_right,
    bottom_left,
    bottom_right,
    left,
    right,
    top,
    bottom,
    app

const GSTART = "\e_G"
const GEND = "\e\\"
const RETURN_RIGHT = "⮑"
const NEWLINE_RIGHT = "⮓"
const MOD_NAMES = (; SHIFT = "Shift", CONTROL = "Ctrl", ALT = "Alt")
const KEYMAP = Dict{String,Function}()
const MODES = (:repl, :screenchars, :screenbytes)

setcursor(scr::Screen, x::Int) = scr.cursor[] = x

function showcursor(scr::Screen)
    local xoff = scr.cursor[]
    local area = scr.areas[:repl]
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
    !scr.diag[] && return
    local evt = scr.lastevt[]
    local mod = evt isa TUI.KeyEvent ? TUI.keymodifier(evt) : []
    local key = evt isa TUI.KeyEvent ? TUI.keycode(evt) : ""
    local keycode = join((v for (k, v) in pairs(MOD_NAMES) if string(k) ∈ mod), "-")
    local col = scr.cursor[]
    local c64key = scr.mouse_area[] === :screen ? scr.repl.worker.memory[OFFSET_KEY] : 0xFF
    setstatus(
        scr,
        "[$col] $c64key $keycode $mod $key[$(typeof(key))] $(evt.data.kind) $evt $(scr.lastcmd[]) {$(scr.mouse_area)}",
    )
end

const LT = BorderLeft | BorderTop
const TR = BorderTop | BorderRight
const LR = BorderLeft | BorderRight
const LTR = LT | TR
const LTB = LT | BorderBottom
const LTRB = LTR | BorderBottom

vertical(
    ::Union{
        BorderType{:TOP},
        BorderType{:TR},
        BorderType{:TL},
        BorderType{:MID},
        BorderType{:BOT},
    },
) = BOX_DRAWINGS_LIGHT_VERTICAL
horizontal(
    ::Union{
        BorderType{:TOP},
        BorderType{:TR},
        BorderType{:TL},
        BorderType{:MID},
        BorderType{:BOT},
    },
) = BOX_DRAWINGS_LIGHT_HORIZONTAL

top_left(::BorderType{:MID}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
top_right(::BorderType{:MID}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
bottom_left(::BorderType{:MID}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
bottom_right(::BorderType{:MID}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT

top_left(::BorderType{:TOP}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT
top_right(::BorderType{:TOP}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT
bottom_left(::BorderType{:TOP}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
bottom_right(::BorderType{:TOP}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT

top_left(::BorderType{:SCREEN}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT
top_right(::BorderType{:SCREEN}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT
bottom_left(::BorderType{:SCREEN}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
bottom_right(::BorderType{:SCREEN}) = BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL

top_left(::BorderType{:REPL_BORDER}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT

top_left(::BorderType{:SCREENCHARS}) = BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
top_right(::BorderType{:SCREENCHARS}) = BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
bottom_left(::BorderType{:SCREENCHARS}) = BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL
bottom_right(::BorderType{:SCREENCHARS}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT

top_left(::BorderType{:SCREENBYTES}) = BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
top_right(::BorderType{:SCREENBYTES}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT
bottom_left(::BorderType{:SCREENBYTES}) = BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL
bottom_right(::BorderType{:SCREENBYTES}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT

top_left(::BorderType{:REGISTERS}) = BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
top_right(::BorderType{:REGISTERS}) = BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT
bottom_left(::BorderType{:REGISTERS}) = BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL
bottom_right(::BorderType{:REGISTERS}) = BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL

top_left(::BorderType{:MONITOR}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
top_right(::BorderType{:MONITOR}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
bottom_left(::BorderType{:MONITOR}) = BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL
bottom_right(::BorderType{:MONITOR}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT

top_left(::BorderType{:TR}) = BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
top_right(::BorderType{:TR}) = BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL

top_right(::BorderType{:TL}) = BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL

top_left(::BorderType{:BOT}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
top_right(::BorderType{:BOT}) = BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
bottom_left(::BorderType{:BOT}) = BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT
bottom_right(::BorderType{:BOT}) = BOX_DRAWINGS_LIGHT_ARC_UP_AND_LEFT

is_raw_mode_enabled() = @ccall libcrossterm.crossterm_terminal_is_raw_mode_enabled()::Bool

function windowsize()
    local oldraw = is_raw_mode_enabled()
    !oldraw && Crossterm.raw_mode(true)
    Crossterm.print("\e[14t")
    Crossterm.flush()
    local reading = false
    local chars = Char[]
    while true
        local c = read(stdin, Char)
        if !reading
            if c == '\e'
                reading = true
            else
                continue
            end
        elseif c == 't'
            break
        end
        push!(chars, c)
    end
    local height, width =
        parse.(Ref(Int), match(r"^.*;([^;]+);([^t]+)$", String(chars)).captures)
    !oldraw && Crossterm.raw_mode(false)
    Crossterm.flush()
    local w, h = Crossterm.size()
    return (; width, height, rows = h, columns = w)
end

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
    adjust = false,
)
    global border_style
    local block =
        Ref(Block(; title, title_style = border_style, border, border_type, border_style))

    FWidget((; block)) do area, buffer
        if adjust
            area = Rect(area.x, area.y, area.width, area.height - 1)
        end
        func(inner(block[], area), buffer)
        render(block[], area, buffer)
    end
end

screenblock(; title = "", border = LTR, border_type = BorderType{:MID}()) =
    Block(; title, title_style = border_style, border, border_type, border_style)

screenblock(block::Block; title = block.title, title_style = block.title_style, border = block.border,
            border_type = block.border_type) =
    Block(; title, title_style, border, border_type, border_style)

set(buffer::Buffer, area::Rect, line::AbstractString, style = nothing) =
    set(buffer, area, [line], style)

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
        row > bottom(area) &&
            break
    end
end

function configure(
    scr::Screen;
    regs = scr.mode[] ∈ (:screenchars, :screenbytes),
    monitor = regs,
    c64screen = regs,
    repl = true,
)
    global border_style
    local bordertype = :TOP
    function bt(t = bordertype)
        bordertype = :MID
        return BorderType{t}()
    end

    set(current_buffer(), current_buffer().area, Cell(' ', screen_style))
    TUI.put("$(GSTART)a=d,d=a$GEND")
    scr.imageshown[] = false
    empty!(scr.layout.widgets)
    empty!(scr.layout.constraints)
    if c64screen
        local width, height, rows, columns = windowsize()
        local hpix = height / rows * 25
        local vpix = hpix * 320 / 200
        local scrwid = Int(ceil(vpix / width * columns))
        scr.screenwid[] = scrwid
        scr.lastscreen[] = UInt8[]
        scr.regsblock[] = screenarea(
            drawregs(scr);
            title = "Registers",
            border_type = bt(:REGISTERS),
            border = LTR,
        )
        push!(scr.layout.constraints, Length(27))
        push!(
            scr.layout.widgets,
            Layout(
                [
                    Layout(
                        [
                            screenarea(;
                                title = "Retro6502 Screen",
                                border_type = bt(:SCREEN),
                                border = LT,
                            ) do area, buf
                                scr.areas[:screen] = area
                                setscreenloc(scr, area.y, area.x)
                                set(buf, area, String[])
                            end,
                            Block(;
                                title = "Retro6502 Repl",
                                title_style = border_style,
                                border = LT,
                                border_type = bt(:REPL_BORDER),
                                border_style,
                            ),
                        ],
                        [Length(26), Length(1)],
                        :vertical,
                    ),
                    screenarea(drawcharacters(scr);
                               title = scr.mode[] == :screenchars ? "Characters" : "Bytes",
                               border_type = bt(scr.mode[] == :screenchars ? :SCREENCHARS : :SCREENBYTES),
                               border = LTRB,
                               adjust = true,
                               ),
                    (if scr.mode[] == :screenchars
                        [Layout(
                            [
                                scr.regsblock[],
                                screenarea(
                                    drawmonitor(scr);
                                    title = "Monitor",
                                    border_type = bt(:MONITOR),
                                    border = LTRB,
                                    adjust = true,
                                ),
                            ],
                            [Length(3), Min(1)],
                            :vertical,
                        )]
                    else
                        []
                    end)...,
                ],
                (if scr.mode[] == :screenchars
                    [Length(scrwid + 1), Length(41), Min(1)]
                else
                    [Length(scrwid + 1), Min(124)]
                end),
                :horizontal,
            ),
        )
    end
    if repl
        push!(scr.layout.constraints, Min(2))
        if c64screen
            push!(
                scr.layout.widgets,
                screenarea(; title = "", border_type = bt(), border = LR) do area, buf
                    drawrepl(scr, area, buf)
                end,
            )
        else
            push!(
                scr.layout.widgets,
                screenarea(; title = "Retro6502 Repl", border_type = bt()) do area, buf
                    drawrepl(scr, area, buf)
                end,
            )
        end
    end
    push!(scr.layout.constraints, Length(3))
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
end

cursorend(scr::Screen) = setcursor(scr, length(scr.repllines[end]) + 1)

drawregs(scr::Screen) = function (area::Rect, buf::Buffer)
    local running = scr.repl.state.running
    local block = scr.regsblock[].props.block
    if running && !contains(block[].title, "RUNNING") || !running && !contains(block[].title, "IDLE")
        block[] = screenblock(block[]; title = "Registers [$(running ? "RUNNING" : "IDLE")]")
    end
    scr.areas[:registers] = area
    local st = scr.repl.state
    local bs = scr.repl.banksettings
    local banks = sprint() do io
        bs.basic && print(io, "BASIC ")
        bs.chars && print(io, "CHARS ")
        bs.kernal && print(io, "KERNAL ")
        #@printf io "VIC #%d SCREEN 0x%04x CHAR 0x%04x" bs.vicbank >> 14 bs.scrmem bs.chrmem
    end
    set(
        buf,
        area,
        [
            (@sprintf "%-4s %-8s %-2s %-2s %-2s %-2s CLOCK                SLACK     BANKS" "PC" "Status" "A" "X" "Y" "SP"),
            (@sprintf "%04x %s %02x %02x %02x %02x %-20d %f  %s" st.pc status(st.status) st.a st.x st.y st.sp st.clockticks6502 st.slack banks),
        ],
    )
end

drawcharacters(scr::Screen) = function (area::Rect, buf::Buffer)
    local mem = scr.repl.worker.memory
    local wide = area.width >= 40 * 3 + 3
    
    for row in 0:24
        local rowoffset = scr.repl.banksettings.scrmem + row*40
        local line = @view mem[A(rowoffset) : A(rowoffset + 39)]
        if scr.mode[] == :screenchars
            local chars = String(screen2ascii.(line))

            set(buf, area.x, area.y + row, chars)
        else
            local offset = 0
            for col in 0:39
                if col > 0 && col % 10 == 0
                    offset += 1
                end
                local byte = line[col + 1]
                local chars = rhex(byte)
                local style = byte ∈ (0x00, UInt8(' ')) ? dim_style : screen_style

                set(buf, area.x + offset + col * (wide ? 3 : 2), area.y + row, chars, style)
            end
        end
    end
end

drawmonitor(scr::Screen) = function (area::Rect, buf::Buffer)
    local pc = scr.repl.state.pc
    local height = area.height
    local start =
        round(Int, min(0xFFFF - height * 16 + 1, max(0, floor(pc / 16.0 - 1) * 16)))
    local mem = scr.repl.worker.memory
    scr.areas[:monitor] = area
    for row = 0:area.height
        local offset = start + row * 16
        offset > 0xFFF0 &&
            break
        local values1 = join((@sprintf "%02x" mem[A(offset+i)] for i = 0:7), " ")
        local values2 = join((@sprintf "%02x" mem[A(offset+i)] for i = 8:15), " ")
        local haspc = offset <= pc <= offset + 15
        set(
            buf,
            Rect(; area.x, y = area.y + row, area.width, height = 1),
            [(@sprintf "%s%04x: %s   %s" (haspc ? "*" : " ") offset values1 values2)],
            screen_style,
        )
        if haspc
            local col = area.x + (pc - offset < 8 ? 7 : 9) + (pc - offset) * 3
            set(buf, col, area.y + row, pc_style)
            set(buf, col + 1, area.y + row, pc_style)
        end
    end
    local offset = 10 + 3 * 16
    area = Rect(; x = area.x + offset, area.y, width = area.width - offset, area.height)
end

function drawrepl(scr::Screen, area::Rect, buf::Buffer)
    if scr.areas[:repl] != area
        scr.areas[:repl] = area
        empty!(scr.wrappedlines)
        for line in scr.repllines
            wrapline(scr, line)
        end
    end
    trimlines(scr)
    scr.cursor[] == 0 && cursorend(scr)
    set(buf, area, [Iterators.flatten(scr.wrappedlines)...])
end

function drawstatus(scr::Screen, area::Rect, buffer::Buffer)
    scr.areas[:status] = area
    set(buffer, area, split(scr.status[], '\n'))
end

TUI.should_quit(scr::Screen) = scr.quit[]

function TUI.init!(scr::Screen, ::TerminalBackend)
    configure(scr)
    scr.quit[] = false
    show_cursor(TERMINAL[])
end

TUI.view(scr::Screen) = scr

function TUI.update!(scr::Screen, evt::TUI.MouseEvent)
    evt.data.kind !== "DownLeft" && return
    local old = scr.mouse_area[]
    scr.mouse_area[] = :unknown
    scr.lastevt[] = evt
    for (name, area) in scr.areas
        !(
            left(area) <= evt.data.column + 1 <= right(area) &&
            top(area) <= evt.data.row + 1 <= bottom(area)
        ) && continue
        scr.mouse_area[] = name
        if name === :screen
            Crossterm.hide()
        end
    end
    if old === :screen && scr.mouse_area != :screen
        Crossterm.show()
        showcursor(scr)
        scr.repl.worker.memory[OFFSET_KEY] = UNUSED_KEYS[1]
        scr.repl.worker.memory[OFFSET_CHR] = 0
    end
end

function TUI.update!(scr::Screen, evt::TUI.KeyEvent)
    local mod = TUI.keymodifier(evt)
    local key = TUI.keycode(evt)
    local keycode = join((v for (k, v) in pairs(MOD_NAMES) if string(k) ∈ mod), "-")

    keycode = isempty(keycode) ? key : keycode * "-" * key
    scr.lastevt[] = evt
    if scr.mouse_area[] === :screen && "CONTROL" ∉ mod
        local keybyte = get(ASCII_TO_KEY_CODE, lowercase(key), nothing)

        isnothing(keybyte) &&
            return
        if evt.data.kind == "Release"
            scr.repl.worker.memory[OFFSET_KEY] = UNUSED_KEYS[1]
            scr.repl.worker.memory[OFFSET_CHR] = 0
        elseif evt.data.kind ∈ ("Press", "Repeat")
            #log("PRESS $evt.data")
            if length(key) == 1 && lowercase(key) != key || "SHIFT" ∈ mod
                keybyte |= SHIFT
            end
            if "CONTROL" ∈ mod
                keybyte |= CTRL
            end
            scr.repl.worker.memory[OFFSET_KEY] = keybyte
            local chr = get(ASCII_CODES, key, nothing)
            scr.repl.worker.memory[OFFSET_CHR] = !isnothing(chr) ? chr : 0x0
        end
    elseif evt.data.kind ∈ ["Press", "Repeat"]
        invokelatest(get(KEYMAP, keycode, (a...) -> nothing), scr, evt, scr.cursor[]...)
    end
end

function setstatus(scr::Screen, msg...)
    scr.status[] = "CTRL-Q: QUIT $(string(msg...))"
end

function app(m::Screen; wait = 1 / 10)
    tui(; mouse = true) do
        @debug "Creating terminal"
        t = TUI.Terminal(; wait)
        TUI.init!(m, t)
        log("STARTING APP")
        while !should_quit(m)
            Revise.revise()
            invokelatest(appevent, m, t)
        end
        @debug "End"
    end
end

function appevent(m::Screen, t)
    update_from_worker(m.repl)
    #@debug "Getting event"
    evt = TUI.try_get_event(t)
    !isnothing(evt) && @debug "Got event" event = evt
    #@debug "Updating model"
    TUI.update!(m, evt)
    #@debug "Rendering model"
    TUI.render(t, m)
    #@debug "Drawing model"
    TUI.draw(t, m)
end

TUI.draw(t::CrosstermTerminal, ::Any) = TUI.draw(t)

function TUI.draw(t::CrosstermTerminal, scr::Screen)
    TUI.draw(t)
    scr.cursor[] == 0 && return
    if isdefined(scr.repl, :worker)
        local mem = scr.repl.worker.memory
        local screenmem = @view mem[intrange(screen)]
        local chardefs = @view mem[intrange(char_defs)]
        if scr.mode[] != :repl
            if screenmem != scr.lastscreen[] || chardefs != scr.lastchars[]
                scr.lastscreen[] = screenmem[:]
                scr.lastchars[] = chardefs[:]
                drawscreen(scr)
            end
            if !scr.imageshown[]
                move_cursor(TERMINAL[], scr.screenloc[]...)
                local screenfile = base64encode(scr.screenfile[1])
                local cmd = "$(GSTART)a=T,f=24,t=f,s=320,v=200,r=25,c=$(scr.screenwid[]),q=2;$screenfile$GEND"
                TUI.put(cmd)
                scr.imageshown[] = true
            end
        end
    end
    showcursor(scr)
end

function drawscreen(scr::Screen)
    log("draw screen")
    local image = scr.image
    local mem = scr.repl.worker.memory
    local scrm = screenmem(scr.repl.banksettings, mem)
    local chardefs = charmem(scr.repl.banksettings, mem)
    local bg = C64_PALETTE[1+mem[BG0.value]]

    #log("DRAWING SCREEN, BG $(bg)\n  SCREEN $(hex(scr.repl.banksettings.chrmem))\n  30 CHAR DEFS BYTES $(hex(chardefs[1:30]))\n  30 SCREEN BYTES $(hex(scrm[1:30]))")
    for col = 0:39
        for row = 0:24
            i = row * 40 + col
            char = scrm[1+i]
            for pixel = 0:7
                x = col * 8 + pixel
                fg = C64_PALETTE[1+mem[COLOR_MEM.value+i]]
                for rowbyte = 0:7
                    y = row * 8 + rowbyte
                    offset = y * 320 * 3 + x * 3
                    pixels = chardefs[1+8*char+rowbyte]
                    color = (pixels >> (7 - pixel)) & 1 == 1 ? fg : bg
                    image[offset+1] = color[1]
                    image[offset+2] = color[2]
                    image[offset+3] = color[3]
                end
            end
        end
    end
    sync!(image)
    scr.imageshown[] = false
end

function delete_image(t::CrosstermTerminal, id::String)
    id == "" && return
    TUI.put("$(GSTART)a=d,d=i,i=$id$GEND")
end

function trimlines(scr::Screen)
    local height = scr.areas[:repl].height
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
    local wrapped =
        split(wrap(line; width = scr.areas[:repl].width, replace_whitespace = false), "\n")

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
    scr.lastcmd[] = scr.repllines[end-1]
    handle_command(scr.repl, scr.repllines[end-1])
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
    local off = x - scr.areas[:repl].x + 1

    if x < 1
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
    local off = x - scr.areas[:repl].x + 1

    if length(line) >= off
        scr.repllines[end] = line[1:off-1]
        rewrap(scr)
    end
end

function key_quit(scr::Screen, ::TUI.KeyEvent, x)
    scr.quit[] = true
end

function key_layout(scr::Screen, ::TUI.KeyEvent, x)
    !isdefined(scr.repl, :worker) &&
        return
    local pos = findfirst(==(scr.mode[]), MODES)
    scr.mode[] = MODES[pos % length(MODES) + 1]
    configure(scr)
end

function key_refresh(scr::Screen, ::TUI.KeyEvent, x)
    invokelatest(configure, scr)
    TerminalUserInterfaces.reset(previous_buffer())
end

function bind_keys(::Screen)
    for c in filter(isprint, Char(0):Char(255))
        KEYMAP[string(c)] = key_self_insert
        KEYMAP[string(uppercase(c))] = key_self_insert
    end
    merge!(
        KEYMAP,
        Dict(
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
        ),
    )
end

println(scr::Screen, args...) = print(scr, args..., "\n")

function print(scr::Screen, args...)
    scr.cursor[] == 0 && error("Attempt to print to an uninitialized screen")
    local content = sprint() do io
        print(io, args...)
    end
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
