module C64
using Revise
using ..Fake6502
using ..Fake6502:
    Machine,
    NewMachine,
    A,
    display_chars,
    diag,
    CONDENSE_START,
    loadprg,
    screen,
    run,
    step,
    VIC_BANK,
    VIC_MEM,
    log
using ..Fake6502: ROM, Addr, AddrRange, intrange, hex, SCREEN_CODES, screen2ascii
using ..Fake6502: register, print_n, call_6502, call_frth, reset, EDIR
using ..Fake6502: prep_call, finish_call, prep_frth, finish_frth, setpc, dbyte, call_fake
import ..Fake6502: Fake6502m, mem, mprint, mprintln, initprg
using ..Fake6502.Fake6502m: Cpu, Temps, pc
import ..Fake6502.Fake6502m: read6502, write6502
using ..Fake6502.Rewinding
using ..Fake6502.Rewinding: Rewinder, RewindSession
using Printf
#using CImGui:
#    GetTextLineHeight,
#    GetTextLineHeightWithSpacing,
#    GetStyle,
#    SetNextWindowFocus,
#    SetKeyboardFocusHere,
#    SetItemDefaultFocus
#using CImGui
#using CImGui.LibCImGui
#using CImGui.ImGuiGLFWBackend
#using CImGui.ImGuiGLFWBackend.LibCImGui
#using CImGui.ImGuiGLFWBackend.LibGLFW
#using CImGui.ImGuiOpenGLBackend
#using CImGui.ImGuiOpenGLBackend.ModernGL
## using CImGui.ImGuiGLFWBackend.GLFW
#using CImGui.CSyntax
#using CImGui.CSyntax.CStatic
#using ProfileCanvas
using Base.Threads

const VIC_CHAR_ROM_BANKS = (0x3, 0x1)
const SCREEN_WIDTH = 40 * 8
const SCREEN_HEIGHT = 25 * 8
const CHAR_OFFSETS = 1:40*25
const CHAR_MEM = A(0x400) # to 0x7E8 -- character defs
const COLOR_MEM = A(0xD800) # to 0xDBE7 -- foreground colors of screen characters
const CHAR_DEFS = A(0xD000)
const BORDER = A(0xD020)
const BG0 = A(0xD021)
const BG1 = A(0xD022)
const BG2 = A(0xD023)
const BG3 = A(0xD024)
const LINE_BYTES = 40 * 8
const CHAR_BYTES = 8
const IO_CTL = A(0x0000)
const BANK_SWITCH = A(0x0001)
const BASIC_ROM = A(0xA000:0xBFFF)
const CHAR_ROM = A(0xD000:0xDFFF)
const KERNAL_ROM = A(0xE000:0xFFFF)
const BANK_CHOICES = [
    [],
    [CHAR_ROM],
    [CHAR_ROM, KERNAL_ROM],
    [BASIC_ROM, CHAR_ROM, KERNAL_ROM],
    [],
    [],
    [KERNAL_ROM],
    [BASIC_ROM, KERNAL_ROM],
]
const UPDATE_PERIOD = 1000000 ÷ 5
revising = false
#! format: off
const KEYBOARD_INPUTS = [
    "STOP"      "Q"        "COMMODORE" " "        "2"         "CTRL"      "BACK"      "1"
    "/"         "^"        "="         "RSHIFT"   "HOME"      ";"         "*"         "\$"
    ","         "@"        ":"         "."        "-"         "L"         "P"         "+"
    "N"         "O"        "K"         "M"        "0"         "J"         "I"         "9"
    "V"         "U"        "H"         "B"        "8"         "G"         "Y"         "7"
    "X"         "T"        "F"         "C"        "6"         "D"         "R"         "5"
    "LSHIFT"    "E"        "S"         "Z"        "4"         "A"         "W"         "3"
    "CRSR DN"   "F5"       "F3"        "F1"       "F7"        "CRSR RT"   "RETURN"    "DELETE"
]
#! format: on

const KEY_CODES = [
"Inst/Del",
"Return",
"Crsr ←→",
"F7/F8",
"F1/F2",
"F3/F4",
"F5/F6",
"Crsr ↑↓",
"3",
"w",
"a",
"4",
"z",
"s",
"e",
"",
"5",
"r",
"d",
"6",
"c",
"f",
"t",
"x",
"7",
"y",
"g",
"8",
"b",
"h",
"u",
"v",
"9",
"i",
"j",
"0",
"m",
"k",
"o",
"n",
"+",
"p",
"l",
"−",
">",
"[",
"@",
"<",
"£",
"*",
"]",
"Clr/Home",
"",
"=",
"↑",
"?",
"1",
"←",
"",
"2",
" ",
"",
"q",
"Run/Stop",
]

const KEY_TO_CODE = Dict(k=>i for (i, k) in enumerate(KEY_CODES))

const SHIFTED = [
    ("delete", "insert")=>"Inst/Del",("right", "left")=>"Crsr ←→", ("f1", "f2")=>"F1/F2",
    ("f3", "f4")=>"F3/F4", ("f5", "f6")=>"F5/F6", ("f7", "f8")=>"F7/F8", ("down", "up")=>"Crsr ↑↓",
    ("home", "end")=>"Clr/Home", ("esc", "")=>"Run/Stop"
]

const ASCII_CODES = Dict(
    (k => UInt8(k[1]) for k in KEY_CODES if length(k) == 1 && Int(k[1]) <= 255)...,
    (k => UInt8(k[1]) for k in uppercase.(KEY_CODES) if length(k) == 1 && Int(k[1]) <= 255)...,
    "Right" => 0x1d,
    "Left" => 0x9d,
    "Down" => 0x11,
    "Up" => 0x91,
)

const ASCII_TO_KEY_CODE = Dict(
    (k=>i for (i, k) in enumerate(KEY_CODES) if length(k) == 1)...,
    "^"=>KEY_TO_CODE["↑"],
    (k=>KEY_TO_CODE[v] for ((k,), v) in SHIFTED)...,
    "enter"=>KEY_TO_CODE["Return"],
)

const CTRL = 0x40
const SHIFT = 0x80
const UNUSED_KEYS = [i for (i, k) in enumerate(KEY_CODES) if k == ""]

function __init__()
    for ((_,k), v) in SHIFTED
        ASCII_TO_KEY_CODE[k] = KEY_TO_CODE[v] | SHIFT
    end
end

setrevising(rev::Bool) = global revising = rev

color(value::Integer) = ((value >> 16, (value >> 8) & 0xFF, value & 0xff, 0xFF))

const COLOR_DEFS = (;
    Black = 0x000000,
    White = 0xFFFFFF,
    Red = 0x880000,
    Cyan = 0xAAFFEE,
    Violet = 0xCC44CC,
    Green = 0x00CC55,
    Blue = 0x0000AA,
    Yellow = 0xEEEE77,
    Orange = 0xDD8855,
    Brown = 0x664400,
    Lightred = 0xFF7777,
    Darkgrey = 0x333333,
    Grey = 0x777777,
    Lightgreen = 0xAAFF66,
    Lightblue = 0x0088FF,
    Lightgrey = 0xBBBBBB,
)

const C64_PALETTE = color.(values(COLOR_DEFS))
const COLORS = (; (name => i - 1 for (i, name) in enumerate(keys(COLOR_DEFS)))...)
const iochan = Channel{Function}(1024)
const scr_width, scr_height = 320, 200

struct Rect
    x::Int
    y::Int
    w::Int # width 0 means left and right are the same, i.e. this is a segment or point 
    h::Int # height 0 means top and bottom are the same, i.e. this is a segment or point
end

Rect(x, y; r, b) = Rect(x, y, r - x, b - y)

@kwdef mutable struct C64_machine
    video_lock::ReentrantLock = ReentrantLock()
    needs_update::Atomic{Bool} = Atomic{Bool}(false)
    all_dirty::Bool = true
    dirty_characters::Vector{Bool} = zeros(Bool, (40 * 25,)) # characters that have changed
    dirty_character_defs::Vector{Bool} = zeros(Bool, (256,)) # character defs that have changed
    multicolor::Bool = false
    banks::Set{AddrRange} = Set{AddrRange}()
    screen_mem::Addr = A(0x400)
    character_mem::Addr = A(0x1000)
    vic_bank_summary::UInt8 = 0x00
    pause::Condition = Condition()
    pause_count::Atomic{Int} = Atomic{Int}(0)
    actually_paused::Condition = Condition()
    pausing::Atomic{Bool} = Atomic{Bool}(false)
    running::Atomic{Bool} = Atomic{Bool}(true)
    rewinder::Union{Nothing, Rewinder} = nothing
    session::RewindSession = RewindSession()
    maxtime::Atomic{UInt64} = Atomic{UInt64}(0)
    curtime::Atomic{UInt64} = Atomic{UInt64}(0)
    fake_routines::Dict{Addr,Function} = Dict{Addr,Function}()
end

Base.show(io::IO, ::C64_machine) = print(io, "C64_machine")

function pause(f::Function, c)
    pause(c)
    try
        f()
    finally
        resume(c)
    end
end

function pause(state)
    c = c64(state)
    # issue a pause command
    lock(c.pause) do
        c.pause_count[] += 1
    end
    wait_for_pause(c)
end

function wait_for_pause(state)
    c = c64(state)
    # wait for the machine to actually pause
    # once it's paused, the machine is known to be locked
    # so it's safe to modify the state until it's resumed (registers, memory, etc.)
    # note: this use of the "double-checked locking pattern" is valid because pausing is atomic
    if !c.pausing[]
        lock(c.actually_paused) do
            !c.pausing[] && wait(c.actually_paused)
        end
    end
end

function resume(state)
    c = c64(state)
    lock(c.pause) do
        c.pause_count[] -= 1
        c.pause_count[] == 0 && notify(c.pause)
    end
end

isscreen(c64::C64_machine, addr::UInt16) = addr & 0xFC00 == c64.screen_mem.value - 1
ischars(c64::C64_machine, addr::UInt16) = addr & 0xF800 == c64.character_mem.value - 1
isvideo(c64::C64_machine, addr::UInt16) = isscreen(c64, addr) || ischars(c64, addr)

right(r::Rect) = r.x + r.w

bottom(r::Rect) = r.y + r.h

above_or_left(r1::Rect, r2::Rect) = right(r1) + 1 < r2.x || bottom(r1) + 1 < r2.y

intersects(r1::Rect, r2::Rect) = !(above_or_left(r1, r2) || above_or_left(r2, r1))

merge(r1::Rect, r2::Rect) = Rect(
    min(r1.x, r2.x),
    min(r1.y, r2.y);
    r = max(right(r1), right(r2)),
    b = max(bottom(r1), bottom(r2)),
)

macro io(args)
    :(use_io(() -> $(esc(args))))
end

usingio = true

use_io(func::Function) = put!(iochan, func)

function process_io()
    if isready(iochan)
        usingio = true
        try
            while isready(iochan)
                take!(iochan)()
            end
        finally
            usingio = false
        end
    end
end

mprint(::Machine, args...) = @io print(args...)
mprintln(::Machine, args...) = @io println(args...)
mprint(::Cpu, args...) = @io print(args...)
mprintln(::Cpu, args...) = @io println(args...)


c64(cpu::Cpu)::C64_machine = c64(cpu.user_data)
c64(state::C64_machine) = state
c64(mach::Machine)::C64_machine = c64(mach.newcpu)

function screen_mem(mach::Machine)
    c = c64(mach)
    screen = mem(mach)
    @view screen[c.screen_mem.value:c.screen_mem.value+999]
end

userom(mach::Machine) = userom(mach.newcpu, c64(mach))

function userom(mach::Cpu, state::C64_machine)
    return (mach.memory[VIC_BANK] & 3) in VIC_CHAR_ROM_BANKS && (state.vic_bank_summary & 0xE) == 4
end

function character_mem(mach::Machine)
    c = c64(mach)
    characters = userom(mach) ? ROM : mem(mach)
    #log("CHARS USING $(userom ? "ROM" : "RAM")")
    @view characters[A(c.character_mem:c.character_mem.value+0x7FF)]
end

Fake6502m.read6502(cpu::Cpu{C64_machine}, addr::UInt16) = read6502(c64(cpu), cpu, addr)

function Fake6502m.read6502(state::C64_machine, cpu::Cpu, addr::UInt16)
    #log("C64 READ 1")
    banks = state.banks
    adr = A(addr)
    #log("C64 READ 2")
    for bank in banks
        #adr ∈ bank && log("READING $(hex(adr)) FROM ROM (read6502)")
        adr ∈ bank && return ROM[adr]
    end
    #log("C64 READ 3")
    cpu.memory[adr]
end

c64_set_mem(cpu::Cpu, addr::Addr, byte::UInt8) =
    c64_set_mem(cpu, UInt16(addr.value - 0x01), byte)

c64_set_mem(cpu::Cpu, addr::UInt16, byte::UInt8) = c64_set_mem(c64(cpu), cpu, addr, byte)

c64_set_mem(state::C64_machine, cpu::Cpu, addr::Addr, byte::UInt8) =
    c64_set_mem(state, cpu, UInt16(addr.value - 0x01), byte)

c64_set_mem(state::C64_machine, cpu::Cpu, addr::UInt16, byte::UInt8) =
    if isnothing(state.rewinder)
        cpu.memory[addr + 1] = byte
    else
        Rewinding.write6502(state.rewinder, cpu, addr, byte)
    end

Fake6502m.jsr(cpu::Cpu{C64_machine}, temps) = Fake6502m.jsr(c64(cpu), cpu, temps)

function Fake6502m.jsr(mach::C64_machine, cpu::Cpu, temps::Temps)
    #log("JSR PC $(hex(pc(cpu, temps))) EA $(hex(cpu.ea))")
    temps = Fake6502m.base_jsr(cpu, temps)
    return jumpfake("JSR", mach, cpu, temps)
end

function Fake6502m.jmp(mach::C64_machine, cpu::Cpu, temps::Temps)
    #log("JMP PC $(hex(pc(cpu, temps))) EA $(hex(cpu.ea))")
    temps = Fake6502m.base_jmp(cpu, temps)
    return jumpfake("JMP", mach, cpu, temps)
end

function jumpfake(label, mach::C64_machine, cpu::Cpu, temps::Temps)
    local curpc = Fake6502m.pc(cpu, temps)
    #log("C64 $label TO $(hex(curpc))")
    curpc >= length(mach.fake_routines) && return temps
    # jumping to fake routine
    #log("$label TO FAKE ROUTINE $(hex(curpc)) $(mach.fake_routines[A(curpc)])")
    local result = mach.fake_routines[A(curpc)](cpu, temps)
    #log("RTS FROM FAKE ROUTINE")
    return Fake6502m.rts(cpu, result isa Temps ? result : temps)
end

Fake6502m.write6502(cpu::Cpu{C64_machine}, addr::UInt16, byte::UInt8) =
    write6502(c64(cpu), cpu, addr, byte)

function Fake6502m.write6502(state::C64_machine, cpu::Cpu, addr::UInt16, byte::UInt8)
    adr = A(addr)
    if isvideo(state, addr)
        video(state) do
            if isscreen(state, addr)
                # writing to screen
                state.needs_update[] = true
                local offset = addr - (state.screen_mem.value - 1)
                local col = offset % 40
                local row = offset ÷ 40
                @io println(
                    "WRITING ON SCREEN AT $(A(addr) - state.screen_mem), $col x $row: $(dbyte(byte))",
                )
                state.dirty_characters[1+offset] = true
            else
                # writing to character data
                @io println("WRITING TO CHARACTER MEM AT ", A(addr) - state.screen_mem)
                state.needs_update[] = true
                state.dirty_character_defs[(adr-state.character_mem)>>8] = true
            end
            c64_set_mem(cpu, addr, byte)
        end
        return
    elseif adr == BANK_SWITCH || adr == IO_CTL
        # writing to bank switcher
        #log("MEM WRITE TRIGGERED A BANK SWITCH, $(hex(adr)) <- $(hex(byte))")
        @io println("WRITE TO BANK SWITCH")
        switch_banks(state, cpu, byte)
        return
    elseif adr == VIC_MEM || adr == VIC_BANK
        cpu.memory[adr] == byte && return
        c64_set_mem(cpu, addr, byte)
        update_vic_bank(cpu.memory, state)
        @io println("WRITE TO VIC MEM")
        return
    elseif any(in_bank.(Ref(adr), (CHAR_ROM, KERNAL_ROM, BASIC_ROM), Ref(state.banks)))
        # skip it, it's ROM
        @io println("WRITE TO ROM")
        return
    end
    c64_set_mem(cpu, addr, byte)
end

function c64_read_mem(mach::Machine, addr::UInt16)
    state = c64(mach)
    banks = state.banks
    adr = A(addr)
    for bank in banks
        adr ∈ bank && log("READING $(hex(adr)) FROM ROM (c64_read_mem)")
        adr ∈ bank && return ROM[adr]
    end
    return mach[adr]
end

in_bank(addr, bank, banks) = addr ∈ bank && bank ∈ banks

function switch_banks(state::C64_machine, cpu::Cpu, value::UInt8)
    #log("SWITCHING BANKS IO 0b$(bitstring(cpu.memory[IO_CTL])[end-2:end]) BANKS 0b$(bitstring(value)[end-2:end])")
    local banks = state.banks
    local originalbanks = Set(banks)
    local io = cpu.memory[IO_CTL]
    local settings = cpu.memory[BANK_SWITCH]
    for bit in (0x01, 0x02, 0x04)
        if io & bit != 0
            settings = (settings & ~bit) | (value & bit)
        end
    end
    empty!(state.banks)
    union!(state.banks, BANK_CHOICES[(settings & 0x7) + 1])
    if originalbanks != banks
        #log("BANK CHOICES CHANGED TO $banks, SET $(io & 7) VALUE $(bitstring(value)[end-2:end])")
        c64_set_mem(state, cpu, BANK_SWITCH, settings)
    #else
    #    log("BANK CHOICES DID NOT CHANGE FROM $banks, SET $(io & 7) VALUE $(bitstring(value)[end-2:end])")
    end
end

"choose screen and character mem based on contents of VIC_BANK and "
function update_vic_bank(mem::AbstractVector{UInt8}, state::C64_machine)
    state.all_dirty = true
    offset = (3 - UInt16(mem[VIC_BANK] & 0xF)) << 14
    state.screen_mem = A(offset + ((UInt16(mem[VIC_MEM]) & 0xF0) << 6))
    state.character_mem = A(offset + ((UInt16(mem[VIC_MEM]) & 0x0E) << 10))
    state.vic_bank_summary = mem[VIC_MEM]
    #log("CHOOSE VIC OFFSET $offset SCREEN $(state.screen_mem) CHARS $(state.character_mem) SETTING $(mem[VIC_MEM])")
end

function c64_step(mach::Machine, state::C64_machine, addrs, lastlabel, labelcount, maxwid)
    # note: this use of the "double-checked locking pattern" is valid because pause_count is atomic
    if state.pause_count[] > 0
        lock(state.pause) do
            if state.pause_count[] > 0
                lock(state.actually_paused) do
                    state.pausing[] = true
                    notify(state.actually_paused)
                end
                # wait until resumed
                wait(state.pause)
            end
        end
    end
    #println("c64 step")
    label = Base.get(addrs, A(mach.cpu.pc), nothing)
    if !isnothing(label)
        if label == lastlabel[]
            labelcount[] == 0 && @io println("  LOOP...")
            labelcount[] += 1
        else
            @io print(rpad(string(label) * ": ", maxwid + 2))
            lastlabel[] = label
            labelcount[] = 0
            diag(mach)
        end
    end
    if isnothing(c64(mach).rewinder)
        mach.temps = Fake6502m.inner_step6502(mach.newcpu, mach.temps)
    else
        mach.temps = Rewinding.inner_step6502(c64(mach).rewinder, mach.newcpu, mach.temps)
        if state.curtime[] == state.maxtime[]
            state.curtime[] = state.rewinder.curtime
        end
        state.maxtime[] = state.rewinder.curtime
    end
end

function initstate(state::C64_machine, cpu::Cpu; clearscreen = true)
    local memory = cpu.memory

    clearscreen && (memory[intrange(screen)] .= ' ')
    memory[BORDER] = 0xE
    memory[BG0] = 0x6
    memory[BG1] = 0x1
    memory[BG2] = 0x2
    memory[BG3] = 0x3
    for mem = 0xD800:0xDBE7
        memory[mem+1] = 0x01
    end
    memory[IO_CTL] = 0x2F
    memory[VIC_BANK] = 0x3
    memory[VIC_MEM] = 0x14
    #log("UPDATE WITH SETTING $(memory[VIC_MEM])")
    update_vic_bank(memory, state)
    switch_banks(state, cpu, 0b111)
end

function init(load::Function; state = C64_machine(), user_data = state, rewind=false)
    mach = NewMachine(; user_data)
    initstate(state, mach.newcpu)
    if rewind
        state.rewinder = Rewinder()
        Rewinding.init(state.rewinder, mach.newcpu, mach.temps)
        Rewinding.init_undo_session(state.rewinder, state.session)
    end
    load(mach)
    labels = mach.labels
    lastlabel = Ref{Any}(nothing)
    labelcount = Ref(0)
    addrs = Dict(addr => name for (name, addr) in labels)
    maxwid = max(length.(string.(keys(labels)))...)
    mach.step = function (mach::Machine)
        global revising

        if revising
            Base.invokelatest(c64_step, mach, state, addrs, lastlabel, labelcount, maxwid)
        else
            c64_step(mach, state, addrs, lastlabel, labelcount, maxwid)
        end
    end
    register(print_n, mach, :print_n)
    return mach, state
end

function video(func::Function, mach::C64_machine)
    #try
    #    error("attempt lock")
    #catch ex
    #    @error "Attempt lock" exception=(ex,catch_backtrace())
    #end
    lock(func, mach.video_lock)
    #func()
end

ticks(mach::Machine) = Fake6502m.ticks(mach.newcpu, mach.temps)

# setup banks after load
initprg(memrange::AddrRange, mach::Machine, state::C64_machine) =
    initprg(memrange, mach.newcpu, state)

function initprg(memrange::AddrRange, cpu::Cpu, state::C64_machine)
    #log("INITIALIZING PROGRAM AT $(first(memrange)):$(last(memrange))")
    if IO_CTL:BANK_SWITCH ∈ memrange && cpu.memory[IO_CTL] != 0
        switch_banks(state, cpu, cpu.memory[BANK_SWITCH])
    #    log("INITIALIZED BANKS WITH $(cpu.memory[IO_CTL]), $(cpu.memory[BANK_SWITCH]) TO $(state.banks)")
    #else
    #    log("DID NOT INITIALIZE BANKS, MEMRANGE: $memrange")
    end
    if VIC_MEM ∉ memrange && VIC_BANK ∉ memrange
        #log("NO VIC MEM SET IN PROGRAM, USING DEFAULTS")
        cpu.memory[VIC_BANK] = 0x03
        cpu.memory[VIC_MEM] = 0x14
    end
    update_vic_bank(cpu.memory, state)
end

function load_condensed(mach::Machine)
    labels = mach.labels
    off, total = loadprg("$EDIR/condensed.prg", mach; labelfile = "$EDIR/condensed.labels")
    println(
        "Loaded ",
        total,
        " bytes at 0x",
        string(off; base = 16, pad = 4),
        ", ",
        length(labels),
        " labels",
    )
    print("labels:")
    for name in sort([keys(labels)...])
        @printf "\n  %04x %s" labels[name].value - 1 name
    end
    println()
    println("ROM MEM: ", hex(ROM[BASIC_ROM.first]))
end

end # module C64
