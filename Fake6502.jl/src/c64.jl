module C64
using ..Fake6502: Machine, NewMachine, A, display_chars, diag, CONDENSE_START, loadprg, screen, run, step
using ..Fake6502: ROM, init_rom, Addr, AddrRange, intRange, hex
using ..Fake6502: register, print_n, call_6502, call_frth, reset
using SimpleDirectMediaLayer
using SimpleDirectMediaLayer.LibSDL2
using Printf

const SCREEN_WIDTH = 40 * 8
const SCREEN_HEIGHT = 25 * 8
const CHAR_OFFSETS = 1:40*25
const CHAR_MEM = A(0x400) # to 0x7E8 -- character defs
const AFTER_CHAR_MEM = A(0x400) # to 0x7E8 -- character defs
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
BANK_CHOICES = [
    (true, BASIC_ROM),
    (true, KERNAL_ROM),
    (),
    (false, CHAR_ROM),
]
const VIC_SETS = Set([A(0x1000), A(0x1800), A(0x9000), A(0x9800)])
const VIC_MEM = A(0xD018)
const VIC_BANK = A(0xDD00)
const UPDATE_PERIOD = 1000000 ÷ 5

#SDL2_pkg_dir = dirname(dirname(pathof(SimpleDirectMediaLayer)))

color(value::Integer) = ((value >> 16, (value >> 8) & 0xFF, value & 0xff, 0xFF))

const COLOR_DEFS =
    (;
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
const COLORS = (; (name => i-1 for (i, name) in enumerate(keys(COLOR_DEFS)))...)
const PUNCT = "[£]⭡⭠ !\"#\$%&'()*+,-./0123456789:;<=>?"
BLANKS(n) = String((c->' ').(1:n))
const SCREEN_CODES = Vector{Char}(
    '@' * String('A':'Z') * PUNCT * BLANKS(64) *
    '@' * String('a':'z') * PUNCT *
    ' ' * String('A':'Z') *
    BLANKS(255 - 219 + 1)
)
screen2ascii(char) = SCREEN_CODES(UInt8(char))

@kwdef mutable struct C64_machine
    renderer::Ptr{SDL_Renderer}
    all_dirty::Bool = true
    dirty_characters::Array{Bool, 1} = zeros(Bool, (40*25,)) # characters that have changed
    dirty_character_defs::Array{Bool, 1} = zeros(Bool, (256,)) # character defs that have changed
    multicolor::Bool = false
    banks::Set{AddrRange} = Set{AddrRange}()
    screen_mem::Addr = A(0x400)
    character_mem::Addr = A(0x1000)
    running::Bool = true
end

function screen_mem(mach::Machine)
    c = c64(mach)
    mach[c.screen_mem:c.screen_mem + 999]
end    

function character_mem(mach::Machine)
    c = c64(mach)
    characters = c.character_mem ∈ VIC_SETS ? ROM : mach.mem
    @view characters[c.character_mem.value:c.character_mem.value + 0x7FF]
end

function with_sdl(func::Function)
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 16)
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 16)
    
    @assert SDL_Init(SDL_INIT_EVERYTHING) == 0 "error initializing SDL: $(unsafe_string(SDL_GetError()))"
    win = SDL_CreateWindow("Game", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1000, 1000, SDL_WINDOW_SHOWN)
    SDL_SetWindowResizable(win, SDL_TRUE)
    renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC)
    SDL_RenderSetLogicalSize(renderer, SCREEN_WIDTH, SCREEN_HEIGHT)
    SDL_RenderClear(renderer)
    try
        func(renderer)
    finally
        SDL_DestroyWindow(win)
        SDL_Quit()
    end
end

function c64_read_mem(mach::Machine, addr::UInt16)
    state = c64(mach)
    banks = state.banks
    for bank in banks
        addr ∈ bank &&
            return ROM[addr.value]
    end
    return mach[addr]
end

function c64_write_mem(mach::Machine, addr::UInt16, byte::UInt8)
    #println("STORE ", hex(addr, 4), " = ", hex(byte))
    state = c64(mach)
    adr = A(addr)
    adr ∈ state.screen_mem:state.screen_mem+999 &&
        println("WRITING ON SCREEN AT ", A(addr) - state.screen_mem)
    if adr == BANK_SWITCH
        # writing to bank switcher
        println("WRITE TO BANK SWITCH")
        switch_banks(mach, byte)
        return
    elseif adr == VIC_MEM || adr == VIC_BANK
        mach[adr] == byte && return
        mach[adr] = byte
        update_vic_bank(mach.mem, state)
        println("WRITE TO VIC MEM")
        return
    elseif any(in_bank.(Ref(adr), (CHAR_ROM, KERNAL_ROM, BASIC_ROM), Ref(state.banks)))
        # skip it, it's ROM
        println("WRITE TO ROM")
        return
    elseif state.screen_mem <= adr < state.screen_mem + 1000
        # writing to screen
        mach.properties[:c64].dirty_characters[adr - state.screen_mem + 1] = true
    elseif state.character_mem <= adr < state.character_mem + 0x800
        # writing to character data
        mach.properties[:c64].dirty_character_defs[(adr - state.character_mem) >> 8] = true
    end
    mach[addr] = byte
end

in_bank(addr, bank, banks) = addr ∈ bank && bank ∈ banks

function switch_banks(mach::Machine, value::UInt8)
    banks = c64(mach).banks
    io = mach[IO_CTL]
    original = settings = mach[BANK_SWITCH]
    for bit in (0x01, 0x02, 0x04)
        if io & bit != 0
            settings = (settings & ~bit) | bit
            (on, bank) = BANK_CHOICES[bit]
            if on
                push!(banks, bank)
            else
                delete!(banks, bank)
            end
        end
    end
    if settings != original
        println("BANK CHOICES CHANGED")
        mach[BANK_SWITCH] = settings
    else
        println("BANK CHOICES DID NOT CHANGE")
    end
end

"choose screen and character mem based on contents of VIC_BANK and "
function update_vic_bank(mem::Vector{UInt8}, state::C64_machine)
    state.all_dirty = true
    offset = A((3 - (mem[VIC_BANK] & 0xF)) << 14)
    state.screen_mem = offset + ((mem[VIC_MEM] & 0xF0) << 6)
    state.character_mem = offset + ((mem[VIC_MEM] & 0x0E) << 11)
end

c64(mach::Machine)::C64_machine = mach.properties[:c64]

function update_screen(mach::Machine)
    c = c64(mach)
    all_dirty = c.all_dirty
    renderer = c.renderer
    dirtydefs = Set(i for (i,d) in enumerate(c.dirty_character_defs) if d)
    isempty(dirtydefs) && !any(c.dirty_characters) &&
        return false
    scr_mem = screen_mem(mach)
    char_mem = character_mem(mach)
    for col in 0:39
        for pixel in 0 : 7
            x = col * 7 + pixel
            for row in 0:24
                i = row * 40 + col
                char = scr_mem[1 + i]
                !all_dirty && !c.dirty_characters[1 + i] && char ∉ dirtydefs &&
                    continue
                c.multicolor &&
                    error("multicolor not supported")
                bg = C64_PALETTE[1 + mach[BG0]]
                fg = C64_PALETTE[1 + mach[COLOR_MEM + i]]
                for rowbyte in 0 : 7
                    y = row * 8 + rowbyte
                    pixels = char_mem[1 + 8 * char + rowbyte]
                    color = (pixels >> (7 - pixel)) & 1 == 1 ? fg : bg
                    SDL_SetRenderDrawColor(renderer, color...)
                    SDL_RenderDrawPoint(renderer, x, y)
                end
            end
        end
    end
    c.dirty_characters .= false
    c.dirty_character_defs .= false
    c.all_dirty = false
    #error("burp")
    SDL_RenderPresent(renderer)
    #SDL_Delay(10)
    return true
end

struct Close <: Exception end
prev_evt = 0

function check_close(renderer)
    event_ref = Ref{SDL_Event}()
    Bool(SDL_PollEvent(event_ref))
    evt = event_ref[]
    evt_ty = evt.type
    if evt.type != SDL_POLLSENTINEL && prev_evt != evt.type
        println(evt.type)
        global prev_evt = evt.type
    end
    if evt.type == SDL_WINDOWEVENT
        if evt.window.event ∈ (SDL_WINDOWEVENT_EXPOSED, SDL_WINDOWEVENT_MAXIMIZED, SDL_WINDOWEVENT_SHOWN, SDL_WINDOWEVENT_RESTORED, SDL_WINDOWEVENT_SIZE_CHANGED, SDL_WINDOWEVENT_RESIZED)
            SDL_RenderPresent(renderer)
        end
    end
    return evt_ty == SDL_QUIT
end

function init_c64(mach::Machine)
    mach.mem[intRange(screen)] .= ' '
    mach[BORDER] = 0xE
    mach[BG0] = 0x6
    mach[BG1] = 0x1
    mach[BG2] = 0x2
    mach[BG3] = 0x3
    init_rom()
    mach[IO_CTL] = 0x2F
    switch_banks(mach, 0x07)
    labels = mach.labels
    off, total = loadprg("a.out", mach; labelfile="condensed.labels")
    println("Loaded ", total, " bytes at 0x", string(off; base=16, pad=4), ", ", length(labels), " labels")
    print("labels:")
    for name in sort([keys(labels)...])
        @printf "\n  %04x %s" labels[name].value-1 name
    end
    println()
end

function test_c64()
    #global mach = NewMachine()
    global mach = NewMachine(; write_func = c64_write_mem)
    with_sdl() do renderer
        state = C64_machine(; renderer)
        mach.properties[:c64] = state
        init_c64(mach)
        println("ROM MEM: ", hex(ROM[BASIC_ROM.first.value]))
        labels = mach.labels
        lastlabel = nothing
        labelcount = 0
        addrs = Dict(addr => name for (name, addr) in labels)
        maxwid = max(length.(string.(keys(labels)))...)
        state.all_dirty = true
        update_screen(mach)
        try
            nextupdate = UPDATE_PERIOD
            mach.step = function(mach::Machine)
                label = Base.get(addrs, A(mach.cpu.pc), nothing)
                if !isnothing(label)
                    if label === lastlabel
                        labelcount === 0 && println("  LOOP...")
                        labelcount += 1
                    else
                        print(rpad(string(label) * ": ", maxwid + 2))
                        lastlabel = label
                        labelcount = 0
                        diag(mach)
                    end
                end
                step(mach)
                if mach.emu.clockticks >= nextupdate
                    update_screen(mach)
                    nextupdate += UPDATE_PERIOD
                end
                if check_close(renderer)
                    mach.cpu.s = 0
                end
            end
            register(print_n, mach, :print_n)

            println("CALLING ASMTEST")
            reset(mach)
            mach.cpu.s = 0xfe
            result = call_6502(mach, :asmtest)
            print("RESULT: ")
            diag(result)

            println("CALLING FRTHTEST")
            reset(mach)
            mach.cpu.s = 0xfe
            call_frth(mach, :frthtest_def)
            println("RESULT: ", A(mach[:frthresult] | (UInt16(mach[mach.labels[:frthresult] + 1]) << 8)))

            run(mach, labels[:main]; max_ticks = 10000)
            state.all_dirty = true
            update_screen(mach)
        catch err
            if !(err isa Close)
                @error "error running instructions" exception=(err,catch_backtrace())
                rethrow(err)
            end
        finally
            diag(mach)
        end
        while !check_close(renderer)
        end
    end
    display_chars(screen_mem(mach)) do c; SCREEN_CODES[c + 1]; end
    println("done testing, ", mach.emu.clockticks, " clock ticks")
end

end # module C64
