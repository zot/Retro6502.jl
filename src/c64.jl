module C64
using Revise
using ..Fake6502
using ..Fake6502: Machine, NewMachine, A, display_chars, diag, CONDENSE_START, loadprg, screen, run, step
using ..Fake6502: ROM, init_rom, Addr, AddrRange, intRange, hex, call_step
using ..Fake6502: register, print_n, call_6502, call_frth, reset, EDIR, USE_GPL
using ..Fake6502: prep_call, finish_call, prep_frth, finish_frth, setpc, check_cpu
import ..Fake6502: Fake6502m, mem, mprint, mprintln
using ..Fake6502.Fake6502m: Cpu
import ..Fake6502.Fake6502m: read6502, write6502
using ..Fake6502.Rewinding
using ..Fake6502.Rewinding: Rewinder
using Printf
using CImGui: GetTextLineHeight, GetTextLineHeightWithSpacing, GetStyle
using CImGui
using CImGui.LibCImGui
using CImGui.ImGuiGLFWBackend
using CImGui.ImGuiGLFWBackend.LibCImGui
using CImGui.ImGuiGLFWBackend.LibGLFW
using CImGui.ImGuiOpenGLBackend
using CImGui.ImGuiOpenGLBackend.ModernGL
# using CImGui.ImGuiGLFWBackend.GLFW
using CImGui.CSyntax
using CImGui.CSyntax.CStatic
using ProfileCanvas
using Base.Threads

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
revising = false

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
    scr_id::Int
    scr_buf::Array{GLubyte, 3} = zeros(GLubyte, (4, scr_width, scr_height))
    scratch_buf::Array{GLubyte, 2} = fill(GLubyte(0xFF), (4, scr_width * scr_height))
    video_lock::ReentrantLock = ReentrantLock()
    needs_update::Atomic{Bool} = Atomic{Bool}(false)
    has_dirty_rects::Atomic{Bool} = Atomic{Bool}(false)
    all_dirty::Bool = true
    dirty_characters::Array{Bool, 1} = zeros(Bool, (40*25,)) # characters that have changed
    dirty_character_defs::Array{Bool, 1} = zeros(Bool, (256,)) # character defs that have changed
    dirty_rects::Vector{Rect} = Rect[]
    multicolor::Bool = false
    banks::Set{AddrRange} = Set{AddrRange}()
    screen_mem::Addr = A(0x400)
    character_mem::Addr = A(0x1000)
    pause::Condition = Condition()
    pause_count::Atomic{Int} = Atomic{Int}(0)
    actually_paused::Condition = Condition()
    pausing::Atomic{Bool} = Atomic{Bool}(false)
    running::Atomic{Bool} = Atomic{Bool}(true)
    rewinder::Rewinder = Rewinder()
    maxtime::Atomic{UInt64} = Atomic{UInt64}(0)
    curtime::Atomic{UInt64} = Atomic{UInt64}(0)
end

function pause(f::Function, c::C64_machine)
    pause(c)
    try
        f()
    finally
        resume(c)
    end
end

function pause(c::C64_machine)
    # issue a pause command
    lock(c.pause) do
        c.pause_count[] += 1
    end
    wait_for_pause(c)
end

function wait_for_pause(c::C64_machine)
    # wait for the machine to actually pause
    # once it's paused, the machine is known to be locked
    # so it's safe to modify the state until it's resumed (registers, memory, etc.)
    # note: this use of the "double-checked locking pattern" is valid because pausing is atomic
    if !c.pausing[]
        lock(c.actually_paused) do
            !c.pausing[] &&
                wait(c.actually_paused)
        end
    end
end

function resume(c::C64_machine)
    lock(c.pause) do
        c.pause_count[] -= 1
        c.pause_count[] == 0 &&
            notify(c.pause)
    end
end

isscreen(c64::C64_machine, addr::UInt16) = addr & 0xFC00 == c64.screen_mem.value - 1
ischars(c64::C64_machine, addr::UInt16) = addr & 0xF800 == c64.character_mem.value - 1
isvideo(c64::C64_machine, addr::UInt16) = isscreen(c64, addr) || ischars(c64, addr)

right(r::Rect) = r.x + r.w

bottom(r::Rect) = r.y + r.h

above_or_left(r1::Rect, r2::Rect) = right(r1) + 1 < r2.x || bottom(r1) + 1 < r2.y

intersects(r1::Rect, r2::Rect) = !(above_or_left(r1, r2) || above_or_left(r2, r1))

merge(r1::Rect, r2::Rect) =
    Rect(min(r1.x, r2.x), min(r1.y, r2.y); r = max(right(r1), right(r2)), b = max(bottom(r1), bottom(r2)))

macro io(args)
    :(use_io(()-> $(esc(args))))
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

mprint(::Machine{C64_machine}, args...) = @io print(args...)
mprintln(::Machine{C64_machine}, args...) = @io println(args...)
mprint(::Cpu{C64_machine}, args...) = @io print(args...)
mprintln(::Cpu{C64_machine}, args...) = @io println(args...)

c64(cpu::Cpu{C64_machine})::C64_machine = cpu.user_data

c64(mach::Machine)::C64_machine = c64(mach.newcpu)

function screen_mem(mach::Machine)
    c = c64(mach)
    screen = mem(mach)
    @view screen[c.screen_mem.value:c.screen_mem.value + 999]
end    

function character_mem(mach::Machine)
    c = c64(mach)
    characters = c.character_mem ∈ VIC_SETS ? ROM : mem(mach)
    @view characters[c.character_mem.value:c.character_mem.value + 0x7FF]
end

function Fake6502m.read6502(cpu::Cpu{C64_machine}, addr::UInt16)
    state = c64(cpu)
    banks = state.banks
    adr = A(addr)
    for bank in banks
        adr ∈ bank &&
            return ROM[adr.value]
    end
    cpu.memory[adr.value]
end

function Fake6502m.write6502(cpu::Cpu{C64_machine}, addr::UInt16, byte::UInt8)
    @io println("WRITE BYTE")
    state = c64(cpu)
    adr = A(addr)
    if isvideo(state, addr)
        video(state) do
            if isscreen(state, addr)
                # writing to screen
                @io println("WRITING ON SCREEN AT ", A(addr) - state.screen_mem)
                state.needs_update[] = true
                state.dirty_characters[adr - state.screen_mem + 1] = true
            else
                # writing to character data
                @io println("WRITING TO CHARACTER MEM AT ", A(addr) - state.screen_mem)
                state.needs_update[] = true
                state.dirty_character_defs[(adr - state.character_mem) >> 8] = true
            end
            #cpu.memory[adr.value] = byte
            Rewinding.write6502(cpu.user_data.rewinder, cpu, addr, byte)
        end
        return
    elseif adr == BANK_SWITCH
        # writing to bank switcher
        @io println("WRITE TO BANK SWITCH")
        switch_banks(state, byte)
        return
    elseif adr == VIC_MEM || adr == VIC_BANK
        cpu.memory[adr.value] == byte && return
        #cpu.memory[adr.value] = byte
        Rewinding.write6502(cpu.user_data.rewinder, cpu, addr, byte)
        update_vic_bank(cpu.memory, state)
        @io println("WRITE TO VIC MEM")
        return
    elseif any(in_bank.(Ref(adr), (CHAR_ROM, KERNAL_ROM, BASIC_ROM), Ref(state.banks)))
        # skip it, it's ROM
        @io println("WRITE TO ROM")
        return
    end
    #cpu.memory[adr.value] = byte
    Rewinding.write6502(cpu.user_data.rewinder, cpu, addr, byte)
end

function c64_read_mem(mach::Machine, addr::UInt16)
    state = c64(mach)
    banks = state.banks
    adr = A(addr)
    for bank in banks
        adr ∈ bank &&
            return ROM[adr.value]
    end
    return mach[adr]
end

#c64_set_mem(mach::Machine, addr::Addr, byte::UInt8) = mach[addr] = byte
#c64_set_mem(mach::Machine, addr::UInt16, byte::UInt8) = c64_set_mem(mach, A(addr), byte)
c64_set_mem(mach::Machine, addr::Addr, byte::UInt8) = c64_set_mem(mach, UInt16(addr.value - 0x01), byte)
c64_set_mem(mach::Machine, addr::UInt16, byte::UInt8) =
    Rewinding.write6502(c64(mach).rewinder, mach.newcpu, addr, byte)
c64_set_mem(cpu::Cpu{C64_machine}, addr::Addr, byte::UInt8) =
    c64_set_mem(cpu, UInt16(addr.value - 0x01), byte)
c64_set_mem(cpu::Cpu{C64_machine}, addr::UInt16, byte::UInt8) =
    Rewinding.write6502(cpu.user_data.rewinder, cpu, addr, byte)

in_bank(addr, bank, banks) = addr ∈ bank && bank ∈ banks

function switch_banks(cpu::Cpu{C64_machine}, value::UInt8)
    banks = cpu.user_data.banks
    io = cpu.memory[IO_CTL.value]
    original = settings = cpu.memory[BANK_SWITCH.value]
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
        @io println("BANK CHOICES CHANGED")
        settings != value && @io println("WARNING, BANK CHOICES IS $settings BUT VALUE WAS $value")
        c64_set_mem(cpu, BANK_SWITCH, settings)
    else
        @io println("BANK CHOICES DID NOT CHANGE")
    end
end

"choose screen and character mem based on contents of VIC_BANK and "
function update_vic_bank(mem::Vector{UInt8}, state::C64_machine)
    state.all_dirty = true
    offset = A((3 - (mem[VIC_BANK] & 0xF)) << 14)
    state.screen_mem = offset + ((mem[VIC_MEM] & 0xF0) << 6)
    state.character_mem = offset + ((mem[VIC_MEM] & 0x0E) << 11)
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
        if label === lastlabel[]
            labelcount[] === 0 && @io println("  LOOP...")
            labelcount[] += 1
        else
            @io print(rpad(string(label) * ": ", maxwid + 2))
            lastlabel[] = label
            labelcount[] = 0
            diag(mach)
        end
    end
    #mach.temps = Fake6502m.inner_step6502(mach.newcpu, mach.temps)
    mach.temps = Rewinding.inner_step6502(c64(mach).rewinder, mach.newcpu, mach.temps)
    if state.curtime[] == state.maxtime[]
        state.curtime[] = state.rewinder.curtime
    end
    state.maxtime[] = state.rewinder.curtime
    @io println("time cur: ", state.curtime[], " max: ", state.maxtime[])
    #diag(mach)
end

function init()
    state =
        C64_machine(; scr_id = ImGuiOpenGLBackend.ImGui_ImplOpenGL3_CreateImageTexture(scr_width, scr_height))
    mach = NewMachine(; user_data = state)
    mem(mach)[intRange(screen)] .= ' '
    mach[BORDER] = 0xE
    mach[BG0] = 0x6
    mach[BG1] = 0x1
    mach[BG2] = 0x2
    mach[BG3] = 0x3
    for mem in 0xD800:0xDBE7
        mach[mem] = 0x01
    end
    if USE_GPL
        mach.newcpu.memory[intRange(screen)] .= ' '
        mach.newcpu.memory[BORDER.value] = 0xE
        mach.newcpu.memory[BG0.value] = 0x6
        mach.newcpu.memory[BG1.value] = 0x1
        mach.newcpu.memory[BG2.value] = 0x2
        mach.newcpu.memory[BG3.value] = 0x3
    end
    init_rom()
    mach[IO_CTL] = 0x2F
    if USE_GPL
        mach.newcpu.memory[IO_CTL.value] = 0x2F
    end
    switch_banks(mach.newcpu, 0x07)
    Rewinding.init(state.rewinder, mach.newcpu, mach.temps)
    labels = mach.labels
    off, total = loadprg("$EDIR/condensed.prg", mach; labelfile="$EDIR/condensed.labels")
    println("Loaded ", total, " bytes at 0x", string(off; base=16, pad=4), ", ", length(labels), " labels")
    print("labels:")
    for name in sort([keys(labels)...])
        @printf "\n  %04x %s" labels[name].value-1 name
    end
    println()
    println("ROM MEM: ", hex(ROM[BASIC_ROM.first.value]))
    labels = mach.labels
    lastlabel = Ref{Any}(nothing)
    labelcount = Ref(0)
    addrs = Dict(addr => name for (name, addr) in labels)
    maxwid = max(length.(string.(keys(labels)))...)
    state.all_dirty = true
    update_screen(mach)
    nextupdate = UPDATE_PERIOD
    mach.step = function(mach::Machine)
        global revising

        if revising
            Base.invokelatest(c64_step, mach, state, addrs, lastlabel, labelcount, maxwid)
        else
            c64_step(mach, state, addrs, lastlabel, labelcount, maxwid)
        end
        if mach.emu.clockticks >= nextupdate
            update_screen(mach)
            nextupdate += UPDATE_PERIOD
        end
    end
    register(print_n, mach, :print_n)
    return mach
end

function merge_dirty(mach::C64_machine, rect::Rect)
    dirty = mach.dirty_rects
    changed = true
    while changed
        changed = false
        for i in length(dirty):-1:1
            if intersects(rect, dirty[i])
                rect = merge(rect, dirty[i])
                deleteat!(dirty, i)
                changed = true
            end
        end
    end
    push!(dirty, rect)
    mach.has_dirty_rects[] = true
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

function update_screen(mach::Machine)
    c = c64(mach)
    all_dirty = c.all_dirty
    dirtydefs = Set(i for (i,d) in enumerate(c.dirty_character_defs) if d)
    scr_mem = screen_mem(mach)
    char_mem = character_mem(mach)
    for col in 0:39
        for row in 0:24
            i = row * 40 + col
            char = scr_mem[1 + i]
            !all_dirty && !c.dirty_characters[1 + i] && char ∉ dirtydefs &&
                continue
            merge_dirty(c, Rect(col * 8, row * 8, 7, 7))
            for pixel in 0 : 7
                x = col * 8 + pixel
                c.multicolor &&
                    error("multicolor not supported")
                bg = C64_PALETTE[1 + mach[BG0]]
                fg = C64_PALETTE[1 + mach[COLOR_MEM + i]]
                for rowbyte in 0 : 7
                    y = row * 8 + rowbyte
                    pixels = char_mem[1 + 8 * char + rowbyte]
                    color = (pixels >> (7 - pixel)) & 1 == 1 ? fg : bg
                    c.scr_buf[1, x + 1, y + 1] = GLubyte(color[1])
                    c.scr_buf[2, x + 1, y + 1] = GLubyte(color[2])
                    c.scr_buf[3, x + 1, y + 1] = GLubyte(color[3])
                end
            end
        end
    end
    c.dirty_characters .= false
    c.dirty_character_defs .= false
    c.all_dirty = false
    c.needs_update[] = false
    println("dirty rects after update: ", c.dirty_rects)
end

struct Close <: Exception end

function draw_rect(id, x, y, w, h, pixels)
    ImGuiOpenGLBackend.glBindTexture(GL_TEXTURE_2D, ImGuiOpenGLBackend.g_ImageTexture[id])
    ImGuiOpenGLBackend.glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, GLsizei(w), GLsizei(h), ImGuiOpenGLBackend.GL_RGBA, ImGuiOpenGLBackend.GL_UNSIGNED_BYTE, pixels)
end

function draw_screen(mach::Machine)
    state = c64(mach)
    !state.needs_update[] && !state.has_dirty_rects[] &&
        return
    video(state) do
        state.needs_update[] &&
            update_screen(mach)
        if state.has_dirty_rects[]
            image_buf = state.scr_buf
            image_id = state.scr_id
            pix = state.scratch_buf
            for r in state.dirty_rects
                count = 1
                for y = r.y:bottom(r), x = r.x:right(r)
                    for c in 1:3
                        pix[c, count] = image_buf[c, x + 1, y + 1]
                    end
                    count += 1
                end
                ImGuiOpenGLBackend.ImGui_ImplOpenGL3_UpdateImageTexture(state.scr_id, image_buf, scr_width, scr_height)
                draw_rect(image_id, r.x, r.y, r.w + 1, r.h + 1, pix)
            end
            empty!(state.dirty_rects)
            state.has_dirty_rects[] = false
        end
    end
end

function draw_ui(mach::Machine, width, height)
    local state = c64(mach)
    local nodeco =
        ImGuiWindowFlags_NoDecoration |
        ImGuiWindowFlags_NoSavedSettings |
        ImGuiWindowFlags_NoDocking
    local style = unsafe_load(CImGui.GetStyle())
    local pad = style.WindowPadding.y
    local ispacing = style.ItemSpacing.y
    local fpad = style.FramePadding.y
    local fborder = style.FrameBorderSize
    local space = pad + ispacing + fpad + fborder

    if CImGui.Begin("Screen", Ptr{Nothing}(0), nodeco,)
        CImGui.SetWindowPos((0, 0))
        CImGui.SetWindowSize(ImVec2(width, height))
        draw_screen(mach)
        sz = CImGui.GetContentRegionAvail()
        antialias(false)
        CImGui.Image(Ptr{Cvoid}(state.scr_id), CImGui.ImVec2(sz.x, sz.y - GetTextLineHeight() - space))
        antialias(true)
        #table for these next three
        CImGui.SetNextItemWidth(-1)
        CImGui.BeginTable("sliderrow", 3, ImGuiTableFlags_NoPadInnerX | ImGuiTableFlags_NoPadOuterX | ImGuiTableFlags_SizingFixedFit)
        CImGui.TableSetupColumn("col1", ImGuiTableColumnFlags_WidthFixed)
        CImGui.TableSetupColumn("col2", ImGuiTableColumnFlags_WidthStretch)
        CImGui.TableSetupColumn("col3", ImGuiTableColumnFlags_WidthFixed)
        CImGui.TableNextRow()
        CImGui.TableNextColumn()
#        CImGui.SetNextItemWidth(16)
        CImGui.SetNextItemWidth(-1)
        pressed = CImGui.Button("<<")
        CImGui.TableNextColumn()
        CImGui.SetNextItemWidth(-1)
        local old = state.curtime[]
        local new = Int32(old)
        @c CImGui.SliderInt("##timeslider", &new, 0, state.maxtime[])
        if old != new
            println("SLIDE TO $new")
            state.curtime[] = new
        end
        CImGui.TableNextColumn()
        #CImGui.SetNextItemWidth(16)
        CImGui.SetNextItemWidth(-1)
        pressed = CImGui.Button(">>")
        CImGui.EndTable()
    end
    CImGui.End()
end

function antialias(enable)
    style = GetStyle()
    style.AntiAliasedLines = enable
    style.AntiAliasedLinesUseTex = enable
    style.AntiAliasedFill = enable
end

function with_imgui(func::Function, init::Function)
    global revising

    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
    if Sys.isapple()
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE) # 3.2+ only
        glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE) # required on Mac
    end
    # create window
    window = glfwCreateWindow(1280, 720, "C64", C_NULL, C_NULL)
    @assert window != C_NULL
    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)  # enable vsync
    # create OpenGL and GLFW context
    window_ctx = ImGuiGLFWBackend.create_context(window)
    gl_ctx = ImGuiOpenGLBackend.create_context()
    # setup Dear ImGui context
    ctx = CImGui.CreateContext()
    # setup Dear ImGui style
    CImGui.StyleColorsDark() #.StyleColorsClassic, StyleColorsLight
    # setup Platform/Renderer bindings
    ImGuiGLFWBackend.init(window_ctx)
    ImGuiOpenGLBackend.init(gl_ctx)
    try
        clear_color = Cfloat[0.45, 0.55, 0.60, 1.00]
        init()
        println("pad: ", unsafe_load(CImGui.GetStyle().WindowPadding), " line height: ", GetTextLineHeight(), " with spacing: ", GetTextLineHeightWithSpacing)
        while glfwWindowShouldClose(window) == 0
            revising && revise()
            glfwPollEvents()
            # start the Dear ImGui frame
            ImGuiOpenGLBackend.new_frame(gl_ctx)
            ImGuiGLFWBackend.new_frame(window_ctx)
            CImGui.NewFrame()
            width, height = Ref{Cint}(), Ref{Cint}() #! need helper fcn
            glfwGetFramebufferSize(window, width, height)
            if revising
                Base.invokelatest(func, width[], height[])
            else
                func(width[], height[])
            end
            # rendering
            CImGui.Render()
            glfwMakeContextCurrent(window)
            glClearColor(clear_color...)
            glClear(GL_COLOR_BUFFER_BIT)
            ImGuiOpenGLBackend.render(gl_ctx)
            if unsafe_load(igGetIO().ConfigFlags) & ImGuiConfigFlags_ViewportsEnable == ImGuiConfigFlags_ViewportsEnable
                backup_current_context = glfwGetCurrentContext()
                igUpdatePlatformWindows()
                GC.@preserve gl_ctx igRenderPlatformWindowsDefault(C_NULL, pointer_from_objref(gl_ctx))
                glfwMakeContextCurrent(backup_current_context)
            end
            glfwSwapBuffers(window)
        end
    catch e
        @error "Error in renderloop!" exception=(e,catch_backtrace())
        #Base.show_backtrace(stderr, catch_backtrace())
    finally
        ImGuiOpenGLBackend.shutdown(gl_ctx)
        ImGuiGLFWBackend.shutdown(window_ctx)
        CImGui.DestroyContext(ctx)
        glfwDestroyWindow(window)
    end
end

ticks(mach::Machine) = Fake6502m.ticks(mach.newcpu, mach.temps)

function test_c64(; revise = false)
    global revising = revise
    local mach = nothing
    local state = nothing
    local task = nothing

    function init_c64()
        mach = init()
        state = c64(mach)
        Threads.@spawn begin
            try
                @io println("CALLING ASMTEST")
                reset(mach)
                mach.cpu.s = 0xfe
                mach.newcpu.sp = 0xfe
                result, temps = call_6502(mach, :asmtest)
                @io begin
                    print("RESULT: ")
                    diag(result, temps)
                end
                
                @io println("CALLING FRTHTEST")
                reset(mach)
                mach.cpu.s = 0xfe
                mach.newcpu.sp = 0xfe
                call_frth(mach, :frthtest_def)
                @io println("RESULT: ", A(mach[:frthresult] | (UInt16(mach[mach.labels[:frthresult] + 1]) << 8)))
                
                run(mach, mach.labels[:main]; max_ticks = 10000)
                state.all_dirty = true
                update_screen(mach)
                state.running[] = false
                state.pausing[] = true
            catch err
                cb = catch_backtrace()
                use_io() do
                    @error "Error in 6502 thread" exception=(err,cb)
                end
            end
        end
        return mach
    end
    with_imgui(init_c64) do w, h
        process_io()
        draw_ui(mach, w, h)
    end
end

end # module C64
