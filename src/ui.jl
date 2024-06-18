module UI

using Revise
using ..Fake6502: C64, Rewinding, Fake6502m, K, Addr, A, ROM, mprint, mprintln, Machine, call_6502, call_frth,
    diag, reset, run
using .Fake6502m: Cpu
using .C64: C64, SCREEN_WIDTH, SCREEN_HEIGHT, scr_width, scr_height, COLOR_DEFS, KEYBOARD_INPUTS, use_io,
    load_condensed, init, c64, C64_machine, screen_mem, character_mem, C64_PALETTE, BG0, COLOR_MEM, @io,
    video, Rect, bottom, right, intersects, merge, pause, setrevising, process_io
using .Rewinding: Rewinder, RewindSession

using CImGui:
    GetTextLineHeight,
    GetTextLineHeightWithSpacing,
    GetStyle,
    SetNextWindowFocus,
    SetKeyboardFocusHere,
    SetItemDefaultFocus
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
#using ProfileCanvas
using Base.Threads

const KEYS = Dict(
    "A" => ImGuiKey_A,
    "B" => ImGuiKey_B,
    "C" => ImGuiKey_C,
    "D" => ImGuiKey_D,
    "E" => ImGuiKey_E,
    "F" => ImGuiKey_F,
    "G" => ImGuiKey_G,
    "H" => ImGuiKey_H,
    "I" => ImGuiKey_I,
    "J" => ImGuiKey_J,
    "K" => ImGuiKey_K,
    "L" => ImGuiKey_L,
    "M" => ImGuiKey_M,
    "N" => ImGuiKey_N,
    "O" => ImGuiKey_O,
    "P" => ImGuiKey_P,
    "Q" => ImGuiKey_Q,
    "R" => ImGuiKey_R,
    "S" => ImGuiKey_S,
    "T" => ImGuiKey_T,
    "U" => ImGuiKey_U,
    "V" => ImGuiKey_V,
    "W" => ImGuiKey_W,
    "X" => ImGuiKey_X,
    "Y" => ImGuiKey_Y,
    "Z" => ImGuiKey_Z,
    "0" => ImGuiKey_0,
    "1" => ImGuiKey_1,
    "2" => ImGuiKey_2,
    "3" => ImGuiKey_3,
    "4" => ImGuiKey_4,
    "5" => ImGuiKey_5,
    "6" => ImGuiKey_6,
    "7" => ImGuiKey_7,
    "8" => ImGuiKey_8,
    "9" => ImGuiKey_9,
    "0" => ImGuiKey_0,
    "STOP" => ImGuiKey_Escape,
    "COMMODORE" => ImGuiKey_LeftAlt,
    " " => ImGuiKey_Space,
    "CTRL" => ImGuiKey_LeftCtrl,
    "LSHIFT" => ImGuiKey_LeftShift,
    "RSHIFT" => ImGuiKey_RightShift,
    "HOME" => ImGuiKey_Home,
    "CRSR DN" => ImGuiKey_DownArrow,
    "CRSR RT" => ImGuiKey_LeftArrow,
    "HOME" => ImGuiKey_Home,
    "RETURN" => ImGuiKey_Enter,
    "DELETE" => ImGuiKey_Delete,
    "F1" => ImGuiKey_F1,
    "F3" => ImGuiKey_F3,
    "F5" => ImGuiKey_F5,
    "F7" => ImGuiKey_F7,
    "BACK" => ImGuiKey_Backspace,
    "/" => ImGuiKey_Slash,
    "^" => (:shift, ImGuiKey_6),
    "=" => ImGuiKey_Equal,
    ";" => ImGuiKey_Semicolon,
    "*" => (:shift, ImGuiKey_8),
    "\$" => (:shift, ImGuiKey_4),
    "," => ImGuiKey_Comma,
    "@" => ImGuiKey_2,
    ":" => (:shift, ImGuiKey_Semicolon),
    "." => ImGuiKey_Period,
    "-" => ImGuiKey_Minus,
    "+" => (:shift, ImGuiKey_Equal),
)
const SHIFT_KEYS = Dict(
    ImGuiKey_UpArrow => "CRSR DN",
    ImGuiKey_LeftArrow => "CRSR RT",
    ImGuiKey_F2 => "F1",
    ImGuiKey_F4 => "F3",
    ImGuiKey_F6 => "F5",
    ImGuiKey_F8 => "F7",
)
const K_COMMODORE = ImGuiKey_LeftAlt
const K_SPACE = ImGuiKey_Space
const K_CTRL = ImGuiKey_LeftCtrl
const K_SLASH = ImGuiKey_Slash
const K_SEMICOLON = ImGuiKey_Semicolon
const K_EQUAL = ImGuiKey_Equal
const K_RSHIFT = ImGuiKey_RightShift
const K_HOME = ImGuiKey_Home
const K_COMMA = ImGuiKey_Comma
const KEYBOARD_COORDS = Dict(
    key => (row - 1, col - 1) for
    (col, colkeys) in enumerate(reverse(eachcol(KEYBOARD_INPUTS))) for
    (row, key) in enumerate(colkeys)
)
const SHIFT_COORDS = KEYBOARD_COORDS["LSHIFT"]
im_key(k::LibCImGui.ImGuiKey) = (k,)
function im_key((s, k)::Tuple{Symbol,LibCImGui.ImGuiKey})
    s != :shift && error("Bad modifier: $s")
    (ImGuiKey_LeftShift, k)
end
const IM_INPUTS = Dict{Any,Any}(im_key(imkeys) => (c64key,) for (c64key, imkeys) in KEYS)

"""
C64 IO routine
"""
function c64_io(cpu::Cpu{C64_machine})
    unsafe_load(CImGui.GetIO().WantCaptureKeyboard) && return
    local c = c64(cpu)
    empty!(c.pressed_keys)
    # read a keyboard value and write it to memory
    for (imkeys, c64keys) in IM_INPUTS
        if all(CImGui.IsKeyPressed, imkeys)
            push!(c.pressed_keys, c64keys...)
            println("PRESSED: $c64keys")
        end
    end
    for k in ImGuiKey.(ImGuiKey_NamedKey_BEGIN:ImGuiKey_NamedKey_END-1)
        #local d = unsafe_load(CImGui.GetKeyData(k))

        #d.DownDuration < 0 &&
        #    continue
        #if CImGui.IsKeyPressed(k, false)
        !CImGui.IsKeyDown(k) && continue
        println("KEY DOWN: $k")
        #println("KEY DOWN: $k, Data: $d")
        #println(d.Down)
        #elseif d.Down
        #    println("ELSE KEY DOWN: $k, Data: $d")
        #end
    end
end

function merge_dirty(mach::C64_machine, rect::Rect)
    dirty = mach.dirty_rects
    changed = true
    while changed
        changed = false
        for i = length(dirty):-1:1
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

function update_screen(mach::Machine)
    c = c64(mach)
    all_dirty = c.all_dirty
    dirtydefs = Set(i for (i, d) in enumerate(c.dirty_character_defs) if d)
    scr_mem = screen_mem(mach)
    char_mem = character_mem(mach)
    for col = 0:39
        for row = 0:24
            i = row * 40 + col
            char = scr_mem[1+i]
            !all_dirty && !c.dirty_characters[1+i] && char ∉ dirtydefs && continue
            merge_dirty(c, Rect(col * 8, row * 8, 7, 7))
            for pixel = 0:7
                x = col * 8 + pixel
                c.multicolor && error("multicolor not supported")
                bg = C64_PALETTE[1+mach[BG0]]
                fg = C64_PALETTE[1+mach[COLOR_MEM+i]]
                for rowbyte = 0:7
                    y = row * 8 + rowbyte
                    pixels = char_mem[1+8*char+rowbyte]
                    color = (pixels >> (7 - pixel)) & 1 == 1 ? fg : bg
                    c.scr_buf[1, x+1, y+1] = GLubyte(color[1])
                    c.scr_buf[2, x+1, y+1] = GLubyte(color[2])
                    c.scr_buf[3, x+1, y+1] = GLubyte(color[3])
                end
            end
        end
    end
    c.dirty_characters .= false
    c.dirty_character_defs .= false
    c.all_dirty = false
    c.needs_update[] = false
    @io println("dirty rects after update: ", c.dirty_rects)
end

struct Close <: Exception end

function draw_rect(id, x, y, w, h, pixels)
    ImGuiOpenGLBackend.glBindTexture(GL_TEXTURE_2D, ImGuiOpenGLBackend.g_ImageTexture[id])
    ImGuiOpenGLBackend.glTexSubImage2D(
        GL_TEXTURE_2D,
        0,
        x,
        y,
        GLsizei(w),
        GLsizei(h),
        ImGuiOpenGLBackend.GL_RGBA,
        ImGuiOpenGLBackend.GL_UNSIGNED_BYTE,
        pixels,
    )
end

function draw_screen(mach::Machine)
    state = c64(mach)
    !state.needs_update[] && !state.has_dirty_rects[] && return
    video(state) do
        state.needs_update[] && update_screen(mach)
        if state.has_dirty_rects[]
            image_buf = state.scr_buf
            image_id = state.scr_id
            pix = state.scratch_buf
            for r in state.dirty_rects
                count = 1
                for y = r.y:bottom(r), x = r.x:right(r)
                    for c = 1:3
                        pix[c, count] = image_buf[c, x+1, y+1]
                    end
                    count += 1
                end
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

    if CImGui.Begin("Screen", Ptr{Nothing}(0), nodeco)
        CImGui.SetWindowPos((0, 0))
        CImGui.SetWindowSize(ImVec2(width, height))
        draw_screen(mach)
        sz = CImGui.GetContentRegionAvail()
        antialias(false)
        CImGui.Image(
            Ptr{Cvoid}(state.scr_id),
            CImGui.ImVec2(sz.x, sz.y - GetTextLineHeight() - space),
        )
        antialias(true)
        #table for these next three
        CImGui.SetNextItemWidth(-1)
        CImGui.BeginTable(
            "sliderrow",
            3,
            ImGuiTableFlags_NoPadInnerX |
            ImGuiTableFlags_NoPadOuterX |
            ImGuiTableFlags_SizingFixedFit,
        )
        CImGui.TableSetupColumn("col1", ImGuiTableColumnFlags_WidthFixed)
        CImGui.TableSetupColumn("col2", ImGuiTableColumnFlags_WidthStretch)
        CImGui.TableSetupColumn("col3", ImGuiTableColumnFlags_WidthFixed)
        CImGui.TableNextRow()
        CImGui.TableNextColumn()
        local old = state.curtime[]
        local new = Int32(old)
        CImGui.SetNextItemWidth(-1)
        if CImGui.Button("<<")
            new -= 0x1
        end
        CImGui.TableNextColumn()
        CImGui.SetNextItemWidth(-1)
        @c CImGui.SliderInt("##timeslider", &new, 0, state.maxtime[])
        CImGui.TableNextColumn()
        CImGui.SetNextItemWidth(-1)
        if CImGui.Button(">>")
            new += 0x1
        end
        CImGui.EndTable()
        try
            if old != new
                state.curtime[] = new
                Rewinding.update_undo_session(state.rewinder, state.session)
                pause(mach.newcpu.user_data) do
                    while state.session.curtime > new
                        Rewinding.back(mach.newcpu, state.rewinder, state.session)
                    end
                    while state.session.curtime < new
                        Rewinding.forward(mach.newcpu, state.rewinder, state.session)
                    end
                end
            end
        catch err
            @error "Error sliding in time" exception = (err, catch_backtrace())
            state.curtime[] = old
        end
    end
    CImGui.End()
    c64_io(mach.newcpu)
end

function antialias(enable)
    style = GetStyle()
    style.AntiAliasedLines = enable
    style.AntiAliasedLinesUseTex = enable
    style.AntiAliasedFill = enable
end

function with_imgui(func::Function, init::Function)
    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
    if Sys.isapple()
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE) # 3.2+ only
        glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE) # required on Mac
    end
    # create window
    window = glfwCreateWindow(1280, 720, "Retro6502: C64", C_NULL, C_NULL)
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
        local mach = init()
        update_screen(mach)
        println(
            "pad: ",
            unsafe_load(CImGui.GetStyle().WindowPadding),
            " line height: ",
            GetTextLineHeight(),
            " with spacing: ",
            GetTextLineHeightWithSpacing,
        )
        while glfwWindowShouldClose(window) == 0
            C64.revising && revise()
            glfwPollEvents()
            # start the Dear ImGui frame
            ImGuiOpenGLBackend.new_frame(gl_ctx)
            ImGuiGLFWBackend.new_frame(window_ctx)
            CImGui.NewFrame()
            width, height = Ref{Cint}(), Ref{Cint}() #! need helper fcn
            glfwGetFramebufferSize(window, width, height)
            if C64.revising
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
            if unsafe_load(igGetIO().ConfigFlags) & ImGuiConfigFlags_ViewportsEnable ==
               ImGuiConfigFlags_ViewportsEnable
                backup_current_context = glfwGetCurrentContext()
                igUpdatePlatformWindows()
                GC.@preserve gl_ctx igRenderPlatformWindowsDefault(
                    C_NULL,
                    pointer_from_objref(gl_ctx),
                )
                glfwMakeContextCurrent(backup_current_context)
            end
            glfwSwapBuffers(window)
        end
    catch e
        @error "Error in renderloop!" exception = (e, catch_backtrace())
        #Base.show_backtrace(stderr, catch_backtrace())
    finally
        ImGuiOpenGLBackend.shutdown(gl_ctx)
        ImGuiGLFWBackend.shutdown(window_ctx)
        CImGui.DestroyContext(ctx)
        glfwDestroyWindow(window)
    end
end

function test_c64(load = load_condensed; revise = false, verbose = false)
    setrevising(revise)
    local mach = nothing
    local state = nothing
    local task = nothing

    function init_c64()
        mach = init(load)
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
                @io println(
                    "RESULT: ",
                    A(mach[:frthresult] | (UInt16(mach[mach.labels[:frthresult]+1]) << 8)),
                )

                run(mach, mach.labels[:main]; max_ticks = 10000)
                state.all_dirty = true
                update_screen(mach)
                state.running[] = false
                state.pausing[] = true
            catch err
                cb = catch_backtrace()
                use_io() do
                    @error "Error in 6502 thread" exception = (err, cb)
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

function __init__()
    for (imkey, c64key) in SHIFT_KEYS
        IM_INPUTS[im_key(imkey)] = ("LSHIFT", c64key)
    end
end

end
