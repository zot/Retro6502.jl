module Fake6502
using Printf
#using ProfileView

export reset, step

#const USE_GPL = true
const USE_GPL = false
const CDIR=joinpath(dirname(@__FILE__), "..", "C")
const EDIR=joinpath(dirname(@__FILE__), "..", "examples")
const RDIR=joinpath(dirname(@__FILE__), "..", "resources")

include("emu.jl")
import .Fake6502m: Cpu
include("base.jl")
include("fakes.jl")
include("c64.jl")

run2(mach::Machine, sym::Symbol, max_ticks = 100) = run2(mach, mach.labels[sym], max_ticks)
function run2(mach::Machine, addr::Addr, max_ticks = 100)
    cpu = mach.newcpu
    cpu.pc = addr.value - 1
    cpu.sp = 0xfe
    local original_max = max_ticks
    println("max_ticks type: ", typeof(max_ticks))
    while cpu.clockticks6502 < max_ticks
        max_ticks -= Fake6502m.inner_step6502(cpu)
    end
    return cpu.clockticks6502 + original_max - max_ticks
end

function init_speed()
    mach = NewMachine(; user_data = nothing)
    mach.mem[intRange(screen)] .= ' '
    loadprg("$EDIR/speed.prg", mach; labelfile="$EDIR/speed.labels")
    mach
end

function speed_test(; profile = false)
    global mach = init_speed()
    println("warm up")
    run2(mach, :endless, 10000)
    println("running benchmark")
    #if profile
    #    @profview run2(mach, :endless; max_ticks = 1000000)
    #else
        start = time()
        local ticks = run2(mach, :endless, 1000000)
        finish = time()
        println("$ticks clock cycles took $(finish - start) seconds")
    #end
end

function test()
    global mach = NewMachine(; user_data = nothing)
    mach.mem[intRange(screen)] .= ' '
    mach.newcpu.memory[intRange(screen)] .= ' '
    labels = mach.labels
    off, total = loadprg("$EDIR/condensed.prg", mach; labelfile="$EDIR/condensed.labels")
    println("Loaded ", total, " bytes at 0x", string(off; base=16, pad=4), ", ", length(labels), " labels")
    print("labels:")
    for name in sort([keys(labels)...])
        @printf "\n  %04x %s" labels[name].value-1 name
    end
    println()
    addrs = Dict(addr => name for (name, addr) in labels)
    lastlabel = nothing
    labelcount = 0
    maxwid = max(0, length.(string.(keys(labels)))...)
    mach.step = function(mach::Machine)
        if USE_GPL
            label = Base.get(addrs, A(mach.cpu.pc), nothing)
        else
            label = Base.get(addrs, A(mach.newcpu.pc), nothing)
        end
        if !isnothing(label)
            if label === lastlabel
                labelcount === 0 && println("  LOOP...")
                labelcount += 1
            else
                diag(mach, rpad(string(label) * ": ", maxwid + 2))
                lastlabel = label
                labelcount = 0
            end
        end
        step(mach)
    end
    register(print_n, mach, :print_n)
    check_cpu(mach)

    println("CALLING ASMTEST")
    reset(mach)
    result = call_6502(mach, :asmtest)
    print("RESULT: ")
    diag(result)

    println("CALLING FRTHTEST")
    reset(mach)
    call_frth(mach, :frthtest_def)
    println("RESULT: ", A(mach[:frthresult] | (UInt16(mach[mach.labels[:frthresult] + 1]) << 8)))

    run(mach, labels[:main]; max_ticks = 10000)
    diag(mach)
    #display_hex(mach.mem)
    if USE_GPL
        display_chars(@view mach.mem[intRange(screen)]) do c; C64.SCREEN_CODES[c + 1]; end
    else
        display_chars(@view mach.newcpu.memory[intRange(screen)]) do c; C64.SCREEN_CODES[c + 1]; end
    end
    println("done testing, ", mach.emu.clockticks, " clock ticks")
end

end # module Fake6502
