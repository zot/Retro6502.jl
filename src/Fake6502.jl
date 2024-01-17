module Fake6502
using Printf
export reset, step

const CDIR=joinpath(dirname(@__FILE__), "..", "C")
const EDIR=joinpath(dirname(@__FILE__), "..", "examples")
const RDIR=joinpath(dirname(@__FILE__), "..", "resources")

include("emu.jl")
import .Fake6502m: BasicCpu
include("base.jl")
include("fakes.jl")
include("c64.jl")

run2(mach::Machine, sym::Symbol; max_ticks = 100) = run(mach, mach.labels[sym]; max_ticks)
function run2(mach::Machine, addr::Addr; max_ticks = 100)
    println("RESETTING")
    reset(mach)
    println("FINISHED RESETTING")
    if USE_GPL
        mach.cpu.pc = addr.value - 1
        mach.cpu.s = 0xfe
    end
    mach.newcpu.pc = addr.value - 1
    mach.newcpu.sp = 0xfe
    diag(mach)
    while mach.emu.clockticks < max_ticks
        call_step(mach)
    end
end

function speed_test()
    global mach = NewMachine()
    mach.mem[intRange(screen)] .= ' '
    labels = mach.labels
    off, total = loadprg("$EDIR/speed.prg", mach; labelfile="$EDIR/speed.labels")
    run2(mach, :endless; max_ticks = 100)
    start = time()
    run2(mach, :endless; max_ticks = 1000000)
    finish = time()
    println("One million clock cycles took $(finish - start) seconds")
end

function test()
    global mach = NewMachine()
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
