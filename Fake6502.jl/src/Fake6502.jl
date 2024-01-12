module Fake6502
using Printf
export reset, step

include("base.jl")
include("c64.jl")

function test()
    global mach = NewMachine()
    mach.mem[screen] .= ' '
    off, total, labels = loadprg("a.out", mach; labelfile="condensed.labels")
    println("Loaded ", total, " bytes at 0x", string(off; base=16, pad=4), ", ", length(labels), " labels")
    print("labels:")
    for name in sort([keys(labels)...])
        @printf "\n  %04x %s" labels[name] name
    end
    println()
    addrs = Dict(UInt16(addr) => name for (name, addr) in labels)
    lastlabel = nothing
    labelcount = 0
    maxwid = max(length.(keys(labels))...)
    run(mach, Base.get(labels, "main", CONDENSE_START); max_ticks = 10000) do mach
        label = Base.get(addrs, mach.cpu.pc, nothing)
        if !isnothing(label)
            if label === lastlabel
                labelcount === 0 && println("  LOOP...")
                labelcount += 1
            else
                print(rpad(label * ": ", maxwid + 2))
                lastlabel = label
                labelcount = 0
                diag(mach)
            end
        end
        step(mach)
    end
    diag(mach)
    #display_hex(mach.mem)
    display_chars(@view mach.mem[screen])
    println("done testing, ", mach.emu.clockticks, " clock ticks")
end

end # module Fake6502
