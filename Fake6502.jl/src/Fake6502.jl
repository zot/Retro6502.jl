module Fake6502
using Printf
export reset, step

include("base.jl")
include("c64.jl")

function test()
    global mach = NewMachine()
    mach.mem[intRange(screen)] .= ' '
    labels = mach.labels
    off, total = loadprg("a.out", mach; labelfile="condensed.labels")
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
    end
    run(mach, labels[:main]; max_ticks = 10000)
    diag(mach)
    #display_hex(mach.mem)
    display_chars(@view mach.mem[intRange(screen)]) do c; C64.SCREEN_CODES[c + 1]; end
    println("done testing, ", mach.emu.clockticks, " clock ticks")
end

end # module Fake6502
