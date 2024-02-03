module Fake6502
using Printf, StaticArrays
using ProfileCanvas

export reset, step

const K = 1024
const M = K * 1024
const G = M * 1024
const PUNCT = "[£]⭡⭠ !\"#\$%&'()*+,-./0123456789:;<=>?"
BLANKS(n) = String((c->' ').(1:n))
const SCREEN_CODES = Vector{Char}(
    '@' * String('A':'Z') * PUNCT * BLANKS(64) *
    '@' * String('a':'z') * PUNCT *
    ' ' * String('A':'Z') *
    BLANKS(255 - 219 + 1)
)
screen2ascii(char) = SCREEN_CODES[UInt8(char) + 1]
const CDIR=joinpath(dirname(@__FILE__), "..", "C")
const EDIR=joinpath(dirname(@__FILE__), "..", "examples")
const RDIR=joinpath(dirname(@__FILE__), "..", "resources")

mprint(::Any, args...) = print(args...)
mprintln(::Any, args...) = println(args...)

dbyte(byte::UInt8) = "$(rhex(byte)) '$(screen2ascii(byte))'"

status(s, mask = 0xFF) =
    join([s & (1 << (8 - i)) != 0 ? n : lowercase(n)
          for (i, n) in enumerate("NVXBDIZC") if (1 << (8 - i)) & mask != 0])

include("emu.jl")
import .Fake6502m: Cpu, Temps, setticks, ticks, pc, incpc, setpc, base_inner_step6502, ticks
include("base.jl")
include("rewinding.jl")
import .Rewinding
import .Rewinding: Rewinder
include("fakes.jl")
include("c64.jl")

run2(mach::Machine, temps::Temps, sym::Symbol, max_ticks::Int64) =
    run2(mach, temps, mach.labels[sym], max_ticks)
run2(mach::Machine, temps::Temps, addr::Addr, max_ticks::Int64) =
    run2(mach.newcpu, temps, addr, max_ticks)
function run2(cpu::Cpu, temps::Temps, addr::Addr, max_ticks::Int64)
    #cpu.pc = addr.value - 1
    temps = Temps(temps; pc = addr.value - 1)
    #@printf "pc: %04x opcode: %02x\n" cpu.pc Fake6502m.read6502(cpu, cpu.pc)
    @printf "pc: %04x opcode: %02x\n" pc(cpu, temps) Fake6502m.read6502(cpu, pc(cpu, temps))
    cpu.sp = 0xfe
    local original_max = max_ticks
    local m::Int64 = max_ticks
    temps = setticks(cpu, temps, 0)
    #println("max_ticks type: ", typeof(max_ticks))
    while ticks(cpu, temps) < m
        temps = Fake6502m.inner_step6502(cpu, temps)
    end
    return temps, ticks(cpu, temps) + original_max - m
end

Fake6502m.write6502(cpu::Cpu{Rewinder}, addr::UInt16, value::UInt8) = Rewinding.write6502(cpu.user_data, cpu, addr, value)

Fake6502m.inner_step6502(cpu::Cpu{Rewinder}, temps::Temps) = Rewinding.inner_step6502(cpu.user_data, cpu, temps)

function init_speed()
    #mach = NewMachine(; user_data = nothing)
    rew = Rewinder()
    mach = NewMachine(; user_data = rew)
    Rewinding.init(rew, mach.newcpu, mach.temps)
    mach.mem[intRange(screen)] .= ' '
    loadprg("$EDIR/speed.prg", mach; labelfile="$EDIR/speed.labels")
    mach
end

function speed_test(; profile = :none)
    global mach = init_speed()
    local ticks
    local temps = Temps(mach.newcpu)
    println("warm up")
    run2(mach, temps, :endless, 10000)
    run2(mach, temps, :endless, 10000)
    run2(mach, temps, :endless, 10000)
    println("running benchmark")
    if profile == :alloc
        #error("No profiler installed")
        @profview_allocs run2(mach, temps, :endless, 10000000)
    elseif profile == :cpu
        #error("No profiler installed")
        @profview run2(mach, temps, :endless, 100000)
    else
        for i in 1:15
            start = time()
            temps, ticks = run2(mach, temps, :endless, 1000000)
            finish = time()
            @printf "%2d: %d clock cycles took %lf milliseconds\n" i ticks (finish - start) * 1000
            local trail = mach.newcpu.user_data
            println("Trail len: ", (length(trail.trails) -1) * Rewinding.SNAPLEN + trail.nextfree - 1)
        end
    end
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
        label = Base.get(addrs, A(pc(mach.newcpu, mach.temps)), nothing)
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
    println("CALLING ASMTEST")
    reset(mach)
    result, temps = call_6502(mach, :asmtest)
    print("RESULT: ")
    diag(result, temps)
    println("CALLING FRTHTEST")
    reset(mach)
    call_frth(mach, :frthtest_def)
    println("RESULT: ", A(mach[:frthresult] | (UInt16(mach[mach.labels[:frthresult] + 1]) << 8)))
    run(mach, labels[:main]; max_ticks = 10000)
    diag(mach)
    #display_hex(mach.mem)
    display_chars(@view mach.newcpu.memory[intRange(screen)]) do c; C64.SCREEN_CODES[c + 1]; end
    println("done testing, ", mach.emu.clockticks, " clock ticks")
end

end # module Fake6502
