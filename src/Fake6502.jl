"""
6502 8-bit microprocessor emulator and tools
"""
module Fake6502

using Printf, StaticArrays
#using ProfileCanvas
import Base.to_index

export reset, step

const K = 1024
const M = K * 1024
const G = M * 1024
const PUNCT = "[£]⭡⭠ !\"#\$%&'()*+,-./0123456789:;<=>?"
BLANKS(n) = String((c -> ' ').(1:n))
const SCREEN_CODES = Vector{Char}(
    '@' *
    String('A':'Z') *
    PUNCT *
    BLANKS(64) *
    '@' *
    String('a':'z') *
    PUNCT *
    ' ' *
    String('A':'Z') *
    BLANKS(255 - 219 + 1),
)
while length(SCREEN_CODES) < 256
    push!(SCREEN_CODES, ' ')
end
const ASCII_TO_SCREEN =
    Dict(v => UInt8(k - 1) for (k, v) in reverse([enumerate(SCREEN_CODES)...]))
screen2ascii(str::AbstractString) = String(screen2ascii.(Vector{UInt8}(str)))
screen2ascii(char::Integer) = SCREEN_CODES[UInt8(char)+1]
ascii2screen(str::AbstractString) = ascii2screen.(Vector{UInt8}(str))
ascii2screen(char::Union{Char,Integer}) = get(ASCII_TO_SCREEN, Char(char), 0xFF)
const CDIR = joinpath(dirname(@__FILE__), "..", "C")
const EDIR = joinpath(dirname(@__FILE__), "..", "examples")
const RDIR = joinpath(dirname(@__FILE__), "..", "resources")

mprint(::Any, args...) = print(args...)
mprintln(::Any, args...) = println(args...)

dbyte(byte::UInt8) = "$(rhex(byte)) '$(screen2ascii(byte))'"

status(s, mask = 0xFF) = join((
    s & (1 << (8 - i)) != 0 ? n : lowercase(n) for
    (i, n) in enumerate("NVXBDIZC") if (1 << (8 - i)) & mask != 0
))

struct Addr
    value::UInt32
    Addr(a::Integer) = new(a + 1)
end

struct AddrRange
    first::Addr
    last::Addr
end

# 1-based addresses
A(x::Integer) = Addr(UInt32(x))
A(r::AddrRange) = A(first(r)-1:last(r)-1)
A(v::AbstractRange) = A(first(v)):Addr(last(v))
A(v::AbstractVector) = Addr.(v)
hex(addr::Addr) = hex(UInt16(addr.value - 1))

intrange(r::AddrRange) = r.first.value:r.last.value
Base.in(addr::Addr, r::AddrRange) = addr.value ∈ intrange(r)
Base.in(r1::AddrRange, r2::AddrRange) = intersect(r1, r2) == r1
Base.:(:)(a::Addr, b::Addr) = AddrRange(a, b)
Base.first(r::AddrRange) = r.first
Base.last(r::AddrRange) = r.last
Base.length(r::AddrRange) = r.last.value - r.first.value + 1
Base.hash(r::AddrRange, h::UInt64) = Base.hash(intrange(r), h)
function Base.intersect(r1::AddrRange, r2::AddrRange)
    local sect = intersect(intrange(r1), intrange(r2))
    A(first(sect)-1:last(sect)-1)
end

Base.getindex(x::AbstractArray, r::AddrRange) = getindex(x, intrange(r))
Base.getindex(x::AbstractArray, a::Addr) = getindex(x, a.value)
Base.setindex!(x::AbstractArray, value, a::Addr) = setindex!(x, value, a.value)
Base.setindex!(x::AbstractArray, value, r::AddrRange) = setindex!(x, value, intrange(r))
Base.to_index(x::AbstractArray, r::AddrRange) = Base.to_index(x, intrange(r))
Base.to_index(::AbstractArray, a::Addr) = a.value

Base.hash(a::Addr, h::UInt64) = Base.hash(a.value, h)
Base.show(io::IO, addr::Addr) = print(io, "Addr($(hex(UInt16(addr.value - 1))))")
Base.:(>>)(a::Addr, i::UInt64) = Addr((a.value - 1) >> i)
Base.:(<<)(a::Addr, i::UInt64) = Addr((a.value - 1) << i)
Base.:(<)(a::Addr, b::Addr) = a.value < b.value
Base.:(-)(a::Addr, b::Addr) = a.value - 1 - b.value
Base.:(-)(a::Addr, i::Integer) = Addr(a.value - 1 - i)
Base.:(-)(i::Integer, a::Addr) = Addr(a.value - 1 - i)
Base.:(+)(a::Addr, i::Integer) = Addr(a.value - 1 + i)
Base.:(+)(i::Integer, a::Addr) = Addr(a.value - 1 + i)

matches(r, s) = !isnothing(match(r, s))

include("emu.jl")
import .Fake6502m: Cpu, Temps, setticks, ticks, pc, incpc, setpc, base_inner_step6502, ticks
include("base.jl")
include("rewinding.jl")
import .Rewinding
import .Rewinding: Rewinder
include("fakes.jl")
include("c64.jl")
include("ui.jl")
include("asmtools.jl")
include("asm.jl")
include("worker.jl")
include("repl.jl")
include("fakerom.jl")

using .AsmRepl: repl, CLEAR
export repl, CLEAR

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

Fake6502m.write6502(cpu::Cpu{Rewinder}, addr::UInt16, value::UInt8) =
    Rewinding.write6502(cpu.user_data, cpu, addr, value)

Fake6502m.inner_step6502(cpu::Cpu{Rewinder}, temps::Temps) =
    Rewinding.inner_step6502(cpu.user_data, cpu, temps)

function init_speed()
    #mach = NewMachine(; user_data = nothing)
    rew = Rewinder()
    mach = NewMachine(; user_data = rew)
    Rewinding.init(rew, mach.newcpu, mach.temps)
    mach.mem[intrange(screen)] .= ' '
    loadprg("$EDIR/speed.prg", mach; labelfile = "$EDIR/speed.labels")
    mach
end

function speed_test(; profile = :none)
    #global mach = init_speed()
    #local ticks
    #local temps = Temps(mach.newcpu)
    #println("warm up")
    #run2(mach, temps, :endless, 10000)
    #run2(mach, temps, :endless, 10000)
    #run2(mach, temps, :endless, 10000)
    #println("running benchmark")
    #if profile == :alloc
    #    #error("No profiler installed")
    #    @profview_allocs run2(mach, temps, :endless, 10000000)
    #elseif profile == :cpu
    #    #error("No profiler installed")
    #    @profview run2(mach, temps, :endless, 100000)
    #else
    #    for i = 1:15
    #        start = time()
    #        temps, ticks = run2(mach, temps, :endless, 1000000)
    #        finish = time()
    #        @printf "%2d: %d clock cycles took %lf milliseconds\n" i ticks (
    #            finish - start
    #        ) * 1000
    #        local trail = mach.newcpu.user_data
    #        println(
    #            "Trail len: ",
    #            (length(trail.trails) - 1) * Rewinding.SNAPLEN + trail.nextfree - 1,
    #        )
    #    end
    #end
end

function test()
    global mach = NewMachine(; user_data = nothing)
    mach.mem[intrange(screen)] .= ' '
    mach.newcpu.memory[intrange(screen)] .= ' '
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
    addrs = Dict(addr => name for (name, addr) in labels)
    lastlabel = nothing
    labelcount = 0
    maxwid = max(0, length.(string.(keys(labels)))...)
    mach.step = function (mach::Machine)
        label = get(addrs, A(pc(mach.newcpu, mach.temps)), nothing)
        if !isnothing(label)
            if label == lastlabel
                labelcount == 0 && println("  LOOP...")
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
    println(
        "RESULT: ",
        A(mach[:frthresult] | (UInt16(mach[mach.labels[:frthresult]+1]) << 8)),
    )
    run(mach, labels[:main]; max_ticks = 10000)
    diag(mach)
    #display_hex(mach.mem)
    display_chars(@view mach.newcpu.memory[intrange(screen)]) do c
        C64.SCREEN_CODES[c+1]
    end
    println("done testing, ", mach.emu.clockticks, " clock ticks")
end

function __init__()
    init_rom()
end

end # module Fake6502
