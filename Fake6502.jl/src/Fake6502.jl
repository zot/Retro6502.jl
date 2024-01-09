module Fake6502
using Printf
export reset, step

const lib = Base.Libc.Libdl.dlopen(joinpath(dirname(@__FILE__), "..", "..", "fake6502.so"))
const fake6502_init = Base.Libc.Libdl.dlsym(lib, :fake6502_init)
const fake6502_reset = Base.Libc.Libdl.dlsym(lib, :fake6502_reset)
const fake6502_step = Base.Libc.Libdl.dlsym(lib, :fake6502_step)
const K = 1024
const screen = 1025:2024

@kwdef struct CpuState
    a::UInt8 = 0
    x::UInt8 = 0
    y::UInt8 = 0
    flags::UInt8 = 0
    pc::UInt16 = 0
    s::UInt8 = 0
end

@kwdef struct EmuState
    instructions::Int32 = 0
    clockticks::Int32 = 0
    ea::UInt16 = 0
    opcode::UInt8 = 0
end

@kwdef struct Context
    cpu::CpuState = CpuState()
    emu::EmuState = EmuState()
    mach::Any
end

"""
    Access a field using a pointer of a particular type using `get(a, ptr)` and `set!(a, ptr, value)`

Also, extend access using properties and indices like: `Access(Context).cpu.x`
"""
struct Access{Base, Leaf}
    offset::UInt
    Access(type::Type) = new{type, type}(0)
    Access{T, L}(offset::UInt) where {T, L} = new{T, L}(offset)
end

function Access(a::Access{Base}, field::Symbol) where Base
    T = type(a)
    for i in 1:fieldcount(T)
        name = fieldname(T, i)
        name === field &&
            return Access{Base, fieldtype(T, i)}(offset(a) + fieldoffset(T, i))
    end
    error("No field $field in type $T")
end

type(::Access{T, L}) where {T, L} = L
offset(a::Access) = getfield(a, :offset)
get(a::Access{T, L}, ptr::Ptr{T}) where {T, L} = unsafe_load(Ptr{L}(ptr) + offset(a), 1)
get(::Access{T}, ::Ptr{U}) where {T, U} =
    error("Unsafe get: accessor for $T is not compatible with pointer to $U")
set!(a::Access{T, L}, ptr::Ptr{T}, value) where {T, L} =
    unsafe_store!(Ptr{L}(ptr) + offset(a), convert(L, value), 1)
set!(::Access{T}, ::Ptr{U}, value) where {T, U} =
    error("Unsafe set: accessor for $T is not compatible with pointer to $U")

"""
    Access fields based on a pointer
The Accessor is a proxy for the structure and overrides property access

# Examples
```
person = Accessor(person_ptr)
println(person.id)
person.name = "fred"
```
"""
struct Accessor{T, L}
    access::Access{T, L}
    ptr::Ptr{T}
    function Accessor(prev::Accessor{T}, field::Symbol) where T
        newa = Access(access(prev), field)
        new{T, type(newa)}(newa, ptr(prev))
    end
    Accessor(ptr::Ptr{T}) where T = new{T, T}(Access(T), ptr)
end

function Base.getproperty(a::Accessor, prop::Symbol)
    a = Accessor(a, prop)
    t = type(a)
    return isprimitivetype(t) || t <: AbstractString || t <: AbstractArray ? get(a) : a
end
Base.setproperty!(a::Accessor, prop::Symbol, value) = set!(Accessor(a, prop), value)

offset(a::Accessor, field::Symbol) = offset(access(Accessor(a, field)))
access(a::Accessor) = getfield(a, :access)
type(::Accessor{T, L}) where {T, L} = L
ptr(a::Accessor) = getfield(a, :ptr)
get(a::Accessor) = get(access(a), ptr(a))
set!(a::Accessor, value) = set!(access(a), ptr(a), value)
"Create an Accessor with the same path but a different pointer"
withptr(a::Accessor{T}, ptr::Ptr{T}) where T = Accessor(access(a), ptr)
withptr(::Accessor{T}, ::Ptr{U}) where {T, U} =
    error("Unsafe set: accessor for $T is not compatible with pointer to $U")

mutable struct Machine
    ctx::Ptr{Context}
    cpu::Accessor{Context}
    emu::Accessor{Context}
    mem::Vector{UInt8}
    read_mem::Function
    write_mem::Function
    Machine() = new()
end

### begin system hooks

Base.getindex(mach::Machine, addr::UInt16) = mach.mem[addr + 1]
Base.setindex!(mach::Machine, byte::UInt8, addr::UInt16) = mach.mem[addr + 1] = byte

Base.getindex(ctx::Context, addr::UInt16) = ctx.mach[addr]
Base.setindex!(ctx::Context, byte::UInt8, addr::UInt16) = ctx.mach[addr] = byte

Base.getindex(ctx::Ptr{Context}, addr::UInt16) = unsafe_load(ctx)[addr]
Base.setindex!(ctx::Ptr{Context}, byte::UInt8, addr::UInt16) = unsafe_load(ctx)[addr] = byte
Base.getproperty(ctx::Ptr{Context}, prop::Symbol) = getproperty(unsafe_load(ctx), prop)

Base.propertynames(::Accessor{T, L}) where {T, L} = Base.fieldnames(L)
Base.getproperty(a::Access, prop::Symbol) = Access(a, prop)
Base.setproperty!(a::Access, prop::Symbol, value) = set!(getproperty(a, prop), value)

# these might work but they are untested
#Base.getindex(a::Access, key::Integer) = index(a, key)
#Base.setindex!(a::Access, value, key::Integer) = index!(a, value, key)
#index(a::Access, ind::Integer) = index(a, type(a), ind)
#index(a::Access{Base}, ::Type{Vector{T}}, ind::Integer) where {T, Base} =
#    Access{Base}(T, offset(a) + sizeof(T) * ind)
#index!(a::Access, value, ind::Integer) = index!(a, type(a), value, ind)
#index!(a::Access{Base}, ::Type{Vector{T}}, value::T, ind::Integer) where {T, Base} =
#    set!(Access{Base}(T, offset(a) + sizeof(T) * ind), value)
#"Convert values that don't quite fit"
#index!(a::Access, t::Type{Vector{T}}, value, ind) where T =
#    index!(a, t, convert(T, value), ind)

### end system hooks

count = 0

# high level machine hooks
function read_mem(mach::Machine, addr::UInt16)::UInt8
    #println("READ 0x", string(addr; base=16, pad=4))
    return mach[addr]
end

function write_mem(mach::Machine, addr::UInt16, byte::UInt8)
    #@printf "WRITE %04x = %02x\n" addr byte
    mach[addr] = byte
end

# primitive ctx hooks
function read_mem(ctx::Ptr{Context}, addr::UInt16)::UInt8
    local mach = ctx.mach
    return mach.read_mem(mach, addr)
end

function write_mem(ctx::Ptr{Context}, addr::UInt16, byte::UInt8)::Cvoid
    local mach = ctx.mach
    mach.write_mem(mach, addr, byte)
    nothing
end

const c_read_mem = @cfunction(read_mem, Cuchar, (Ptr{Context}, Cushort))
const c_write_mem = @cfunction(write_mem, Cvoid, (Ptr{Context}, Cushort, Cuchar))

function NewMachine(; read_func = read_mem, write_func = write_mem)
    local machine = Machine()
    machine.mem = zeros(UInt8, 64K)
    machine.read_mem = read_func
    machine.write_mem = write_func
    machine.ctx = @ccall $fake6502_init(c_read_mem::Ptr{Cvoid}, c_write_mem::Ptr{Cvoid}, machine::Ref{Machine})::Ptr{Context}
    machine.cpu = Accessor(machine.ctx).cpu
    machine.emu = Accessor(machine.ctx).emu
    machine
end

run(step_func::Function, mach::Machine, addr::Integer; max_ticks = 100) =
    run(mach, UInt16(addr); max_ticks, step_func)

run(mach::Machine, addr::Integer; max_ticks = 100) = run(mach, UInt16(addr); max_ticks)

diag(mach::Machine) = @printf "pc = %04x s = %02x ticks = %d\n" mach.cpu.pc mach.cpu.s mach.emu.clockticks

run(step_func::Function, mach::Machine, addr::UInt16; max_ticks = 100) =
    run(mach, addr; max_ticks, step_func)

function run(mach::Machine, addr::UInt16; max_ticks = 100, step_func = step)
    println("RESETTING")
    reset(mach)
    println("FINISHED RESETTING")
    mach.cpu.pc = addr
    mach.cpu.s = 0xfe
    @printf "cpu.a: %d cpu.x: %d cpu.y: %d cpu.flags: %d cpu.s: %d cpu.pc: %d\n" offset(mach.cpu, :a) offset(mach.cpu, :x) offset(mach.cpu, :y) offset(mach.cpu, :flags) offset(mach.cpu, :s) offset(mach.cpu, :pc)
    #diag(mach)
    while mach.emu.clockticks < max_ticks && mach.cpu.s != 0
        step_func(mach)
        #diag(mach)
    end
end

loadprg(filename, mach::Machine; labelfile="") = loadprg(filename, mach.mem; labelfile)
function loadprg(filename, mem::Vector{UInt8}; labelfile="")
    total = 1
    off = 0
    open(filename, "r") do io
        off = read(io, UInt16)
        while true
            buf = @view mem[off+total:end]
            len = readbytes!(io, buf, length(buf))
            len <= 0 && break
            total += len
        end
    end
    labels = Dict()
    if isfile(labelfile)
        open(labelfile, "r") do io
            for line in readlines(io)
                (_, addr, name) = split(line)
                labels[name[2:end]] = parse(Int, addr; base = 16)
            end
        end
    end
    return off, total-1, labels
end

reset(mach::Machine) = @ccall $fake6502_reset(mach.ctx::Ptr{Context})::Cvoid

step(mach::Machine) = @ccall $fake6502_step(mach.ctx::Ptr{Context})::Cvoid

function display_hex(mem::Vector{UInt8})
    area = mem[screen]
    for i in 1:40:length(screen)
        println(lpad(i:i+39, 10), ": ", join((b-> string(b; base=16, pad=2)).(area[i:i+39]), " "))
    end
end

function display_chars(mem::Vector{UInt8})
    area = mem[screen]
    println("+", join((x->"-").(1:40), ""), "+")
    for i in 1:40:length(screen)
        println("|", String(area[i:i+39]), "|")
    end
    println("+", join((x->"-").(1:40), ""), "+")
end

const CONDENSE_START = 2850

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
    display_chars(mach.mem)
    println("done testing, ", mach.emu.clockticks, " clock ticks")
end

end # module Fake6502
