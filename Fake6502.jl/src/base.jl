using Printf
export reset, step

# 1-based addresses
A(x::Integer) = x + 1
A(v::AbstractVector) = v .+ 1

const lib = Base.Libc.Libdl.dlopen(joinpath(dirname(@__FILE__), "..", "..", "fake6502.so"))
const fake6502_init = Base.Libc.Libdl.dlsym(lib, :fake6502_init)
const fake6502_reset = Base.Libc.Libdl.dlsym(lib, :fake6502_reset)
const fake6502_step = Base.Libc.Libdl.dlsym(lib, :fake6502_step)
const K = 1024
const SCREEN_LEN = 40 * 25
const screen = A(0x0400:0x07FF)
const colors = A(0xD800:0xDBFF) # color is bits 8-11, color 1 for each character (color 3 in multicolor)
const ROM_FILES = Dict(
    A(0x1000:0x1FFF) => "resources/characters.bin", # visible only to VIC-II
    A(0x9000:0x9FFF) => "resources/characters.bin", # visible only to VIC-II
    [A(0xA000:0xBFFF), A(0xE000:0xFFFF)] => "resources/basic-kernal.bin", # visible by default
    A(0xD000:0xDFFF) => "resources/characters.bin", # not visible by default
)
const ROM = zeros(UInt8, 64K)
rom_initialized = false
scanline_counter = 0

@kwdef struct CpuState
    a::UInt8 = 0
    x::UInt8 = 0
    y::UInt8 = 0
    flags::UInt8 = 0
    s::UInt8 = 0
    pc::UInt16 = 0
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
    properties::Dict{Any,Any}
    Machine() = new()
end

### begin system hooks

#Base.getindex(mach::Machine, addr::UInt16) = mach.mem[addr + 1]
#Base.setindex!(mach::Machine, byte::UInt8, addr::UInt16) = mach.mem[addr + 1] = byte
Base.getindex(mach::Machine, addr::Integer) = mach.mem[addr + 1]
Base.setindex!(mach::Machine, byte::UInt8, addr::Integer) = mach.mem[addr + 1] = byte

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

function init_rom()
    rom_initialized && return
    for (addr, file) in ROM_FILES
        open(file, "r") do io
            ranges = addr isa AbstractRange ? [addr] : addr
            for range in ranges
                readbytes!(io, @view(ROM[range]), length(range))
            end
        end
    end
end

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
    println("c_read_mem ", c_read_mem)
    println("c_write_mem ", c_write_mem)
    machine.ctx = @ccall $fake6502_init(c_read_mem::Ptr{Cvoid}, c_write_mem::Ptr{Cvoid}, machine::Ref{Machine})::Ptr{Context}
    println("CTX ", machine.ctx)
    machine.cpu = Accessor(machine.ctx).cpu
    machine.emu = Accessor(machine.ctx).emu
    machine.properties = Dict{Any, Any}()
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
    for i in 1:40:SCREEN_LEN
        println(lpad(i:i+39, 10), ": ", join((b-> string(b; base=16, pad=2)).(area[i:i+39]), " "))
    end
end

display_chars(screen_mem::Vector{UInt8}) = display_chars(identity, screen_mem)

function display_chars(cvt::Function, screen_mem::AbstractVector{T}) where {T <: UInt8}
    println("+", join((x->"-").(1:40), ""), "+")
    for i in 1:40:SCREEN_LEN
        println("|", String(cvt.(screen_mem[i:i+39])), "|")
    end
    println("+", join((x->"-").(1:40), ""), "+")
end

const CONDENSE_START = 2850
