using Printf
export reset, step

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
A(v::AbstractRange) = Addr(first(v)):Addr(last(v))
A(v::AbstractVector) = Addr.(v)

intRange(r::AddrRange) = r.first.value:r.last.value
Base.in(addr::Addr, r::AddrRange) = addr.value ∈ r.first.value:r.last.value
Base.:(:)(a::Addr, b::Addr) = AddrRange(a, b)
Base.first(r::AddrRange) = r.first
Base.last(r::AddrRange) = r.last
Base.length(r::AddrRange) = r.last.value - r.first.value + 1
Base.hash(r::AddrRange, h::UInt64) = Base.hash(intRange(r), h)

Base.hash(a::Addr, h::UInt64) = Base.hash(a.value, h)
Base.show(io::IO, addr::Addr) = print(io, "Addr(0x", lpad(string(UInt16(addr.value) - 1; base=16), 4, "0"), ")")
Base.:(>>)(a::Addr, i::UInt64) = Addr((a.value - 1) >> i)
Base.:(<<)(a::Addr, i::UInt64) = Addr((a.value - 1) << i)
Base.:(<)(a::Addr, b::Addr) = a.value < b.value
Base.:(-)(a::Addr, b::Addr) = a.value - 1 - b.value
Base.:(-)(a::Addr, i::Integer) = Addr(a.value - 1 - i)
Base.:(-)(i::Integer, a::Addr) = Addr(a.value - 1 - i)
Base.:(+)(a::Addr, i::Integer) = Addr(a.value - 1 + i)
Base.:(+)(i::Integer, a::Addr) = Addr(a.value - 1 + i)

const JSR = 0x20
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
    step::Function
    properties::Dict{Any,Any}
    labels::Dict{Symbol,Addr}
    addrs::Dict{Addr,Symbol}
    fake_base::Addr
    fake_routines::Dict{Addr,Function}
    verbose::Bool
    Machine() = new()
end

### begin system hooks

Base.getindex(mach::Machine, r::AddrRange) = mach.mem[intRange(r)]
Base.getindex(mach::Machine, sym::Symbol) = mach[mach.labels[sym]]
Base.getindex(mach::Machine, addr::Integer) = mach[Addr(addr)]
Base.getindex(mach::Machine, addr::Addr) = mach.mem[addr.value]
Base.getindex(::Machine, addr) =
    error("Memory locations can only hold be accessed with symbols, integers, and addresses")
Base.setindex!(mach::Machine, byte::UInt8, sym::Symbol) = mach[mach.labels[sym]] = byte
Base.setindex!(mach::Machine, byte::UInt8, addr::Integer) = mach[Addr(addr)] = byte
Base.setindex!(mach::Machine, byte::UInt8, addr::Addr) = mach.mem[addr.value] = byte
Base.setindex!(::Machine, byte, ::Addr) = error("Memory locations can only hold bytes")
Base.setindex!(::Machine, ::UInt8, addr) =
    error("Memory locations can only hold be accessed with symbols, integers, and addresses")

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
            ranges = addr isa AddrRange ? [addr] : addr
            for range in ranges
                fst = first(range).value
                lst = last(range).value
                readbytes!(io, @view(ROM[fst:lst]), length(range))
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

hex(num::Union{UInt8, Int8}) = hex(num, 2)
hex(num::Union{UInt16,Int16}) = hex(num, 4)
hex(num::Integer, pad =
    num <= 0xFF ? 2 : 4) = "0x" * lpad(string(num; base=16), pad, "0")

function call_step(mach::Machine)
    if mach[mach.cpu.pc] === JSR
        # check for fake routine
        addr = A(mach[mach.cpu.pc + 1] + (UInt16(mach[mach.cpu.pc + 2]) << 8))
        label = Base.get(mach.addrs, addr, hex(UInt16(addr.value - 1)))
        println("JSR $label ($addr) [$(hex((addr.value - 1) & 0xFF00))]")
        if addr ∈ keys(mach.fake_routines)
            println("FAKE ROUTINE")
            mach.fake_routines[addr](mach)
            mach.cpu.pc += 3
            return
        #elseif ((addr.value - 1) & 0xFF00) == 0xFF00
        #    println("ROM INST: $(mach.read_mem(mach, UInt16(addr.value - 1)))")
        #    mach.verbose = true
        end
    end
    mach.step(mach)
    mach.verbose && diag(mach)
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

function push_stack(mach::Machine, addr::Addr)
    adr = addr.value - 1
    mach[A(0x100 + mach.cpu.s)] = UInt8(adr >> 8)
    mach[A(0x100 + mach.cpu.s - 1)] = UInt8(adr & 0xFF)
    mach.cpu.s -= 2
end

call_6502(mach::Machine, sym::Symbol) = call_6502(mach, mach.labels[sym])
function call_6502(mach::Machine, addr::Addr)
    oldstate = unsafe_load(mach.ctx)
    push_stack(mach, A(0x00))
    mach.cpu.pc = addr.value - 1
    while mach.cpu.s != oldstate.cpu.s
        call_step(mach)
    end
    newstate = unsafe_load(mach.ctx)
    unsafe_store!(mach.ctx, oldstate)
    return newstate
 end

function call_frth(mach::Machine, sym)
    # save pc
    pclo = mach[:pc]
    pchi = mach[mach.labels[:pc] + 1]
    # call frth word
    # set up pc to return to Julia
    retstub = mach.labels[:retstub].value - 1
    mach[:pc] = UInt8(retstub & 0xFF)
    mach[mach.labels[:pc] + 1] = UInt8(retstub >> 8)
    # call code
    call_6502(mach, sym)
    diag(mach)
    # restore pc
    mach[:pc] = pclo
    mach[mach.labels[:pc] + 1] = pchi
end

function NewMachine(; read_func = read_mem, write_func = write_mem, step_func = step)
    local machine = Machine()
    machine.mem = zeros(UInt8, 64K)
    # use default funcs until after reset
    machine.read_mem = read_mem
    machine.write_mem = write_mem
    machine.step = step
    machine.labels = Dict{Symbol,Addr}()
    machine.addrs = Dict{Addr,Symbol}()
    machine.fake_routines = Dict{Addr,Function}()
    machine.properties = Dict{Any, Any}()
    machine.verbose = false
    machine.ctx = @ccall $fake6502_init(c_read_mem::Ptr{Cvoid}, c_write_mem::Ptr{Cvoid}, machine::Ref{Machine})::Ptr{Context}
    println("CTX ", machine.ctx)
    machine.cpu = Accessor(machine.ctx).cpu
    machine.emu = Accessor(machine.ctx).emu
    machine.step = step_func
    machine.read_mem = read_func
    machine.write_mem = write_func
    machine
end

register(func::Function, mach::Machine, sym::Symbol) = register(func, mach, mach.labels[sym])
function register(func::Function, mach::Machine, addr::Addr)
    mach.fake_routines[addr] = func
end

diag(mach::Machine) = diag(unsafe_load(mach.ctx))
diag(ctx::Context) = diag(ctx.cpu, ctx.emu)
diag(cpu::CpuState, emu::EmuState) = @printf "a: %02x x: %02x y: %02x pc: %04x s: %02x ticks: %d\n" cpu.a cpu.x cpu.y cpu.pc cpu.s emu.clockticks

run(mach::Machine, addr::Addr; max_ticks = 100) = run(mach, addr; max_ticks)

function run(mach::Machine, addr::Addr; max_ticks = 100)
    println("RESETTING")
    reset(mach)
    println("FINISHED RESETTING")
    mach.cpu.pc = addr.value - 1
    mach.cpu.s = 0xfe
    @printf "cpu.a: %d cpu.x: %d cpu.y: %d cpu.flags: %d cpu.s: %d cpu.pc: %d\n" offset(mach.cpu, :a) offset(mach.cpu, :x) offset(mach.cpu, :y) offset(mach.cpu, :flags) offset(mach.cpu, :s) offset(mach.cpu, :pc)
    #diag(mach)
    while mach.emu.clockticks < max_ticks && mach.cpu.s != 0
        call_step(mach)
        #diag(mach)
    end
end

function loadprg(filename, mach::Machine; labelfile="")
    mem = mach.mem
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
    if isfile(labelfile)
        open(labelfile, "r") do io
            for line in readlines(io)
                (_, addr, name) = split(line)
                sym = Symbol(name[2:end])
                addr = A(parse(Int, addr; base = 16))
                mach.labels[sym] = addr
                mach.addrs[addr] = sym
            end
        end
    end
    return off, total-1
end

reset(mach::Machine) = @ccall $fake6502_reset(mach.ctx::Ptr{Context})::Cvoid

step(mach::Machine) = @ccall $fake6502_step(mach.ctx::Ptr{Context})::Cvoid

function display_hex(mem::Vector{UInt8})
    area = mem[screen]
    for i in 1:40:SCREEN_LEN
        println(lpad(i:i+39, 10), ": ", join((b-> string(b; base=16, pad=2)).(area[i:i+39]), " "))
    end
end

display_chars(screen_mem::AbstractArray{T, 1}) where {T <: UInt8} = display_chars(identity, screen_mem)

function display_chars(cvt::Function, screen_mem::AbstractVector{T}) where {T <: UInt8}
    println("+", join((x->"-").(1:40), ""), "+")
    for i in 1:40:SCREEN_LEN
        println("|", String(cvt.(screen_mem[i:i+39])), "|")
    end
    println("+", join((x->"-").(1:40), ""), "+")
end

const CONDENSE_START = 2850
