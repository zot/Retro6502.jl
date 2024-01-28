#=
Example test data:

{
  "name": "00 3f f7",
  "initial": {
    "pc": 35714,
    "s": 81,
    "a": 203,
    "x": 117,
    "y": 162,
    "p": 106,
    "ram": [
      [35714, 0],
      [35715, 63],
      [35716, 247],
      [65534, 212],
      [65535, 37],
      [9684, 237]
    ]
  },
  "final": {
    "pc": 9684,
    "s": 78,
    "a": 203,
    "x": 117,
    "y": 162,
    "p": 110,
    "ram": [
      [335, 122],
      [336, 132],
      [337, 139],
      [9684, 237],
      [35714, 0],
      [35715, 63],
      [35716, 247],
      [65534, 212],
      [65535, 37]
    ]
  },
  "cycles": [
    [35714, 0, "read"],
    [35715, 63, "read"],
    [337, 139, "write"],
    [336, 132, "write"],
    [335, 122, "write"],
    [65534, 212, "read"],
    [65535, 37, "read"]
  ]
}

=#

using JSON3
using Test
using Printf
using Fake6502
using Fake6502: rhex, Fake6502m, NewMachine, Machine
import .Fake6502m: Cpu, step6502, read6502, write6502, addrsyms, opsyms, FLAG_DECIMAL, Temps

const REGISTERS = ((:a, :a, :a),(:x, :x, :x), (:y, :y, :y), (:pc, :pc, :pc),(:sp, :s, :s), (:status, :p, :flags))

const CYCLES = []
const FAKE_CYCLES = []
#const CYCLE_ACCURACY = :complete
#const CYCLE_ACCURACY = :counts
const CYCLE_ACCURACY = :none

struct TestCpu end

function read6502(cpu::Cpu{TestCpu}, addr::UInt16)
    push!(CYCLES, [addr, cpu.memory[addr + 1], "read"])
    cpu.memory[addr + 1]
end
function write6502(cpu::Cpu{TestCpu}, addr::UInt16, value::UInt8)
    push!(CYCLES, [addr, value, "write"])
    cpu.memory[addr + 1] = value
end

function fake_read_mem(mach::Machine, addr::UInt16)::UInt8
    #println("READ 0x", string(addr; base=16, pad=4))
    push!(FAKE_CYCLES, [addr, mach[addr], "read"])
    return mach[addr]
end

function fake_write_mem(mach::Machine, addr::UInt16, byte::UInt8)
    #@printf "WRITE %04x = %02x\n" addr byte
    push!(FAKE_CYCLES, [addr, byte, "write"])
    mach[addr] = byte
end

status(s, mask = 0xFF) =
    join([s & (1 << (8 - i)) != 0 ? n : lowercase(n)
          for (i, n) in enumerate("NVXBDIZC") if (1 << (8 - i)) & mask != 0])

function register(c, reg)
    if reg == :p
        "p:$(status(c.p))"
    else
        "$reg:$(rhex(c[reg]))"
    end
end

function testcondstr(buf, cond)
    push!(buf, join([register(cond, p) for p in (:a, :x, :y, :s, :p, :pc)], " "))
    for (addr, val) in sort(cond.ram; by=first)
        note = addr == cond.pc ? "*" :
            addr == 0x0100 | cond.s ? ">" :
            " "
        push!(buf, @sprintf "  %s%04x: %02x" note addr val)
    end
end

function teststr(test)
    buf = []
    testcondstr(buf, test.initial)
    testcondstr(buf, test.final)
    push!(buf, @sprintf "%-15s%s" "EXPECTED" "ACTUAL")
    for i in 1:max(length(test.cycles), length(CYCLES))
        local str1 = if i <= length(test.cycles)
            local (addr, val, type) = test.cycles[i]
            @sprintf "%-5s %04x %02x  " type addr val
        else
            lpad("", 15)
        end
        local str2 = if i <= length(CYCLES)
            local (addr, val, type) = CYCLES[i]
            @sprintf "%-5s %04x %02x  " type addr val
        else
            lpad("", 15)
        end
        push!(buf, str1 * str2)
    end
    join(buf, "\n")
end

function fail(cpu::Cpu, test, file, number, msg, throw = true)
    #@error "$(sprint() do io; JSON3.pretty(io, JSON3.write(test)); end)\n$file:$number:test $(test.name)\n  $msg"
    op = cpu.opcode + 1
    @error "$(throw ? "" : "[IGNORED] ")$file:$number:test $(test.name) ($(opsyms[op]) $(addrsyms[op]))\n$msg\n$(teststr(test))"
    throw && @test "$file:$number" === "FAILED"
end

function run_fake_test(machine::Cpu, fake::Machine, test, file, number)
    for (new, std, fk) in REGISTERS
        val = getfield(machine, new)
        setproperty!(machine, new, convert(typeof(val), test.initial[std]))
        setproperty!(fake.cpu, fk, convert(typeof(val), test.initial[std]))
    end
    for (addr, value) in test.initial.ram
        machine.memory[addr + 1] = value
        fake.mem[addr + 1] = value
    end
    Fake6502.call_step(fake)
    Fake6502m.step6502(machine)
    errs = []
    cycles = [CYCLES...]
    empty!(CYCLES)
    for (new, _, fk) in REGISTERS
        if getproperty(machine, new) != getproperty(fake.cpu, fk)
            push!(errs, "$new != $fk")
        end
    end
    for (addr,) in test.final.ram
        if machine.memory[addr + 1] != fake.mem[addr + 1]
            push!(errs, "[$addr:$(machine.memory[addr + 1])] != $(fake.mem[addr + 1])")
        end
    end
    if CYCLE_ACCURATE
        missing = setdiff(FAKE_CYCLES, cycles)
        extra = setdiff(cycles, FAKE_CYCLES)
        !isempty(extra) && push!(errs, "Extra activity: $(cyclestr(extra))")
        !isempty(missing) && push!(errs, "Missing activity: $(cyclestr(missing))")
    end
    if !isempty(errs)
        fail(machine, test, file, number, join(errs, "\n  "))
    end
    empty!(FAKE_CYCLES)
end

retrievefield(machine::Cpu, temps::Temps, field::Symbol) =
    field == :pc ? temps.pc : getfield(machine, field)

function run_data_test(machine::Cpu, test, file, number)
    local temps = Temps()
    for (new, std) in REGISTERS
        if new == :pc
            temps = Temps(temps; pc = UInt16(test.initial.pc))
        else
            val = getfield(machine, new)
            setfield!(machine, new, convert(typeof(val), test.initial[std]))
        end
    end
    for (addr, value) in test.initial.ram
        machine.memory[addr + 1] = value
    end
    temps = Fake6502m.step6502(machine, temps)
    errs = []
    cycles = [CYCLES...]
    for (new, std) in REGISTERS
        if retrievefield(machine, temps, new) != test.final[std]
            if new == :status
                local diff = machine.status ⊻ UInt8(test.final.p)
                push!(errs, "status ($(status(machine.status, diff))) != p ($(status(test.final.p, diff)))")
            else
                push!(errs, "$new ($(rhex(retrievefield(machine, temps, new)))) != $std ($(rhex(test.final[std])))")
            end
        end
    end
    for (addr, value) in test.final.ram
        if machine.memory[addr + 1] != value
            push!(errs, @sprintf "[%04x %02x] != %02x" addr machine.memory[addr + 1] value)
        end
    end
    if CYCLE_ACCURACY == :complete
        missing = setdiff(test.cycles, cycles)
        extra = setdiff(cycles, test.cycles)
        !isempty(extra) && push!(errs, "Extra activity: $(cyclestr(extra))")
        !isempty(missing) && push!(errs, "Missing activity: $(cyclestr(missing))")
    elseif CYCLE_ACCURACY == :counts && temps.clockticks6502 != length(test.cycles)
        op = machine.opcode + 1
        @error "$file:$number:test $(test.name) ($(opsyms[op]) $(addrsyms[op])) WARNING: clock ticks $(temps.clockticks6502) != $(length(test.cycles))"
    end
    if !isempty(errs)
        local op = first([val for (addr, val) in test.initial.ram if addr == test.initial.pc])
        fail(machine, test, file, number, join(errs, "\n  "))
    end
    empty!(CYCLES)
end

function cyclestr(cyc)
    join([@sprintf("  %-5s %04x %02x", type, addr, val) for (addr, val, type) in cyc], "\n")
end

function runtests(machine::Cpu, dir, inst; mode=:data)
    name = rhex(inst)
    println(name, "...")
    file = joinpath(dir, "$name.json")
    count = 1
    fake = if mode === :fake
        NewMachine(; read_func = fake_read_mem, write_func = fake_write_mem)
    end
    for test in JSON3.read(file)
        mode === :data && run_data_test(machine, test, file, count)
        mode === :fake && run_fake_test(machine, fake, test, file, count)
        count += 1
    end
end

function runtests(dir, inst; mode=:data)
    println("DIR: ", dir)
    runtests(Cpu{TestCpu}(), dir, inst; mode)
end

const ILLEGAL = [
    0x02, 0x03, 0x04, 0x07, 0xFF, 0x0B, 0x0C, 0x0F,
    0x12, 0x13, 0x14, 0x17, 0x1A, 0x1B, 0x1C, 0x1F,
    0x22, 0x23, 0xFF, 0x27, 0xFF, 0x2B, 0xFF, 0x2F,
    0x32, 0x33, 0x34, 0x37, 0x3A, 0x3B, 0x3C, 0x3F,
    0x42, 0x43, 0x44, 0x47, 0xFF, 0x4B, 0xFF, 0x4F,
    0x52, 0x53, 0x54, 0x57, 0x5A, 0x5B, 0x5C, 0x5F,
    0x62, 0x63, 0x64, 0x67, 0xFF, 0x6B, 0xFF, 0x6F,
    0x72, 0x73, 0x74, 0x77, 0x7A, 0x7B, 0x7C, 0x7F,
    0x82, 0x83, 0xFF, 0x87, 0xFF, 0x8B, 0xFF, 0x8F,
    0x92, 0x93, 0xFF, 0x97, 0xFF, 0x9B, 0x9C, 0x9E, 0x9F,
    0xFF, 0xA3, 0xFF, 0xA7, 0xFF, 0xAB, 0xFF, 0xAF,
    0xB2, 0xB3, 0xFF, 0xB7, 0xFF, 0xBB, 0xBC, 0xBF,
    0xC2, 0xC3, 0xFF, 0xC7, 0xFF, 0xCB, 0xCC, 0xCF,
    0xD2, 0xD3, 0xD4, 0xD7, 0xDA, 0xDB, 0xDC, 0xDF,
    0xE2, 0xE3, 0xFF, 0xE7, 0xFF, 0xEB, 0xEC, 0xEF,
    0xF2, 0xF3, 0xF4, 0xF7, 0xFA, 0xFB, 0xFC, 0xFF,
]
const DONE = 0x00:0x6F
const SKIP = [0x10]

function runtests(dir; mode=:data)
    println("DIR: ", dir)
    machine = Cpu(; user_data = TestCpu())
    for inst in 0x00:0xFF
        #inst ∈ ILLEGAL && continue
        #inst ∈ DONE && continue
        #inst ∈ SKIP && continue
        runtests(machine, dir, inst; mode)
    end
end

#runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"); mode=:fake)
runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"))
#runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x10)

#@testset "test instructions" begin
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x00)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x01)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x02)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x03)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x04)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x05)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x06)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x07)
#    runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x08)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x09)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x0A)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x0B)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x0C)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x0D)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x0E)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0x0F)
#    #runtests(joinpath(dirname(realpath(@__FILE__)), "test-files"), 0xA9)
#end
