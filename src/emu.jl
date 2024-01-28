#=
/* Fake6502 CPU emulator core v1.3J ******************
 *Original Author:Mike Chambers (miker00lz@gmail.com)*
 *                                                   *
 *New Author:David MHS Webster (github.com/gek169)   *
 *    Leave a star on github to show thanks for this *
 *        FULLY PUBLIC DOMAIN, CC0 CODE              *
 * Which I give to you and the world with absolutely *
 *  no attribution, monetary compensation, or        *
 *  copyleft requirement. Just write code!           *
 *New Author (v1.3J): Bill Burdick (github.com/zot)  *
 *    Leave a star on github to show thanks for this *
 *        FULLY PUBLIC DOMAIN, CC0 CODE              *
 * Which I give to you and the world with absolutely *
 *  no attribution, monetary compensation, or        *
 *  copyleft requirement. Just write code!           *
 *****************************************************
 *       Let all that you do be done with love       *
 *****************************************************
 *This version has been overhauled with major bug    *
 *fixes relating to decimal mode and adc/sbc. I've   *
 *put the emulator through its paces in kernalemu    *
 *as well as run it through an instruction exerciser *
 *to make sure it works properly. I also discovered  *
 *bugs in the instruction exerciser while I was at it*
 *I might contribute some fixes back to them.        *
 *****************************************************
 * v1.3J - port to Julia, encapsulation, hooks       *
 * v1.3 - refactoring and more bug fixes             *
 * v1.2 - Major bug fixes in handling adc and sbc    *
 * v1.1 - Small bugfix in BIT opcode, but it was the *
 *        difference between a few games in my NES   *
 *        emulator working and being broken!         *
 *        I went through the rest carefully again    *
 *        after fixing it just to make sure I didn't *
 *        have any other typos! (Dec. 17, 2011)      *
 *                                                   *
 * v1.0 - First release (Nov. 24, 2011)              *
 *****************************************************
 * LICENSE: This source code is released into the    *
 * public domain, but if you use it please do give   *
 * credit. I put a lot of effort into writing this!  *
 * Note by GEK: this is not a requirement.           *
 *****************************************************
 * Fake6502 is a MOS Technology 6502 CPU emulation   *
 * engine in C. It was written as part of a Nintendo *
 * Entertainment System emulator I've been writing.  *
 *                                                   *
 * A couple important things to know about are two   *
 * defines in the code. One is "UNDOCUMENTED" which, *
 * when defined, allows Fake6502 to compile with     *
 * full support for the more predictable             *
 * undocumented instructions of the 6502. If it is   *
 * undefined, undocumented opcodes just act as NOPs. *
 *                                                   *
 * The other define is "NES_CPU", which causes the   *
 * code to compile without support for binary-coded  *
 * decimal (BCD) support for the ADC and SBC         *
 * opcodes. The Ricoh 2A03 CPU in the NES does not   *
 * support BCD, but is otherwise identical to the    *
 * standard MOS 6502. (Note that this define is      *
 * enabled in this file if you haven't changed it    *
 * yourself. If you're not emulating a NES, you      *
 * should comment it out.)                           *
 *                                                   *
 * If you do discover an error in timing accuracy,   *
 * or operation in general please e-mail me at the   *
 * address above so that I can fix it. Thank you!    *
 *                                                   *
 *****************************************************
 * Usage:                                            *
 *                                                   *
 * Basic functionality works as is.                  *
 * Customize it by defining your own methods:        *
 *   read6502, write6502, step6502                   *
 *                                                   *
 * You can parameterize BasicCpu                     *
 * You can also make your own subtype of Cpu         *
 *****************************************************
 * Useful functions in this emulator:                *
 *                                                   *
 * reset6502(cpu::Cpu)                               *
 *   - Call this once before you begin execution.    *
 *                                                   *
 * exec6502(cpu::Cpu, tickcount::Int64)              *
 *   - Execute 6502 code up to the next specified    *
 *     count of clock ticks.                         *
 *                                                   *
 * step6502(cpu::Cpu)                                *
 *   - Execute a single instrution.                  *
 *                                                   *
 * irq6502(cpu::Cpu)                                 *
 *   - Trigger a hardware IRQ in the 6502 core.      *
 *                                                   *
 * nmi6502(cpu::Cpu)                                 *
 *   - Trigger an NMI in the 6502 core.              *
 *                                                   *
 *****************************************************
 * Useful variables in this emulator:                *
 *                                                   *
 * cpu.clockticks6502::Int64                         *
 *   - A running total of the emulated cycle count   *
 *     during a call to exec6502.                    *
 * cpu.instructions::Int64                           *
 *   - A running total of the total emulated         *
 *     instruction count. This is not related to     *
 *     clock cycle timing.                           *
 *                                                   *
 *****************************************************/

/*
	6510 EMULATION NOTES:
	1) On the 6510 processor, the only difference is that the addresses 0 and 1 are used
	for data direction and data, respectively.

	2) The initial value of address 0 should always be 0.

	3) Read this page
	https://ist.uwaterloo.ca/~schepers/MJK/6510.html
*/
=#

module Fake6502m

using Printf, StaticArrays

#const TEST_COMPAT = true
const TEST_COMPAT = false
#const FAKE_COMPAT = true
const FAKE_COMPAT = false

const DECIMALMODE = true

# STATUS: NV-BDIZC
const FLAG_CARRY = 0x01
const FLAG_ZERO = 0x02
const FLAG_INTERRUPT = 0x04
const FLAG_DECIMAL = 0x08
#/*bits 4 and 5.*/
const FLAG_BREAK = 0x10
const FLAG_CONSTANT = 0x20
const FLAG_OVERFLOW = 0x40
const SIGN = 0x80
const FLAG_SIGN = SIGN

const BASE_STACK = 0x100

const K = 1024

@kwdef mutable struct Cpu{T}
    #pc::UInt16 = 0x0000
    a::UInt8 = 0x00
    x::UInt8 = 0x00
    y::UInt8 = 0x00
    sp::UInt8 = 0x00
    status::UInt8 = 0x00
    instructions::Int64 = 0
    clockticks6502::Int64 = 0
    oldpc::UInt16 = 0x0000
    # addressing
    ea::UInt16 = 0x0000
    reladdr::UInt16 = 0x0000
    # temporary values
    result::UInt8 = 0x00
    opcode::UInt8 = 0x00
    # making these booleans is slower than using UInt8
    penaltyop::UInt8 = 0x00
    penaltyaddr::UInt8 = 0x00
    #memory::MVector{64K, UInt8} = zeros(MVector{64K, UInt8})
    memory::Vector{UInt8} = zeros(UInt8, 64K)
    user_data::T
end

@kwdef struct Temps
    # registers
    pc::UInt16 = 0x0000
    #status::UInt8 = 0x00
    #clockticks6502::Int64 = 0
    # addressing
    #ea::UInt16 = 0x0000
    #memory::Vector{UInt8} = zeros(UInt8, 64K)
    # temps
    #opcode::UInt8 = 0x00
    #result::UInt16 = 0x0000
    #reladdr::UInt16 = 0x0000
    ## temporary values
    #opcode::UInt8 = 0x00
    ## making these booleans is slower than using UInt8
    #penaltyop::UInt8 = 0x00
    #penaltyaddr::UInt8 = 0x00
end

ticks(cpu, ::Temps) = cpu.clockticks6502

function setticks(cpu, temps::Temps, ticks)
    cpu.clockticks6502 = ticks
    temps
end

function addticks(cpu, temps::Temps, ticks)
    cpu.clockticks6502 += ticks
    temps
end

#penaltyddr(temps::Temps) = Temps(temps; penaltyaddr = 0x01)
#penaltyop(temps::Temps) = Temps(temps; penaltyop = 0x01)

#add(t::Temps; clockticks6502 = 0) = Temps(t; clockticks6502 = t.clockticks6502 + clockticks6502)

#Temps(t::Temps; ea = t.ea, result = t.result, clockticks6502 = t.clockticks6502) =
#    Temps(t; ea, result, clockticks6502)
#Temps(t::Temps; ea = t.ea, opcode = t.opcode, penaltyop = t.penaltyop, penaltyaddr = t.penaltyaddr) =
#    Temps(t; ea, opcode, penaltyop, penaltyaddr)
#Temps(t::Temps; ea = t.ea, opcode = t.opcode, memory = t.memory) = Temps(t; ea, opcode, memory)
#Temps(t::Temps; ea = t.ea, opcode = t.opcode) = Temps(t; ea, opcode)
#Temps(t::Temps; ea = t.ea) = Temps(t; ea)
#Temps(t::Temps; ea = nothing) = t
Temps(t::Temps; ea = nothing, pc = t.pc) = Temps(; pc)
#Temps(t::Temps) = t

#Temps(cpu::Cpu) = Temps(; memory = cpu.memory)
Temps(::Cpu) = Temps()

function copy(src::Cpu{T}) where {T}
    new = Cpu{T}(; src.user_data)
    copy(src, new)
    new
end

function copy(src::Cpu, dst::Cpu)
     #dst.pc = src.pc
     dst.a = src.a
     dst.x = src.x
     dst.y = src.y
     dst.sp = src.sp
     dst.status = src.status
     dst.instructions = src.instructions
     #dst.clockticks6502 = src.clockticks6502
     dst.oldpc = src.oldpc
     dst.ea = src.ea
     dst.reladdr = src.reladdr
     #dst.value = src.value
     #dst.result = src.result
     dst.opcode = src.opcode
     #dst.penaltyop = src.penaltyop
     #dst.penaltyaddr = src.penaltyaddr
     dst.memory = src.memory
end

# Basic Cpu does not interfere with computation
read6502(cpu, addr::UInt16) = cpu.memory[addr + 1]
write6502(cpu, addr::UInt16, value::UInt8) = cpu.memory[addr + 1] = value
hookexternal() = nothing

const ticktable = SVector(
#       |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  |
#=0=#      7,    6,    2,    8,    3,    3,    5,    5,    3,    2,    2,    2,    4,    4,    6,    6,  #=0=#
#=1=#      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  #=1=#
#=2=#      6,    6,    2,    8,    3,    3,    5,    5,    4,    2,    2,    2,    4,    4,    6,    6,  #=2=#
#=3=#      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  #=3=#
#=4=#      6,    6,    2,    8,    3,    3,    5,    5,    3,    2,    2,    2,    3,    4,    6,    6,  #=4=#
#=5=#      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  #=5=#
#=6=#      6,    6,    2,    8,    3,    3,    5,    5,    4,    2,    2,    2,    5,    4,    6,    6,  #=6=#
#=7=#      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  #=7=#
#=8=#      2,    6,    2,    6,    3,    3,    3,    3,    2,    2,    2,    2,    4,    4,    4,    4,  #=8=#
#=9=#      2,    6,    2,    6,    4,    4,    4,    4,    2,    5,    2,    5,    5,    5,    5,    5,  #=9=#
#=A=#      2,    6,    2,    6,    3,    3,    3,    3,    2,    2,    2,    2,    4,    4,    4,    4,  #=A=#
#=B=#      2,    5,    2,    5,    4,    4,    4,    4,    2,    4,    2,    4,    4,    4,    4,    4,  #=B=#
#=C=#      2,    6,    2,    8,    3,    3,    5,    5,    2,    2,    2,    2,    4,    4,    6,    6,  #=C=#
#=D=#      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  #=D=#
#=E=#      2,    6,    2,    8,    3,    3,    5,    5,    2,    2,    2,    2,    4,    4,    6,    6,  #=E=#
#=F=#      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7   #=F=#
)

saveaccum(cpu, n) = cpu.a = UInt8(n & 0xFF)

#/*flag modifiers*/
setcarry(cpu) = cpu.status |= FLAG_CARRY
clearcarry(cpu) = cpu.status &= (~FLAG_CARRY)
setzero(cpu) = cpu.status |= FLAG_ZERO
clearzero(cpu) = cpu.status &= (~FLAG_ZERO)
setinterrupt(cpu) = cpu.status |= FLAG_INTERRUPT
clearinterrupt(cpu) = cpu.status &= (~FLAG_INTERRUPT)
setdecimal(cpu) = cpu.status |= FLAG_DECIMAL
cleardecimal(cpu) = cpu.status &= (~FLAG_DECIMAL)
setoverflow(cpu) = cpu.status |= FLAG_OVERFLOW
clearoverflow(cpu) = cpu.status &= (~FLAG_OVERFLOW)
setsign(cpu) = cpu.status |= FLAG_SIGN
clearsign(cpu) = cpu.status &= (~FLAG_SIGN)
isstatusset(cpu, flags) = cpu.status & flags == flags
isstatusclear(cpu, flags) = cpu.status & flags == 0
setstatus(cpu, flags, values) = cpu.status = (cpu.status & ~flags) | values

function setzeroif(cpu, condition::Bool)
    if condition
        setzero(cpu)
    else
        clearzero(cpu)
    end
end

function setsignif(cpu, condition::Bool)
    if condition
        setsign(cpu)
    else
        clearsign(cpu)
    end
end

function setcarryif(cpu, condition::Bool)
    if condition
        setcarry(cpu)
    else
        clearcarry(cpu)
    end
end

function setoverflowif(cpu, condition::Bool)
    if condition
        setoverflow(cpu)
    else
        clearoverflow(cpu)
    end
end

#/*flag calculation */
zerocalc(cpu, n::UInt8) = setzeroif(cpu, n == 0x00)
zerocalc(cpu, n::UInt16) = setzeroif(cpu, (n & 0x00FF) == 0x0000)
signcalc(cpu, n::UInt16) = setsignif(cpu, (n & 0x0080) != 0x0000)
signcalc(cpu, n::UInt8) = setsignif(cpu, (n & 0x80) != 0x00)
carrycalc(cpu, n::UInt16) = setcarryif(cpu, (n & 0xFF00) != 0x0000)
overflowcalc(cpu, result, accumulator, memory) =
    setoverflowif(cpu, ((result ⊻ UInt16(accumulator)) & ((result ⊻ memory) & SIGN)) != 0)

#/*a few general functions used by various other functions*/
function push_6502_16(cpu, pushval::UInt16)
    write6502(cpu, BASE_STACK + cpu.sp, UInt8((pushval >> 8) & 0xFF))
    write6502(cpu, BASE_STACK + UInt16((cpu.sp - 0x1) & 0xFF), UInt8(pushval & 0xFF))
    cpu.sp -= 0x2;
end

function push_6502_8(cpu, pushval::UInt8)
    write6502(cpu, BASE_STACK + UInt16(cpu.sp), pushval)
    cpu.sp -= 0x01
end

function pull_6502_16(cpu)
    temp16 = read6502(cpu, BASE_STACK + ((cpu.sp + 0x01) & 0xFF)) | (UInt16(read6502(cpu, BASE_STACK + ((cpu.sp + 0x02) & 0xFF))) << 8)
    cpu.sp += 0x2;
    return temp16
end

function pull_6502_8(cpu)
    cpu.sp += 0x1
    return read6502(cpu, BASE_STACK + cpu.sp)
end

function mem_6502_read16(cpu, addr::UInt16)
    return UInt16(read6502(cpu, addr)) |
        (UInt16(read6502(cpu, addr + 0x1)) << 8)
end

function reset6502(cpu, temps)
#	/*
#	    pc = (ushort)read6502(0xFFFC) | ((ushort)read6502(0xFFFD) << 8);
#	    a = 0;
#	    x = 0;
#	    y = 0;
#	    sp = 0xFD;
#	    status |= FLAG_CONSTANT;
#    */
    read6502(cpu, 0x00ff)
    read6502(cpu, 0x00ff)
    read6502(cpu, 0x00ff)
    read6502(cpu, 0x0100)
    read6502(cpu, 0x01ff)
    read6502(cpu, 0x01fe)
    cpu.instructions = 0
    #cpu.pc = mem_6502_read16(cpu, 0xfffc)
    cpu.sp = 0xfd
    cpu.status |= FLAG_CONSTANT | FLAG_INTERRUPT
    setticks(cpu, Temps(temps; pc = mem_6502_read16(cpu, 0xfffc)), 0)
end

pc(::Cpu, temps::Temps) = temps.pc
incpc(::Cpu, temps::Temps, delta::UInt8) = Temps(temps; pc = temps.pc + UInt16(delta))
incpc(::Cpu, temps::Temps, delta::UInt16) = Temps(temps; pc = temps.pc + delta)
incpc(::Cpu, temps::Temps, delta) = Temps(temps; pc = Int32(temps.pc + delta) & 0xFFFF)

#/*addressing mode functions, calculates effective addresses*/
function imp(cpu, temps)
    #TEST_COMPAT && read6502(cpu, cpu.pc)
    TEST_COMPAT && read6502(cpu, pc(cpu, temps))
    temps
end

#/*addressing mode functions, calculates effective addresses*/
function acc(cpu, temps)
    #TEST_COMPAT && read6502(cpu, cpu.pc)
    TEST_COMPAT && read6502(cpu, pc(cpu, temps))
    return temps
end

#/*addressing mode functions, calculates effective addresses*/
function imm(cpu, temps)
    #local ea = cpu.pc
    #cpu.ea = cpu.pc
    cpu.ea = pc(cpu, temps)
    #cpu.pc += 0x1
    return incpc(cpu, temps, 0x01)
    #Temps(temps; ea)
    #temps
end

function zp(cpu, temps) # /*zero-page*/
    #local ea = UInt16(read6502(cpu, cpu.pc))
    #cpu.ea = UInt16(read6502(cpu, cpu.pc))
    cpu.ea = UInt16(read6502(cpu, pc(cpu, temps)))
    #cpu.pc += 0x1
    return incpc(cpu, temps, 0x01)
    #Temps(temps; ea)
    #temps
end

function zpx(cpu, temps) # /*zero-page,X*/
    #local zp = UInt16(read6502(cpu, cpu.pc))
    local zp = UInt16(read6502(cpu, pc(cpu, temps)))
    TEST_COMPAT && cpu.x != 0 && read6502(cpu, zp)
    #local ea = (zp + UInt16(cpu.x)) & 0xFF #/*zero-page wraparound*/
    cpu.ea = (zp + UInt16(cpu.x)) & 0xFF #/*zero-page wraparound*/
    #cpu.pc += 0x1
    return incpc(cpu, temps, 0x01)
    #Temps(temps; ea)
    #temps
end

function zpy(cpu, temps) # /*zero-page,Y*/
    #local ea = (UInt16(read6502(cpu, cpu.pc)) + UInt16(cpu.y)) & 0xFF # /*zero-page wraparound*/
    #cpu.ea = (UInt16(read6502(cpu, cpu.pc)) + UInt16(cpu.y)) & 0xFF # /*zero-page wraparound*/
    cpu.ea = (UInt16(read6502(cpu, pc(cpu, temps))) + UInt16(cpu.y)) & 0xFF # /*zero-page wraparound*/
    #cpu.pc += 0x1
    return incpc(cpu, temps, 0x01)
    #Temps(temps; ea)
    #temps
end

function rel(cpu, temps) #/*relative for branch ops (8-bit immediate value, sign-extended)*/
    #cpu.reladdr = UInt16(read6502(cpu, cpu.pc))
    cpu.reladdr = UInt16(read6502(cpu, pc(cpu, temps)))
    #cpu.pc += 0x1
    if (cpu.reladdr & SIGN) != 0x0000
        cpu.reladdr |= 0xFF00
    end
    return incpc(cpu, temps, 0x01)
    #temps
end

function abso(cpu, temps) #/*absolute*/
    #local ea  =UInt16(read6502(cpu, cpu.pc)) | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8)
    #cpu.ea = UInt16(read6502(cpu, cpu.pc)) | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8)
    cpu.ea = UInt16(read6502(cpu, pc(cpu, temps))) | (UInt16(read6502(cpu, UInt16(pc(cpu, temps)+0x1))) << 8)
    #cpu.pc += 0x2
    return incpc(cpu, temps, 0x02)
    #Temps(temps; ea)
    #temps
end


function absx(cpu, temps) #/*absolute,X*/
    local startpage
    #local addr = UInt16(read6502(cpu, cpu.pc))
    local addr = UInt16(read6502(cpu, pc(cpu, temps)))
    #local ea = addr | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8)
    local ea = addr | (UInt16(read6502(cpu, UInt16(pc(cpu, temps)+0x1))) << 8)
    startpage = ea & 0xFF00
    ea += UInt16(cpu.x)
    if (startpage != (ea & 0xFF00)) #/*one cycle penlty for page-crossing on some opcodes*/
        TEST_COMPAT && read6502(cpu, startpage | (ea & 0xFF))
        cpu.penaltyaddr = 0x1
        #temps = penaltyaddr(temps)
    end
    #cpu.pc += 0x2
    cpu.ea = ea
    return incpc(cpu, temps, 0x02)
    #Temps(temps; ea)
end

function absy(cpu, temps) # /*absolute,Y*/
    local startpage
    #local addr = UInt16(read6502(cpu, cpu.pc))
    local addr = UInt16(read6502(cpu, pc(cpu, temps)))
    #local ea = addr | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8)
    local ea = addr | (UInt16(read6502(cpu, UInt16(pc(cpu, temps)+0x1))) << 8)
    startpage = ea & 0xFF00;
    ea += UInt16(cpu.y)
    if (startpage != (ea & 0xFF00)) # /*one cycle penlty for page-crossing on some opcodes*/
        TEST_COMPAT && read6502(cpu, startpage | (ea & 0xFF))
        cpu.penaltyaddr = 0x1
        #temps = penaltyaddr(temps)
    end
    #cpu.pc += 0x2;
    cpu.ea = ea
    return incpc(cpu, temps, 0x02)
    #Temps(temps; ea)
    #temps
end

function ind(cpu, temps) # /*indirect*/
    local eahelp, eahelp2
    #eahelp = UInt16(read6502(cpu, cpu.pc)) | UInt16(UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8);
    eahelp = UInt16(read6502(cpu, pc(cpu, temps))) | UInt16(UInt16(read6502(cpu, UInt16(pc(cpu, temps)+0x1))) << 8);
    eahelp2 = (eahelp & 0xFF00) | ((eahelp + 0x0001) & 0x00FF) # /*replicate 6502 page-boundary wraparound bug*/
    #local ea = UInt16(read6502(cpu, eahelp)) | (UInt16(read6502(cpu, eahelp2)) << 8)
    cpu.ea = UInt16(read6502(cpu, eahelp)) | (UInt16(read6502(cpu, eahelp2)) << 8)
    #cpu.pc += 0x2;
    return incpc(cpu, temps, 0x02)
    #Temps(temps; ea)
    #temps
end

function indx(cpu, temps) # /* (indirect,X)*/
    #local zp = UInt16(read6502(cpu, cpu.pc))
    local zp = UInt16(read6502(cpu, pc(cpu, temps)))
    TEST_COMPAT && cpu.x != 0 && read6502(cpu, zp)
    local eahelp = UInt16((zp + UInt16(cpu.x)) & 0xFF) # /*zero-page wraparound for table pointer*/
    #cpu.pc += 0x1
    #local ea = UInt16(read6502(cpu, eahelp & 0x00FF)) | (UInt16(read6502(cpu, UInt16((eahelp+0x1) & 0x00FF))) << 8)
    cpu.ea = UInt16(read6502(cpu, eahelp & 0x00FF)) | (UInt16(read6502(cpu, UInt16((eahelp+0x1) & 0x00FF))) << 8)
    return incpc(cpu, temps, 0x01)
    #Temps(temps; ea)
    #temps
end

function indy(cpu, temps) # /* (indirect),Y*/
    #local eahelp = UInt16(read6502(cpu, cpu.pc));
    local eahelp = UInt16(read6502(cpu, pc(cpu, temps)));
    #cpu.pc += 0x1
    local eahelp2 = (eahelp & 0xFF00) | ((eahelp + 0x0001) & 0x00FF) # /*zero-page wraparound*/
    local ea = UInt16(read6502(cpu, eahelp)) | (UInt16(read6502(cpu, eahelp2)) << 8);
    local startpage = ea & 0xFF00;
    ea += UInt16(cpu.y)
    if startpage != (ea & 0xFF00) # /*one cycle penlty for page-crossing on some opcodes*/
        cpu.penaltyaddr = 0x1
        #temps = penaltyaddr(temps)
        TEST_COMPAT && read6502(cpu, startpage | (ea & 0xFF))
    end
    cpu.ea = ea
    return incpc(cpu, temps, 0x01)
    #Temps(temps; ea)
    #temps
end

function getvalue(cpu, temps)
    is_acc(cpu.opcode) && return UInt16(cpu.a)
    #is_acc(temps.opcode) && return UInt16(cpu.a)
    return UInt16(read6502(cpu, cpu.ea));
    #return UInt16(read6502(cpu, temps.ea));
end

function getvalue16(cpu, temps)
    return UInt16(read6502(cpu, cpu.ea)) | (UInt16(read6502(cpu, UInt16(cpu.ea+0x1))) << 8);
    #return UInt16(read6502(cpu, temps.ea)) | (UInt16(read6502(cpu, UInt16(temps.ea+0x1))) << 8);
end

function putvalue(cpu, temps, saveval::UInt16)
    if is_acc(cpu.opcode)
    #if is_acc(temps.opcode)
        # addr mode is acc
        cpu.a = UInt8(saveval & 0x00FF);
    else
        write6502(cpu, cpu.ea, UInt8(saveval & 0x00FF))
        #write6502(cpu, temps.ea, UInt8(saveval & 0x00FF))
    end
end

#/*instruction handler functions*/

signed(i::UInt8) = Int16(reinterpret(Int8, i))
signed(i::UInt16) = reinterpret(Int16, i)

# Thanks to Bruce Clark: http://www.6502.org/tutorials/decimal_mode.html
function adc_non_nes(cpu, temps, value)
    (cpu.status & FLAG_DECIMAL) == 0 &&
        return adc_nes(cpu, temps, value)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local A::UInt8 = cpu.a
    local B::UInt8 = value
    local C::UInt8 = cpu.status & 0x0001
    local AL::UInt8 = (A & 0x0F) + (B & 0x0F) + C
    if AL >= 0x0A
        AL = ((AL + 0x06) & 0x0F) + 0x10
    end
    local result::UInt16 = UInt16(A & 0xF0) + UInt16(B & 0xF0) + AL
    # result can be over 0xFF at this point
    if result >= 0xA0
        result += 0x60
    end
    cpu.a = UInt8(result & 0xFF)
    setcarryif(cpu, result & 0xFF00 != 0)
    setzeroif(cpu, (A + B + C) & 0xFF == 0)
    local AH::Int16 = signed(A & 0xF0) + signed(B & 0xF0) + signed(AL)
    setsignif(cpu, AH & SIGN != 0)
    setoverflowif(cpu, AH < -128 || AH > 127)
    temps
end

function adc_nes(cpu, temps, value)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local result = UInt16(cpu.a) + value + UInt16(cpu.status & FLAG_CARRY)
    carrycalc(cpu, result)
    zerocalc(cpu, result)
    overflowcalc(cpu, result, cpu.a, value)
    signcalc(cpu, result)
    saveaccum(cpu, result)
    temps
end

# to emulate NES, make a custom adc method call adc_nes instead of adc_non_nes
adc(cpu, temps, value = getvalue(cpu, temps)) = adc_non_nes(cpu, temps, value)

# AND oper + set C as ASL, immediate only
function anc(cpu, temps)
    local A = cpu.a & getvalue(cpu, temps)
    zerocalc(cpu, A)
    signcalc(cpu, A)
    setstatus(cpu, FLAG_CARRY, A >> 7)
    saveaccum(cpu, A)
    temps
end

function and(cpu, temps)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    value = getvalue(cpu, temps)
    local result = UInt16(cpu.a) & value
    zerocalc(cpu, result)
    signcalc(cpu, result)
    saveaccum(cpu, result)
    temps
end

function asl(cpu, temps)
    local value = getvalue(cpu, temps)
    TEST_COMPAT && addrsyms[cpu.opcode + 1] != :acc && putvalue(cpu, temps, value)
    #TEST_COMPAT && addrsyms[temps.opcode + 1] != :acc && putvalue(cpu, temps, value)
    local result = value << 1
    carrycalc(cpu, result)
    zerocalc(cpu, result)
    signcalc(cpu, result)
    putvalue(cpu, temps, result)
    temps
end

function check_cross_page_boundary(cpu, temps)
    TEST_COMPAT &&
        read6502(cpu, cpu.oldpc)
    #if cpu.oldpc & 0xFF00 != cpu.pc & 0xFF00
    if cpu.oldpc & 0xFF00 != pc(cpu, temps) & 0xFF00
        # /*check if jump crossed a page boundary*/
        if TEST_COMPAT
            #read6502(cpu, (cpu.oldpc & 0xFF00) | (cpu.pc & 0x00FF))
            read6502(cpu, (cpu.oldpc & 0xFF00) | (pc(cpu, temps) & 0x00FF))
        end
        addticks(cpu, temps, 2)
    else
        addticks(cpu, temps, 1)
    end
end

function bcc(cpu, temps)
    if cpu.status & FLAG_CARRY == 0x0
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end

function bcs(cpu, temps)
    if cpu.status & FLAG_CARRY == FLAG_CARRY
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end

function beq(cpu, temps)
    if cpu.status & FLAG_ZERO == FLAG_ZERO
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end


function bit(cpu, temps)
    local value = getvalue(cpu, temps);
    local result = UInt16(cpu.a) & value
    zerocalc(cpu, result)
    cpu.status = (cpu.status & 0x3F) | (UInt8(value) & 0xC0);
    temps
end

function bmi(cpu, temps)
    if isstatusset(cpu, FLAG_SIGN)
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end

function bne(cpu, temps)
    if  isstatusclear(cpu, FLAG_ZERO)
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end

function bpl(cpu, temps)
    if isstatusclear(cpu, FLAG_SIGN)
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end

function brk_6502(cpu, temps)
    #TEST_COMPAT && read6502(cpu, cpu.pc)
    TEST_COMPAT && read6502(cpu, pc(cpu, temps))
    #cpu.pc += 0x1
    temps = incpc(cpu, temps, 0x01)
    #push_6502_16(cpu, cpu.pc)
    push_6502_16(cpu, pc(cpu, temps))
    push_6502_8(cpu, cpu.status | FLAG_BREAK)
    setinterrupt(cpu)
    #cpu.pc = UInt16(read6502(cpu, 0xFFFE)) | (UInt16(read6502(cpu, 0xFFFF)) << 8)
    return Temps(temps; pc = UInt16(read6502(cpu, 0xFFFE)) | (UInt16(read6502(cpu, 0xFFFF)) << 8))
    #temps
end

function bvc(cpu, temps)
    if cpu.status & FLAG_OVERFLOW == 0x0
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end

function bvs(cpu, temps)
    if cpu.status & FLAG_OVERFLOW == FLAG_OVERFLOW
        #cpu.oldpc = cpu.pc
        cpu.oldpc = pc(cpu, temps)
        #cpu.pc += cpu.reladdr
        temps = incpc(cpu, temps, cpu.reladdr)
        return check_cross_page_boundary(cpu, temps)
    else
        temps
    end
end


function clc(cpu, temps)
    clearcarry(cpu)
    temps
end

function cld(cpu, temps)
    cleardecimal(cpu)
    temps
end

function cli(cpu, temps)
    clearinterrupt(cpu)
    temps
end

function clv(cpu, temps)
    clearoverflow(cpu)
    temps
end

function cmp(cpu, temps)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local value = getvalue(cpu, temps)
    local result = UInt16(cpu.a) - value
    setcarryif(cpu, cpu.a >= UInt8(value & 0x00FF))
    setzeroif(cpu, cpu.a == UInt8(value & 0x00FF))
    signcalc(cpu, result)
    temps
end


function cpx(cpu, temps)
    local value = getvalue(cpu, temps)
    local result = UInt16(cpu.x) - value
    setcarryif(cpu, cpu.x >= UInt8(value & 0x00FF))
    setzeroif(cpu, cpu.x == UInt8(value & 0x00FF))
    signcalc(cpu, result)
    temps
end

function cpy(cpu, temps)
    local value = getvalue(cpu, temps)
    local result = UInt16(cpu.y) - value
    setcarryif(cpu, cpu.y >= UInt8(value & 0x00FF))
    setzeroif(cpu, cpu.y == UInt8(value & 0x00FF))
    signcalc(cpu, result)
    temps
end

function dec(cpu, temps)
    local value = getvalue(cpu, temps)
    local result = value - 0x1
    zerocalc(cpu, result)
    signcalc(cpu, result)
    putvalue(cpu, temps, result)
    temps
end


function dex(cpu, temps)
    cpu.x -= 0x1
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
    temps
end

function dey(cpu, temps)
    cpu.y -= 0x1
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
    temps
end

function eor(cpu, temps)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local value = getvalue(cpu, temps);
    local result = UInt16(cpu.a) ⊻ value;
    zerocalc(cpu, result);
    signcalc(cpu, result);
    saveaccum(cpu, result);
    temps
end

function inc(cpu, temps)
    local value = getvalue(cpu, temps)
    local result = value + 0x1
    zerocalc(cpu, result)
    signcalc(cpu, result)
    putvalue(cpu, temps, result)
    cpu.result = UInt8(result & 0xFF)
    temps
end

function inx(cpu, temps)
    cpu.x += 0x1
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
    temps
end

function iny(cpu, temps)
    cpu.y += 0x1
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
    temps
end

function jam(cpu, temps)
    #cpu.pc -= 0x1
    temps = incpc(cpu, temps, -1)
    return addticks(cpu, temps, 1)
end

function jmp(cpu, temps)
    #cpu.pc = cpu.ea
    #cpu.pc = temps.ea
    return Temps(temps; pc = cpu.ea)
    #temps
end

function jsr(cpu, temps)
    local oldsp = cpu.sp
    #TEST_COMPAT && (cpu.pc - 0x0001) != 0x0100 | oldsp && # not in page zero
    TEST_COMPAT && (pc(cpu, temps) - 0x0001) != 0x0100 | oldsp && # not in page zero
        read6502(cpu, 0x0100 | oldsp)
    #push_6502_16(cpu, cpu.pc - 0x1)
    push_6502_16(cpu, pc(cpu, temps) - 0x1)
    local ea = cpu.ea
    #local ea = temps.ea
    #if (cpu.pc - 0x0001) == 0x0100 | oldsp # in page zero
    if (pc(cpu, temps) - 0x0001) == 0x0100 | oldsp # in page zero
        ea = (ea & 0xFF) | (UInt16(read6502(cpu, 0x0100 | oldsp)) << 8)
        cpu.ea = ea
    end
    #cpu.pc = ea
    return Temps(temps, pc = ea)
    #Temps(temps; ea)
    #temps
end

function lda(cpu, temps)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local value = getvalue(cpu, temps)
    cpu.a = UInt8(value & 0x00FF)
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
    temps
end

function ldx(cpu, temps)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local value = getvalue(cpu, temps)
    cpu.x = UInt8(value & 0x00FF)
   
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
    temps
end

function ldy(cpu, temps)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local value = getvalue(cpu, temps)
    cpu.y = UInt8(value & 0x00FF)
   
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
    temps
end

function lsr(cpu, temps)
    local value = getvalue(cpu, temps)
    local result = value >> 1
    setcarryif(cpu, (value & 0x1) != 0)
    zerocalc(cpu, result)
    signcalc(cpu, result)
    putvalue(cpu, temps, result)
    temps
end

function nop(cpu, temps)
    local op = cpu.opcode
    #local op = temps.opcode
    if op == 0x1C || op == 0x3C || op == 0x5C || op == 0x7C || op == 0xDC || op == 0xFC
        cpu.penaltyop = 0x1
        #temps = penaltyop(temps)
    end
    if TEST_COMPAT
        addr = addrsyms[op + 1]
        if addr == :absx || addr == :absy
            cpu.penaltyaddr == 0x0 &&
            #temps.penaltyaddr == 0x0 &&
                getvalue(cpu, temps)
        elseif addr != :imm && addr != :abso && addr != :imp
            getvalue(cpu, temps)
        end
    end
    temps
end

function ora(cpu, temps)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local value = getvalue(cpu, temps)
    local result = UInt16(cpu.a) | value
    zerocalc(cpu, result)
    signcalc(cpu, result)
    saveaccum(cpu, result)
    temps
end

function pha(cpu, temps)
    push_6502_8(cpu, cpu.a)
    temps
end

function php(cpu, temps)
    #TEST_COMPAT && read6502(cpu, cpu.pc)
    TEST_COMPAT && read6502(cpu, pc(cpu, temps))
    push_6502_8(cpu, cpu.status | FLAG_BREAK)
    temps
end

function pla(cpu, temps)
    cpu.a = pull_6502_8(cpu)
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
    temps
end

function plp(cpu, temps)
    cpu.status = (pull_6502_8(cpu) | FLAG_CONSTANT) & 0xEF
    temps
end

function rol(cpu, temps)
    local value = getvalue(cpu, temps)
    FAKE_COMPAT && putvalue(cpu, temps, value)
    local result = (value << 1) | (cpu.status & FLAG_CARRY)
    carrycalc(cpu, result)
    zerocalc(cpu, result)
    signcalc(cpu, result)
    putvalue(cpu, temps, result)
    temps
end

function ror(cpu, temps)
    local value = getvalue(cpu, temps)
    FAKE_COMPAT && putvalue(cpu, temps, value)
    local result = (value >> 1) | ((cpu.status & FLAG_CARRY) << 7)
    setcarryif(cpu, (value & 1) != 0)
    zerocalc(cpu, result)
    signcalc(cpu, result)
    putvalue(cpu, temps, result)
    cpu.result = result
    temps
end

function rti(cpu, temps)
    cpu.status = (pull_6502_8(cpu) | FLAG_CONSTANT) & 0xEF
    local value = pull_6502_16(cpu)
    #cpu.pc = value
    return Temps(temps; pc = value)
    #temps
end

function rts(cpu, temps)
    local value = pull_6502_16(cpu)
    #cpu.pc = value + 0x0001
    return Temps(temps; pc = value + 0x0001)
    #temps
end

# Thanks to Bruce Clark: http://www.6502.org/tutorials/decimal_mode.html
function sbc_non_nes(cpu, temps, value)
    isstatusclear(cpu, FLAG_DECIMAL) &&
        return sbc_nes(cpu, temps, value)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    local A::Int16 = Int16(cpu.a)
    local B::UInt8 = value & 0xFF
    local C::UInt8 = cpu.status & FLAG_CARRY
    local AL::Int16 = (A & 0x0F) - Int8(B & 0x0F) + Int8(C) - Int16(0x0001)
    if AL < 0
        AL = ((AL - Int16(0x0006)) & 0x0F) - Int16(0x0010)
    end
    A = (A & 0xF0) - (B & 0xF0) + AL
    if A < 0
        A -= 0x60
    end
    local fb = B ⊻ 0x00FF
    local fresult = UInt16(cpu.a) + fb + UInt16(cpu.status & FLAG_CARRY)
    zerocalc(cpu, fresult)
    signcalc(cpu, fresult)
    overflowcalc(cpu, fresult, cpu.a, fb)
    carrycalc(cpu, fresult)
    saveaccum(cpu, reinterpret(UInt16, A))
    temps
end

function sbc_nes(cpu, temps, value)
    cpu.penaltyop = 0x1
    #temps = penaltyop(temps)
    value ⊻= 0x00FF
    local result = UInt16(cpu.a) + value + UInt16(cpu.status & FLAG_CARRY)
    zerocalc(cpu, result)
    overflowcalc(cpu, result, cpu.a, value)
    signcalc(cpu, result)
    carrycalc(cpu, result)
    saveaccum(cpu, result)
    temps
end

# to emulate NES, make a custom adc method call sbc_nes instead of sbc_non_nes
sbc(cpu, temps, value = getvalue(cpu, temps)) = sbc_non_nes(cpu, temps, value)

function sec(cpu, temps)
    setcarry(cpu)
    temps
end

function sed(cpu, temps)
    setdecimal(cpu)
    temps
end

function sei(cpu, temps)
    setinterrupt(cpu)
    temps
end

function sta(cpu, temps)
    putvalue(cpu, temps, UInt16(cpu.a))
    temps
end

function stx(cpu, temps)
    putvalue(cpu, temps, UInt16(cpu.x))
    temps
end

function sty(cpu, temps)
    putvalue(cpu, temps, UInt16(cpu.y))
    temps
end

function tax(cpu, temps)
    cpu.x = cpu.a
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
    temps
end

function tay(cpu, temps)
    cpu.y = cpu.a
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
    temps
end

function tsx(cpu, temps)
    cpu.x = cpu.sp
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
    temps
end

function txa(cpu, temps)
    cpu.a = cpu.x
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
    temps
end

function txs(cpu, temps)
    cpu.sp = cpu.x
    temps
end

function tya(cpu, temps)
    cpu.a = cpu.y
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
    temps
end

#/*undocumented instructions~~~~~~~~~~~~~~~~~~~~~~~~~*/
# to disable them, override them to call nop()

# AND oper + LSR
function alr(cpu, temps)
    local M = getvalue(cpu, temps)
    local anded = cpu.a & M
    local A = anded >> 1
    local N = A & SIGN
    local Z = A == 0x00 ? FLAG_ZERO : 0
    local C = anded & FLAG_CARRY
    cpu.a = A
    setstatus(cpu, FLAG_SIGN | FLAG_ZERO | FLAG_CARRY, N | Z | C)
    temps
end

# AND X + AND oper
# A' = (A \/ EE) /\ X /\ M
function ane(cpu, temps)
    cpu.a = (cpu.a | 0xEE) & cpu.x & getvalue(cpu, temps)
    cpu.status = (cpu.a & SIGN) | (cpu.status & 0x7D) | (cpu.a == 0 ? FLAG_ZERO : 0x0)
    temps
end

# A' = (C << 7) | ((A & M) >> 1)
# status: N, Z, C = A'6, V = (A6 & M6) ⊻ (A7 & M7)
# decimal mode differs
function arr(cpu, temps)
    local V, C
    local A = cpu.a
    local anded = A & getvalue(cpu, temps)
    local N = (cpu.status & FLAG_CARRY) << 7
    A = N | (anded >> 1)
    local Z = A == 0 ? 0x02 : 0x00
    if cpu.status & FLAG_DECIMAL == 0
        # binary mode
        if (anded & 0x0F) > 5
            local low = (A & 0x0F) + 0x06
        end
        V = (anded ⊻ (anded >> 1)) & FLAG_OVERFLOW
        C = (A & 0x40) >> 6
    else
        # decimal mode
        local AH = anded >> 4
        local AL = anded & 0x0F
        V = (anded ⊻ A) & FLAG_OVERFLOW
        if AL + (AL & 1) > 0x05
            A = (A & 0xF0) | ((A + 0x06) & 0x0F)
        end
        C = AH + (AH & 1) > 5 != 0 ? FLAG_CARRY : 0x00
        if C != 0
            A = (A + 0x60) & 0xFF
        end
    end
    cpu.a = A
    setstatus(cpu, FLAG_SIGN | FLAG_OVERFLOW | FLAG_ZERO | FLAG_CARRY, N | V | Z | C)
    temps
end

function las(cpu, temps)
    local value = cpu.sp & getvalue(cpu, temps)
    cpu.a = value
    cpu.x = value
    cpu.sp = value
    cpu.status = (value & SIGN) | (cpu.status & 0x7D) | (value == 0 ? FLAG_ZERO : 0x0)
    temps
end

function lax(cpu, temps)
    temps = lda(cpu, temps)
    ldx(cpu, temps)
end

function lxa(cpu, temps)
    local value = getvalue(cpu, temps) & (cpu.a | 0xEE)
    cpu.a = value
    cpu.x = value
    cpu.status = (value & SIGN) | (cpu.status & 0x7D) | (value == 0 ? FLAG_ZERO : 0x0)
    temps
end

function sax(cpu, temps)
    temps = sta(cpu, temps)
    temps = stx(cpu, temps)
    putvalue(cpu, temps, UInt16(cpu.a & cpu.x))
    checkpenalty(cpu, temps)
end

function sbx(cpu, temps)
    local first = cpu.a & cpu.x
    local second = getvalue(cpu, temps)
    local value = UInt16(first) - second
    cpu.x = UInt8(value & 0xFF)
    setcarryif(cpu, first >= UInt8(second & 0x00FF))
    setzeroif(cpu, first == UInt8(second & 0x00FF))
    signcalc(cpu, value)
    temps
end

function dcp(cpu, temps)
    temps = dec(cpu, temps)
    temps = cmp(cpu, temps)
    checkpenalty(cpu, temps)
end

function isc(cpu, temps)
    temps = inc(cpu, temps)
    sbc(cpu, temps, cpu.result)
end

function slo(cpu, temps)
    local M = getvalue(cpu, temps) << 1
    putvalue(cpu, temps, M)
    local A = cpu.a | UInt8(M & 0xFF)
    local N = A & SIGN
    local Z = A == 0x00 ? FLAG_ZERO : 0
    local C = UInt8(M >> 8)
    cpu.a = A
    setstatus(cpu, FLAG_SIGN | FLAG_ZERO | FLAG_CARRY, N | Z | C)
    checkpenalty(cpu, temps)
end

function rla(cpu, temps)
    local M = (getvalue(cpu, temps) << 1) | (cpu.status & FLAG_CARRY)
    putvalue(cpu, temps, M)
    local A = cpu.a & M
    local N = A & SIGN
    local Z = A == 0x00 ? FLAG_ZERO : 0
    local C = UInt8(M >> 8)
    cpu.a = A
    setstatus(cpu, FLAG_SIGN | FLAG_ZERO | FLAG_CARRY, N | Z | C)
    checkpenalty(cpu, temps)
end

function rra(cpu, temps)
    temps = ror(cpu, temps)
    temps = adc(cpu, temps, cpu.result)
    checkpenalty(cpu, temps)
end

function sha(cpu, temps)
    putvalue(cpu, temps, cpu.a & cpu.x & ((cpu.ea >> 8) + 0x1))
    #putvalue(cpu, temps, cpu.a & cpu.x & ((temps.ea >> 8) + 0x1))
    temps
end

function shx(cpu, temps)
    putvalue(cpu, temps, cpu.x & ((cpu.ea >> 8) + 0x1))
    #putvalue(cpu, temps, cpu.x & ((temps.ea >> 8) + 0x1))
    temps
end

function shy(cpu, temps)
    putvalue(cpu, temps, cpu.y & ((cpu.ea >> 8) + 0x1))
    #putvalue(cpu, temps, cpu.y & ((temps.ea >> 8) + 0x1))
    temps
end

function checkpenalty(cpu, temps)
    if cpu.penaltyop == 0 || cpu.penaltyaddr == 0
    #if temps.penaltyop != 0 && temps.penaltyaddr != 0
        temps
    else
        addticks(cpu, temps, -1)
    end
end

function sre(cpu, temps)
    temps = lsr(cpu, temps)
    temps = eor(cpu, temps)
    checkpenalty(cpu, temps)
end

function tas(cpu, temps)
    cpu.sp = cpu.a & cpu.x
    putvalue(cpu, temps, cpu.a & cpu.x & ((cpu.ea >> 8) + 0x1))
    #putvalue(cpu, temps, cpu.a & cpu.x & ((temps.ea >> 8) + 0x1))
    temps
end

function nmi6502(cpu, temps)
    #push_6502_16(cpu, cpu.pc)
    push_6502_16(cpu, pc(cpu, temps))
    push_6502_8(cpu, cpu.status  & ~FLAG_BREAK)
    cpu.status |= FLAG_INTERRUPT
    #cpu.pc = UInt16(read6502(cpu, 0xFFFA)) | (UInt16(read6502(cpu, 0xFFFB)) << 8)
    return Temps(temps; pc = UInt16(read6502(cpu, 0xFFFA)) | (UInt16(read6502(cpu, 0xFFFB)) << 8))
    #temps
end

function irq6502(cpu, temps::Temps)
	#/*
    #push_6502_16(pc);
    #push_6502_8(status);
    #status |= FLAG_INTERRUPT;
    #pc = (ushort)read6502(0xFFFE) | ((ushort)read6502(0xFFFF) << 8);
    #*/
	if (cpu.status & FLAG_INTERRUPT) == 0x0
		#push_6502_16(cpu, cpu.pc)
		push_6502_16(cpu, pc(cpu, temps))
		push_6502_8(cpu, cpu.status & ~FLAG_BREAK)
		cpu.status |= FLAG_INTERRUPT
		#/*pc = mem_6502_read16(0xfffe);*/
		#cpu.pc = UInt16(read6502(cpu, 0xFFFE)) | (UInt16(read6502(cpu, 0xFFFF)) << 8)
		return Temps(temps, pc = UInt16(read6502(cpu, 0xFFFE)) | (UInt16(read6502(cpu, 0xFFFF)) << 8))
    end
    return temps
end

const addrsyms = SVector(
#    |  0  |  1  |  2 |  3  |  4 |  5 |  6 |  7 |  8 |  9  |  A |  B  |  C  |  D  |  E  |  F  |
#=0=#  :imp,:indx,:imp,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:acc, :imm,:abso,:abso,:abso,:abso,  #0
#=1=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpx,:zpx,:imp,:absy,:imp,:absy,:absx,:absx,:absx,:absx,  #1
#=2=# :abso,:indx,:imp,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:acc, :imm,:abso,:abso,:abso,:abso,  #2
#=3=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpx,:zpx,:imp,:absy,:imp,:absy,:absx,:absx,:absx,:absx,  #3
#=4=#  :imp,:indx,:imp,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:acc, :imm,:abso,:abso,:abso,:abso,  #4
#=5=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpx,:zpx,:imp,:absy,:imp,:absy,:absx,:absx,:absx,:absx,  #5
#=6=#  :imp,:indx,:imp,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:acc, :imm, :ind,:abso,:abso,:abso,  #6
#=7=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpx,:zpx,:imp,:absy,:imp,:absy,:absx,:absx,:absx,:absx,  #7
#=8=#  :imm,:indx,:imm,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:imp, :imm,:abso,:abso,:abso,:abso,  #8
#=9=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpy,:zpy,:imp,:absy,:imp,:absy,:absx,:absx,:absy,:absy,  #9
#=A=#  :imm,:indx,:imm,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:imp, :imm,:abso,:abso,:abso,:abso,  #A
#=B=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpy,:zpy,:imp,:absy,:imp,:absy,:absx,:absx,:absy,:absy,  #B
#=C=#  :imm,:indx,:imm,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:imp, :imm,:abso,:abso,:abso,:abso,  #C
#=D=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpx,:zpx,:imp,:absy,:imp,:absy,:absx,:absx,:absx,:absx,  #D
#=E=#  :imm,:indx,:imm,:indx, :zp, :zp, :zp, :zp,:imp, :imm,:imp, :imm,:abso,:abso,:abso,:abso,  #E
#=F=#  :rel,:indy,:imp,:indy,:zpx,:zpx,:zpx,:zpx,:imp,:absy,:imp,:absy,:absx,:absx,:absx,:absx,  #F
)

is_acc(op::UInt8) = op & 0x90 == 0x00 && op & 0x0F == 0x0A

is_imm(op::UInt8) =
    (op & 0x90 == 0x80 && op & 0x0D == 0x00) ||
    (op & 0x10 == 0x00 && (op & 0x0F == 0x09 || op & 0x0F == 0x0B))

function address(c::Cpu, t::Temps)::Temps
    local o = c.opcode
    #o = t.opcode
    #@printf "ADDR OPCODE: %02x\n" o
    if     o==0x00  imp(c, t) elseif o==0x01  indx(c, t) elseif o==0x02  imp(c, t) elseif o==0x03 indx(c, t)
    elseif o==0x04   zp(c, t) elseif o==0x05    zp(c, t) elseif o==0x06   zp(c, t) elseif o==0x07   zp(c, t)
    elseif o==0x08  imp(c, t) elseif o==0x09   imm(c, t) elseif o==0x0A  acc(c, t) elseif o==0x0B  imm(c, t)
    elseif o==0x0C abso(c, t) elseif o==0x0D  abso(c, t) elseif o==0x0E abso(c, t) elseif o==0x0F abso(c, t)
    elseif o==0x10  rel(c, t) elseif o==0x11  indy(c, t) elseif o==0x12  imp(c, t) elseif o==0x13 indy(c, t)
    elseif o==0x14  zpx(c, t) elseif o==0x15   zpx(c, t) elseif o==0x16  zpx(c, t) elseif o==0x17  zpx(c, t)
    elseif o==0x18  imp(c, t) elseif o==0x19  absy(c, t) elseif o==0x1A  imp(c, t) elseif o==0x1B absy(c, t)
    elseif o==0x1C absx(c, t) elseif o==0x1D  absx(c, t) elseif o==0x1E absx(c, t) elseif o==0x1F absx(c, t)
    elseif o==0x20 abso(c, t) elseif o==0x21  indx(c, t) elseif o==0x22  imp(c, t) elseif o==0x23 indx(c, t)
    elseif o==0x24   zp(c, t) elseif o==0x25    zp(c, t) elseif o==0x26   zp(c, t) elseif o==0x27   zp(c, t)
    elseif o==0x28  imp(c, t) elseif o==0x29   imm(c, t) elseif o==0x2A  acc(c, t) elseif o==0x2B  imm(c, t)
    elseif o==0x2C abso(c, t) elseif o==0x2D  abso(c, t) elseif o==0x2E abso(c, t) elseif o==0x2F abso(c, t)
    elseif o==0x30  rel(c, t) elseif o==0x31  indy(c, t) elseif o==0x32  imp(c, t) elseif o==0x33 indy(c, t)
    elseif o==0x34  zpx(c, t) elseif o==0x35   zpx(c, t) elseif o==0x36  zpx(c, t) elseif o==0x37  zpx(c, t)
    elseif o==0x38  imp(c, t) elseif o==0x39  absy(c, t) elseif o==0x3A  imp(c, t) elseif o==0x3B absy(c, t)
    elseif o==0x3C absx(c, t) elseif o==0x3D  absx(c, t) elseif o==0x3E absx(c, t) elseif o==0x3F absx(c, t)
    elseif o==0x40  imp(c, t) elseif o==0x41  indx(c, t) elseif o==0x42  imp(c, t) elseif o==0x43 indx(c, t)
    elseif o==0x44   zp(c, t) elseif o==0x45    zp(c, t) elseif o==0x46   zp(c, t) elseif o==0x47   zp(c, t)
    elseif o==0x48  imp(c, t) elseif o==0x49   imm(c, t) elseif o==0x4A  acc(c, t) elseif o==0x4B  imm(c, t)
    elseif o==0x4C abso(c, t) elseif o==0x4D  abso(c, t) elseif o==0x4E abso(c, t) elseif o==0x4F abso(c, t)
    elseif o==0x50  rel(c, t) elseif o==0x51  indy(c, t) elseif o==0x52  imp(c, t) elseif o==0x53 indy(c, t)
    elseif o==0x54  zpx(c, t) elseif o==0x55   zpx(c, t) elseif o==0x56  zpx(c, t) elseif o==0x57  zpx(c, t)
    elseif o==0x58  imp(c, t) elseif o==0x59  absy(c, t) elseif o==0x5A  imp(c, t) elseif o==0x5B absy(c, t)
    elseif o==0x5C absx(c, t) elseif o==0x5D  absx(c, t) elseif o==0x5E absx(c, t) elseif o==0x5F absx(c, t)
    elseif o==0x60  imp(c, t) elseif o==0x61  indx(c, t) elseif o==0x62  imp(c, t) elseif o==0x63 indx(c, t)
    elseif o==0x64   zp(c, t) elseif o==0x65    zp(c, t) elseif o==0x66   zp(c, t) elseif o==0x67   zp(c, t)
    elseif o==0x68  imp(c, t) elseif o==0x69   imm(c, t) elseif o==0x6A  acc(c, t) elseif o==0x6B  imm(c, t)
    elseif o==0x6C  ind(c, t) elseif o==0x6D  abso(c, t) elseif o==0x6E abso(c, t) elseif o==0x6F abso(c, t)
    elseif o==0x70  rel(c, t) elseif o==0x71  indy(c, t) elseif o==0x72  imp(c, t) elseif o==0x73 indy(c, t)
    elseif o==0x74  zpx(c, t) elseif o==0x75   zpx(c, t) elseif o==0x76  zpx(c, t) elseif o==0x77  zpx(c, t)
    elseif o==0x78  imp(c, t) elseif o==0x79  absy(c, t) elseif o==0x7A  imp(c, t) elseif o==0x7B absy(c, t)
    elseif o==0x7C absx(c, t) elseif o==0x7D  absx(c, t) elseif o==0x7E absx(c, t) elseif o==0x7F absx(c, t)
    elseif o==0x80  imm(c, t) elseif o==0x81  indx(c, t) elseif o==0x82  imm(c, t) elseif o==0x83 indx(c, t)
    elseif o==0x84   zp(c, t) elseif o==0x85    zp(c, t) elseif o==0x86   zp(c, t) elseif o==0x87   zp(c, t)
    elseif o==0x88  imp(c, t) elseif o==0x89   imm(c, t) elseif o==0x8A  imp(c, t) elseif o==0x8B  imm(c, t)
    elseif o==0x8C abso(c, t) elseif o==0x8D  abso(c, t) elseif o==0x8E abso(c, t) elseif o==0x8F abso(c, t)
    elseif o==0x90  rel(c, t) elseif o==0x91  indy(c, t) elseif o==0x92  imp(c, t) elseif o==0x93 indy(c, t)
    elseif o==0x94  zpx(c, t) elseif o==0x95   zpx(c, t) elseif o==0x96  zpy(c, t) elseif o==0x97  zpy(c, t)
    elseif o==0x98  imp(c, t) elseif o==0x99  absy(c, t) elseif o==0x9A  imp(c, t) elseif o==0x9B absy(c, t)
    elseif o==0x9C absx(c, t) elseif o==0x9D  absx(c, t) elseif o==0x9E absy(c, t) elseif o==0x9F absy(c, t)
    elseif o==0xA0  imm(c, t) elseif o==0xA1  indx(c, t) elseif o==0xA2  imm(c, t) elseif o==0xA3 indx(c, t)
    elseif o==0xA4   zp(c, t) elseif o==0xA5    zp(c, t) elseif o==0xA6   zp(c, t) elseif o==0xA7   zp(c, t)
    elseif o==0xA8  imp(c, t) elseif o==0xA9   imm(c, t) elseif o==0xAA  imp(c, t) elseif o==0xAB  imm(c, t)
    elseif o==0xAC abso(c, t) elseif o==0xAD  abso(c, t) elseif o==0xAE abso(c, t) elseif o==0xAF abso(c, t)
    elseif o==0xB0  rel(c, t) elseif o==0xB1  indy(c, t) elseif o==0xB2  imp(c, t) elseif o==0xB3 indy(c, t)
    elseif o==0xB4  zpx(c, t) elseif o==0xB5   zpx(c, t) elseif o==0xB6  zpy(c, t) elseif o==0xB7  zpy(c, t)
    elseif o==0xB8  imp(c, t) elseif o==0xB9  absy(c, t) elseif o==0xBA  imp(c, t) elseif o==0xBB absy(c, t)
    elseif o==0xBC absx(c, t) elseif o==0xBD  absx(c, t) elseif o==0xBE absy(c, t) elseif o==0xBF absy(c, t)
    elseif o==0xC0  imm(c, t) elseif o==0xC1  indx(c, t) elseif o==0xC2  imm(c, t) elseif o==0xC3 indx(c, t)
    elseif o==0xC4   zp(c, t) elseif o==0xC5    zp(c, t) elseif o==0xC6   zp(c, t) elseif o==0xC7   zp(c, t)
    elseif o==0xC8  imp(c, t) elseif o==0xC9   imm(c, t) elseif o==0xCA  imp(c, t) elseif o==0xCB  imm(c, t)
    elseif o==0xCC abso(c, t) elseif o==0xCD  abso(c, t) elseif o==0xCE abso(c, t) elseif o==0xCF abso(c, t)
    elseif o==0xD0  rel(c, t) elseif o==0xD1  indy(c, t) elseif o==0xD2  imp(c, t) elseif o==0xD3 indy(c, t)
    elseif o==0xD4  zpx(c, t) elseif o==0xD5   zpx(c, t) elseif o==0xD6  zpx(c, t) elseif o==0xD7  zpx(c, t)
    elseif o==0xD8  imp(c, t) elseif o==0xD9  absy(c, t) elseif o==0xDA  imp(c, t) elseif o==0xDB absy(c, t)
    elseif o==0xDC absx(c, t) elseif o==0xDD  absx(c, t) elseif o==0xDE absx(c, t) elseif o==0xDF absx(c, t)
    elseif o==0xE0  imm(c, t) elseif o==0xE1  indx(c, t) elseif o==0xE2  imm(c, t) elseif o==0xE3 indx(c, t)
    elseif o==0xE4   zp(c, t) elseif o==0xE5    zp(c, t) elseif o==0xE6   zp(c, t) elseif o==0xE7   zp(c, t)
    elseif o==0xE8  imp(c, t) elseif o==0xE9   imm(c, t) elseif o==0xEA  imp(c, t) elseif o==0xEB  imm(c, t)
    elseif o==0xEC abso(c, t) elseif o==0xED  abso(c, t) elseif o==0xEE abso(c, t) elseif o==0xEF abso(c, t)
    elseif o==0xF0  rel(c, t) elseif o==0xF1  indy(c, t) elseif o==0xF2  imp(c, t) elseif o==0xF3 indy(c, t)
    elseif o==0xF4  zpx(c, t) elseif o==0xF5   zpx(c, t) elseif o==0xF6  zpx(c, t) elseif o==0xF7  zpx(c, t)
    elseif o==0xF8  imp(c, t) elseif o==0xF9  absy(c, t) elseif o==0xFA  imp(c, t) elseif o==0xFB absy(c, t)
    elseif o==0xFC absx(c, t) elseif o==0xFD  absx(c, t) elseif o==0xFE absx(c, t) elseif o==0xFF absx(c, t)
    end
end

const opsyms = SVector(
#        |  0  |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |  A |  B |  C |  D |  E |  F |
#=0=# :brk_6502,:ora,:jam,:slo,:nop,:ora,:asl,:slo,:php,:ora,:asl,:anc,:nop,:ora,:asl,:slo, # 0
#=1=#      :bpl,:ora,:jam,:slo,:nop,:ora,:asl,:slo,:clc,:ora,:nop,:slo,:nop,:ora,:asl,:slo, # 1
#=2=#      :jsr,:and,:jam,:rla,:bit,:and,:rol,:rla,:plp,:and,:rol,:anc,:bit,:and,:rol,:rla, # 2
#=3=#      :bmi,:and,:jam,:rla,:nop,:and,:rol,:rla,:sec,:and,:nop,:rla,:nop,:and,:rol,:rla, # 3
#=4=#      :rti,:eor,:jam,:sre,:nop,:eor,:lsr,:sre,:pha,:eor,:lsr,:alr,:jmp,:eor,:lsr,:sre, # 4
#=5=#      :bvc,:eor,:jam,:sre,:nop,:eor,:lsr,:sre,:cli,:eor,:nop,:sre,:nop,:eor,:lsr,:sre, # 5
#=6=#      :rts,:adc,:jam,:rra,:nop,:adc,:ror,:rra,:pla,:adc,:ror,:arr,:jmp,:adc,:ror,:rra, # 6
#=7=#      :bvs,:adc,:jam,:rra,:nop,:adc,:ror,:rra,:sei,:adc,:nop,:rra,:nop,:adc,:ror,:rra, # 7
#=8=#      :nop,:sta,:nop,:sax,:sty,:sta,:stx,:sax,:dey,:nop,:txa,:ane,:sty,:sta,:stx,:sax, # 8
#=9=#      :bcc,:sta,:jam,:sha,:sty,:sta,:stx,:sax,:tya,:sta,:txs,:tas,:shy,:sta,:shy,:sha, # 9
#=A=#      :ldy,:lda,:ldx,:lax,:ldy,:lda,:ldx,:lax,:tay,:lda,:tax,:lxa,:ldy,:lda,:ldx,:lax, # A
#=B=#      :bcs,:lda,:jam,:lax,:ldy,:lda,:ldx,:lax,:clv,:lda,:tsx,:las,:ldy,:lda,:ldx,:lax, # B
#=C=#      :cpy,:cmp,:nop,:dcp,:cpy,:cmp,:dec,:dcp,:iny,:cmp,:dex,:sbx,:cpy,:cmp,:dec,:dcp, # C
#=D=#      :bne,:cmp,:jam,:dcp,:nop,:cmp,:dec,:dcp,:cld,:cmp,:nop,:dcp,:nop,:cmp,:dec,:dcp, # D
#=E=#      :cpx,:sbc,:nop,:isc,:cpx,:sbc,:inc,:isc,:inx,:sbc,:nop,:sbc,:cpx,:sbc,:inc,:isc, # E
#=F=#      :beq,:sbc,:jam,:isc,:nop,:sbc,:inc,:isc,:sed,:sbc,:nop,:isc,:nop,:sbc,:inc,:isc  # F
)

#opcode_cmp(i) = :(opcode == $(UInt8(i - 1)))
#
#instruction_for(i) = quote
#    $(addrsyms[i])(cpu)
#    $(opsyms[i])(cpu)
#end
#
#macro gencalls()
#    cases = [i == 256 ? instruction_for(i) : i for i in 2:256]
#    while length(cases) > 1
#        last = pop!(cases)
#        penultimate = pop!(cases)
#        push!(cases, Expr(:elseif, opcode_cmp(penultimate), instruction_for(penultimate), last))
#    end
#    ifs = Expr(:if, opcode_cmp(1), instruction_for(1), cases[1])
#    esc(:(function instruction(cpu, opcode::UInt8)
#              $ifs
#          end))
#end
#
#@gencalls()

function opcode(c::Cpu, t::Temps)::Temps #::Cpu)
    local o = c.opcode
    #o = t.opcode
    if o==0x00 brk_6502(c, t) elseif o==0x01 ora(c, t) elseif o==0x02 jam(c, t) elseif o==0x03 slo(c, t)
    elseif o==0x04  nop(c, t) elseif o==0x05 ora(c, t) elseif o==0x06 asl(c, t) elseif o==0x07 slo(c, t)
    elseif o==0x08  php(c, t) elseif o==0x09 ora(c, t) elseif o==0x0A asl(c, t) elseif o==0x0B anc(c, t)
    elseif o==0x0C  nop(c, t) elseif o==0x0D ora(c, t) elseif o==0x0E asl(c, t) elseif o==0x0F slo(c, t)
    elseif o==0x10  bpl(c, t) elseif o==0x11 ora(c, t) elseif o==0x12 jam(c, t) elseif o==0x13 slo(c, t)
    elseif o==0x14  nop(c, t) elseif o==0x15 ora(c, t) elseif o==0x16 asl(c, t) elseif o==0x17 slo(c, t)
    elseif o==0x18  clc(c, t) elseif o==0x19 ora(c, t) elseif o==0x1A nop(c, t) elseif o==0x1B slo(c, t)
    elseif o==0x1C  nop(c, t) elseif o==0x1D ora(c, t) elseif o==0x1E asl(c, t) elseif o==0x1F slo(c, t)
    elseif o==0x20  jsr(c, t) elseif o==0x21 and(c, t) elseif o==0x22 jam(c, t) elseif o==0x23 rla(c, t)
    elseif o==0x24  bit(c, t) elseif o==0x25 and(c, t) elseif o==0x26 rol(c, t) elseif o==0x27 rla(c, t)
    elseif o==0x28  plp(c, t) elseif o==0x29 and(c, t) elseif o==0x2A rol(c, t) elseif o==0x2B anc(c, t)
    elseif o==0x2C  bit(c, t) elseif o==0x2D and(c, t) elseif o==0x2E rol(c, t) elseif o==0x2F rla(c, t)
    elseif o==0x30  bmi(c, t) elseif o==0x31 and(c, t) elseif o==0x32 jam(c, t) elseif o==0x33 rla(c, t)
    elseif o==0x34  nop(c, t) elseif o==0x35 and(c, t) elseif o==0x36 rol(c, t) elseif o==0x37 rla(c, t)
    elseif o==0x38  sec(c, t) elseif o==0x39 and(c, t) elseif o==0x3A nop(c, t) elseif o==0x3B rla(c, t)
    elseif o==0x3C  nop(c, t) elseif o==0x3D and(c, t) elseif o==0x3E rol(c, t) elseif o==0x3F rla(c, t)
    elseif o==0x40  rti(c, t) elseif o==0x41 eor(c, t) elseif o==0x42 jam(c, t) elseif o==0x43 sre(c, t)
    elseif o==0x44  nop(c, t) elseif o==0x45 eor(c, t) elseif o==0x46 lsr(c, t) elseif o==0x47 sre(c, t)
    elseif o==0x48  pha(c, t) elseif o==0x49 eor(c, t) elseif o==0x4A lsr(c, t) elseif o==0x4B alr(c, t)
    elseif o==0x4C  jmp(c, t) elseif o==0x4D eor(c, t) elseif o==0x4E lsr(c, t) elseif o==0x4F sre(c, t)
    elseif o==0x50  bvc(c, t) elseif o==0x51 eor(c, t) elseif o==0x52 jam(c, t) elseif o==0x53 sre(c, t)
    elseif o==0x54  nop(c, t) elseif o==0x55 eor(c, t) elseif o==0x56 lsr(c, t) elseif o==0x57 sre(c, t)
    elseif o==0x58  cli(c, t) elseif o==0x59 eor(c, t) elseif o==0x5A nop(c, t) elseif o==0x5B sre(c, t)
    elseif o==0x5C  nop(c, t) elseif o==0x5D eor(c, t) elseif o==0x5E lsr(c, t) elseif o==0x5F sre(c, t)
    elseif o==0x60  rts(c, t) elseif o==0x61 adc(c, t) elseif o==0x62 jam(c, t) elseif o==0x63 rra(c, t)
    elseif o==0x64  nop(c, t) elseif o==0x65 adc(c, t) elseif o==0x66 ror(c, t) elseif o==0x67 rra(c, t)
    elseif o==0x68  pla(c, t) elseif o==0x69 adc(c, t) elseif o==0x6A ror(c, t) elseif o==0x6B arr(c, t)
    elseif o==0x6C  jmp(c, t) elseif o==0x6D adc(c, t) elseif o==0x6E ror(c, t) elseif o==0x6F rra(c, t)
    elseif o==0x70  bvs(c, t) elseif o==0x71 adc(c, t) elseif o==0x72 jam(c, t) elseif o==0x73 rra(c, t)
    elseif o==0x74  nop(c, t) elseif o==0x75 adc(c, t) elseif o==0x76 ror(c, t) elseif o==0x77 rra(c, t)
    elseif o==0x78  sei(c, t) elseif o==0x79 adc(c, t) elseif o==0x7A nop(c, t) elseif o==0x7B rra(c, t)
    elseif o==0x7C  nop(c, t) elseif o==0x7D adc(c, t) elseif o==0x7E ror(c, t) elseif o==0x7F rra(c, t)
    elseif o==0x80  nop(c, t) elseif o==0x81 sta(c, t) elseif o==0x82 nop(c, t) elseif o==0x83 sax(c, t)
    elseif o==0x84  sty(c, t) elseif o==0x85 sta(c, t) elseif o==0x86 stx(c, t) elseif o==0x87 sax(c, t)
    elseif o==0x88  dey(c, t) elseif o==0x89 nop(c, t) elseif o==0x8A txa(c, t) elseif o==0x8B ane(c, t)
    elseif o==0x8C  sty(c, t) elseif o==0x8D sta(c, t) elseif o==0x8E stx(c, t) elseif o==0x8F sax(c, t)
    elseif o==0x90  bcc(c, t) elseif o==0x91 sta(c, t) elseif o==0x92 jam(c, t) elseif o==0x93 sha(c, t)
    elseif o==0x94  sty(c, t) elseif o==0x95 sta(c, t) elseif o==0x96 stx(c, t) elseif o==0x97 sax(c, t)
    elseif o==0x98  tya(c, t) elseif o==0x99 sta(c, t) elseif o==0x9A txs(c, t) elseif o==0x9B tas(c, t)
    elseif o==0x9C  shy(c, t) elseif o==0x9D sta(c, t) elseif o==0x9E shx(c, t) elseif o==0x9F sha(c, t)
    elseif o==0xA0  ldy(c, t) elseif o==0xA1 lda(c, t) elseif o==0xA2 ldx(c, t) elseif o==0xA3 lax(c, t)
    elseif o==0xA4  ldy(c, t) elseif o==0xA5 lda(c, t) elseif o==0xA6 ldx(c, t) elseif o==0xA7 lax(c, t)
    elseif o==0xA8  tay(c, t) elseif o==0xA9 lda(c, t) elseif o==0xAA tax(c, t) elseif o==0xAB lxa(c, t)
    elseif o==0xAC  ldy(c, t) elseif o==0xAD lda(c, t) elseif o==0xAE ldx(c, t) elseif o==0xAF lax(c, t)
    elseif o==0xB0  bcs(c, t) elseif o==0xB1 lda(c, t) elseif o==0xB2 jam(c, t) elseif o==0xB3 lax(c, t)
    elseif o==0xB4  ldy(c, t) elseif o==0xB5 lda(c, t) elseif o==0xB6 ldx(c, t) elseif o==0xB7 lax(c, t)
    elseif o==0xB8  clv(c, t) elseif o==0xB9 lda(c, t) elseif o==0xBA tsx(c, t) elseif o==0xBB las(c, t)
    elseif o==0xBC  ldy(c, t) elseif o==0xBD lda(c, t) elseif o==0xBE ldx(c, t) elseif o==0xBF lax(c, t)
    elseif o==0xC0  cpy(c, t) elseif o==0xC1 cmp(c, t) elseif o==0xC2 nop(c, t) elseif o==0xC3 dcp(c, t)
    elseif o==0xC4  cpy(c, t) elseif o==0xC5 cmp(c, t) elseif o==0xC6 dec(c, t) elseif o==0xC7 dcp(c, t)
    elseif o==0xC8  iny(c, t) elseif o==0xC9 cmp(c, t) elseif o==0xCA dex(c, t) elseif o==0xCB sbx(c, t)
    elseif o==0xCC  cpy(c, t) elseif o==0xCD cmp(c, t) elseif o==0xCE dec(c, t) elseif o==0xCF dcp(c, t)
    elseif o==0xD0  bne(c, t) elseif o==0xD1 cmp(c, t) elseif o==0xD2 jam(c, t) elseif o==0xD3 dcp(c, t)
    elseif o==0xD4  nop(c, t) elseif o==0xD5 cmp(c, t) elseif o==0xD6 dec(c, t) elseif o==0xD7 dcp(c, t)
    elseif o==0xD8  cld(c, t) elseif o==0xD9 cmp(c, t) elseif o==0xDA nop(c, t) elseif o==0xDB dcp(c, t)
    elseif o==0xDC  nop(c, t) elseif o==0xDD cmp(c, t) elseif o==0xDE dec(c, t) elseif o==0xDF dcp(c, t)
    elseif o==0xE0  cpx(c, t) elseif o==0xE1 sbc(c, t) elseif o==0xE2 nop(c, t) elseif o==0xE3 isc(c, t)
    elseif o==0xE4  cpx(c, t) elseif o==0xE5 sbc(c, t) elseif o==0xE6 inc(c, t) elseif o==0xE7 isc(c, t)
    elseif o==0xE8  inx(c, t) elseif o==0xE9 sbc(c, t) elseif o==0xEA nop(c, t) elseif o==0xEB sbc(c, t)
    elseif o==0xEC  cpx(c, t) elseif o==0xED sbc(c, t) elseif o==0xEE inc(c, t) elseif o==0xEF isc(c, t)
    elseif o==0xF0  beq(c, t) elseif o==0xF1 sbc(c, t) elseif o==0xF2 jam(c, t) elseif o==0xF3 isc(c, t)
    elseif o==0xF4  nop(c, t) elseif o==0xF5 sbc(c, t) elseif o==0xF6 inc(c, t) elseif o==0xF7 isc(c, t)
    elseif o==0xF8  sed(c, t) elseif o==0xF9 sbc(c, t) elseif o==0xFA nop(c, t) elseif o==0xFB isc(c, t)
    elseif o==0xFC  nop(c, t) elseif o==0xFD sbc(c, t) elseif o==0xFE inc(c, t) elseif o==0xFF isc(c, t)
    end
end

function exec6502(cpu, temps, tickcount::Int64)
	#/*
	#	BUG FIX:
	#	overflow of unsigned 32 bit integer causes emulation to hang.
	#	An instruction might cause the tick count to wrap around into the billions.

	#	The system is changed so that now clockticks 6502 is reset every single time that exec is called.
	#*/
    local instructions = 0
    temps = setticks(cpu, temps, 0)
    while ticks(cpu, temps) < tickcount
        temps = inner_step6502(cpu, temps)
        instructions += 1
    end
    cpu.instructions = instructions
    temps
end

function inner_step6502(cpu, temps::Temps)
    #cpu.opcode = read6502(cpu, cpu.pc)
    cpu.opcode = read6502(cpu, pc(cpu, temps))
    cpu.penaltyop = 0
    cpu.penaltyaddr = 0
    #temps = Temps(temps; opcode = read6502(cpu, cpu.pc), penaltyop = 0, penaltyaddr = 0)
    #temps = Temps(temps; opcode = read6502(cpu, cpu.pc))
    #cpu.pc += 0x1
    temps = incpc(cpu, temps, 0x01)
    cpu.status |= FLAG_CONSTANT
    temps = opcode(cpu, address(cpu, temps))
    local base_ticks::Int64 = ticktable[cpu.opcode + 1]::Int64
    #local base_ticks::Int64 = ticktable[temps.opcode + 1]::Int64
    #/*The is commented out in Mike Chamber's usage of the 6502 emulator for MOARNES*/
    if cpu.penaltyop == 0x00 || cpu.penaltyaddr == 0x00
    #if temps.penaltyop != 0x0 && temps.penaltyaddr != 0x0
        addticks(cpu, temps, base_ticks)
    else
        addticks(cpu, temps, base_ticks + 1)
    end
end

step6502(cpu, temps) = inner_step6502(cpu, setticks(cpu, temps, 0))

end # module Fake6502
