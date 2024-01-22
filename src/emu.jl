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
const FAKE_COMPAT = true
#const FAKE_COMPAT = false

const DECIMALMODE = true

const FLAG_CARRY = 0x01
const FLAG_ZERO = 0x02
const FLAG_INTERRUPT = 0x04
const FLAG_DECIMAL = 0x08
#/*bits 4 and 5.*/
const FLAG_BREAK = 0x10
const FLAG_CONSTANT = 0x20
const FLAG_OVERFLOW = 0x40
const FLAG_SIGN = 0x80

const BASE_STACK = 0x100

const K = 1024

abstract type AbstractCpu end

@kwdef mutable struct Cpu{T} <: AbstractCpu
    pc::UInt16 = 0x0000
    a::UInt8 = 0x00
    x::UInt8 = 0x00
    y::UInt8 = 0x00
    sp::UInt8 = 0x00
    status::UInt8 = 0x00
    instructions::Int64 = 0
    clockticks6502::Int64 = 0
    clockgoal6502::Int64 = 0
    oldpc::UInt16 = 0x0000
    # addressing
    ea::UInt16 = 0x0000
    reladdr::UInt16 = 0x0000
    # temporary values
    value::UInt16 = 0x0000
    result::UInt16 = 0x0000
    opcode::UInt8 = 0x00
    oldstatus::UInt8 = 0x00
    penaltyop::UInt8 = 0x00
    penaltyaddr::UInt8 = 0x00
    memory::MVector{64K, UInt8} = zeros(MVector{64K, UInt8})
    user_data::T
end

function copy(src::Cpu{T}) where {T}
    new = Cpu{T}(; src.user_data)
    copy(src, new)
    new
end

function copy(src::Cpu, dst::Cpu)
     dst.pc = src.pc
     dst.a = src.a
     dst.x = src.x
     dst.y = src.y
     dst.sp = src.sp
     dst.status = src.status
     dst.instructions = src.instructions
     dst.clockticks6502 = src.clockticks6502
     dst.clockgoal6502 = src.clockgoal6502
     dst.oldpc = src.oldpc
     dst.ea = src.ea
     dst.reladdr = src.reladdr
     dst.value = src.value
     dst.result = src.result
     dst.opcode = src.opcode
     dst.oldstatus = src.oldstatus
     dst.penaltyop = src.penaltyop
     dst.penaltyaddr = src.penaltyaddr
     dst.memory = src.memory
end

# Basic Cpu does not interfere with computation
read6502(cpu::AbstractCpu, addr::UInt16) = cpu.memory[addr + 1]
write6502(cpu::AbstractCpu, addr::UInt16, value::UInt8) = cpu.memory[addr + 1] = value
hookexternal(::AbstractCpu) = nothing

#opsyms = SVector(
#        |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  |
##=0 :brk_6502, :ora, :jam, :slo, :nop, :ora, :asl, :slo, :php, :ora, :asl, :anc, :nop, :ora, :asl, :slo, #=0=#
##=1      :bpl, :ora, :jam, :slo, :nop, :ora, :asl, :slo, :clc, :ora, :nop, :slo, :nop, :ora, :asl, :slo, #=1=#
##=2      :jsr, :and, :jam, :rla, :bit, :and, :rol, :rla, :plp, :and, :rol, :nop, :bit, :and, :rol, :rla, #=2=#
##=3      :bmi, :and, :jam, :rla, :nop, :and, :rol, :rla, :sec, :and, :nop, :rla, :nop, :and, :rol, :rla, #=3=#
##=4      :rti, :eor, :jam, :sre, :nop, :eor, :lsr, :sre, :pha, :eor, :lsr, :nop, :jmp, :eor, :lsr, :sre, #=4=#
##=5      :bvc, :eor, :jam, :sre, :nop, :eor, :lsr, :sre, :cli, :eor, :nop, :sre, :nop, :eor, :lsr, :sre, #=5=#
##=6      :rts, :adc, :jam, :rra, :nop, :adc, :ror, :rra, :pla, :adc, :ror, :nop, :jmp, :adc, :ror, :rra, #=6=#
##=7      :bvs, :adc, :jam, :rra, :nop, :adc, :ror, :rra, :sei, :adc, :nop, :rra, :nop, :adc, :ror, :rra, #=7=#
##=8      :nop, :sta, :nop, :sax, :sty, :sta, :stx, :sax, :dey, :nop, :txa, :nop, :sty, :sta, :stx, :sax, #=8=#
##=9      :bcc, :sta, :nop, :nop, :sty, :sta, :stx, :sax, :tya, :sta, :txs, :nop, :nop, :sta, :nop, :nop, #=9=#
##=A      :ldy, :lda, :ldx, :lax, :ldy, :lda, :ldx, :lax, :tay, :lda, :tax, :nop, :ldy, :lda, :ldx, :lax, #=A=#
##=B      :bcs, :lda, :nop, :lax, :ldy, :lda, :ldx, :lax, :clv, :lda, :tsx, :lax, :ldy, :lda, :ldx, :lax, #=B=#
##=C      :cpy, :cmp, :nop, :dcp, :cpy, :cmp, :dec, :dcp, :iny, :cmp, :dex, :nop, :cpy, :cmp, :dec, :dcp, #=C=#
##=D      :bne, :cmp, :nop, :dcp, :nop, :cmp, :dec, :dcp, :cld, :cmp, :nop, :dcp, :nop, :cmp, :dec, :dcp, #=D=#
##=E      :cpx, :sbc, :nop, :isb, :cpx, :sbc, :inc, :isb, :inx, :sbc, :nop, :sbc, :cpx, :sbc, :inc, :isb, #=E=#

##=F      :beq, :sbc, :nop, :isb, :nop, :sbc, :inc, :isb, :sed, :sbc, :nop, :isb, :nop, :sbc, :inc, :isb  #=F=#
#)

ticktable = SVector(
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

saveaccum(cpu::AbstractCpu, n) = cpu.a = UInt8(n & 0xFF)

#/*flag modifiers*/
setcarry(cpu::AbstractCpu) = cpu.status |= FLAG_CARRY
clearcarry(cpu::AbstractCpu) = cpu.status &= (~FLAG_CARRY)
setzero(cpu::AbstractCpu) = cpu.status |= FLAG_ZERO
clearzero(cpu::AbstractCpu) = cpu.status &= (~FLAG_ZERO)
setinterrupt(cpu::AbstractCpu) = cpu.status |= FLAG_INTERRUPT
clearinterrupt(cpu::AbstractCpu) = cpu.status &= (~FLAG_INTERRUPT)
setdecimal(cpu::AbstractCpu) = cpu.status |= FLAG_DECIMAL
cleardecimal(cpu::AbstractCpu) = cpu.status &= (~FLAG_DECIMAL)
setoverflow(cpu::AbstractCpu) = cpu.status |= FLAG_OVERFLOW
clearoverflow(cpu::AbstractCpu) = cpu.status &= (~FLAG_OVERFLOW)
setsign(cpu::AbstractCpu) = cpu.status |= FLAG_SIGN
clearsign(cpu::AbstractCpu) = cpu.status &= (~FLAG_SIGN)

function setzeroif(cpu::AbstractCpu, condition::Bool)
    if condition
        setzero(cpu)
    else
        clearzero(cpu)
    end
end

function setsignif(cpu::AbstractCpu, condition::Bool)
    if condition
        setsign(cpu)
    else
        clearsign(cpu)
    end
end

function setcarryif(cpu::AbstractCpu, condition::Bool)
    if condition
        setcarry(cpu)
    else
        clearcarry(cpu)
    end
end

function setoverflowif(cpu::AbstractCpu, condition::Bool)
    if condition
        setoverflow(cpu)
    else
        clearoverflow(cpu)
    end
end

#/*flag calculation */
zerocalc(cpu::AbstractCpu, n) = setzeroif(cpu, (n & 0x00FF) == 0)
signcalc(cpu::AbstractCpu, n) = setsignif(cpu, (n & 0x0080) != 0)
carrycalc(cpu::AbstractCpu, n) = setcarryif(cpu, (n & 0xFF00) != 0)
overflowcalc(cpu::AbstractCpu, result, accumulator, memory) =
    setoverflowif(cpu, ((result ⊻ UInt16(accumulator)) & ((result ⊻ memory) & 0x80)) != 0)

#/*a few general functions used by various other functions*/
function push_6502_16(cpu::AbstractCpu, pushval::UInt16)
    write6502(cpu, BASE_STACK + cpu.sp, UInt8((pushval >> 8) & 0xFF))
    write6502(cpu, BASE_STACK + UInt16((cpu.sp - 0x1) & 0xFF), UInt8(pushval & 0xFF))
    cpu.sp -= 0x2;
end

function push_6502_8(cpu::AbstractCpu, pushval::UInt8)
    write6502(cpu, BASE_STACK + cpu.sp, pushval)
    cpu.sp -= 0x1
end

function pull_6502_16(cpu::AbstractCpu)
    temp16 = read6502(cpu, BASE_STACK + ((cpu.sp + 0x01) & 0xFF)) | (UInt16(read6502(cpu, BASE_STACK + ((cpu.sp + 0x02) & 0xFF))) << 8)
    cpu.sp += 0x2;
    return temp16
end

function pull_6502_8(cpu::AbstractCpu)
    cpu.sp += 0x1
    return read6502(cpu, BASE_STACK + cpu.sp)
end

function mem_6502_read16(cpu::AbstractCpu, addr::UInt16)
    return UInt16(read6502(cpu, addr)) |
        (UInt16(read6502(cpu, addr + 0x1)) << 8)
end

function reset6502(cpu::AbstractCpu)
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
    cpu.clockticks6502 = 0
    cpu.instructions = 0
    cpu.pc = mem_6502_read16(cpu, 0xfffc)
    cpu.sp = 0xfd
    cpu.status |= FLAG_CONSTANT | FLAG_INTERRUPT
end

#/*addressing mode functions, calculates effective addresses*/
function imp(cpu::AbstractCpu)
    TEST_COMPAT && read6502(cpu, cpu.pc)
    nothing
end

#/*addressing mode functions, calculates effective addresses*/
function acc(cpu::AbstractCpu)
    TEST_COMPAT && read6502(cpu, cpu.pc)
end

#/*addressing mode functions, calculates effective addresses*/
function imm(cpu::AbstractCpu)
    cpu.ea = cpu.pc
    cpu.pc += 0x1
end

function zp(cpu::AbstractCpu) # /*zero-page*/
    cpu.ea = UInt16(read6502(cpu, cpu.pc))
    cpu.pc += 0x1
end

function zpx(cpu::AbstractCpu) # /*zero-page,X*/
    local zp = UInt16(read6502(cpu, cpu.pc))
    TEST_COMPAT && cpu.x != 0 && read6502(cpu, zp)
    cpu.ea = (zp + UInt16(cpu.x)) & 0xFF #/*zero-page wraparound*/
    cpu.pc += 0x1
end

function zpy(cpu::AbstractCpu) # /*zero-page,Y*/
    cpu.ea = (UInt16(read6502(cpu, cpu.pc)) + UInt16(cpu.y)) & 0xFF # /*zero-page wraparound*/
    cpu.pc += 0x1
end

function rel(cpu::AbstractCpu) #/*relative for branch ops (8-bit immediate value, sign-extended)*/
    cpu.reladdr = UInt16(read6502(cpu, cpu.pc))
    cpu.pc += 0x1
    if (cpu.reladdr & 0x80) != 0
        cpu.reladdr |= 0xFF00
    end
end

function abso(cpu::AbstractCpu) #/*absolute*/
    cpu.ea = UInt16(read6502(cpu, cpu.pc)) | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8)
    cpu.pc += 0x2
end


function absx(cpu::AbstractCpu) #/*absolute,X*/
    local startpage
    local addr = UInt16(read6502(cpu, cpu.pc))
    cpu.ea = addr | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8)
    startpage = cpu.ea & 0xFF00
    cpu.ea += UInt16(cpu.x)

    if (startpage != (cpu.ea & 0xFF00)) #/*one cycle penlty for page-crossing on some opcodes*/
        TEST_COMPAT && read6502(cpu, startpage | (cpu.ea & 0xFF))
        cpu.penaltyaddr = 0x1
    end

    cpu.pc += 0x2
end

function absy(cpu::AbstractCpu) # /*absolute,Y*/
    local startpage
    local addr = UInt16(read6502(cpu, cpu.pc))
    cpu.ea = addr | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8);
    startpage = cpu.ea & 0xFF00;
    cpu.ea += UInt16(cpu.y)

    if (startpage != (cpu.ea & 0xFF00)) # /*one cycle penlty for page-crossing on some opcodes*/
        TEST_COMPAT && read6502(cpu, startpage | (cpu.ea & 0xFF))
        cpu.penaltyaddr = 0x1;
    end

    cpu.pc += 0x2;
end

function ind(cpu::AbstractCpu) # /*indirect*/
    local eahelp, eahelp2
    eahelp = UInt16(read6502(cpu, cpu.pc)) | UInt16(UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8);
    eahelp2 = (eahelp & 0xFF00) | ((eahelp + 0x0001) & 0x00FF) # /*replicate 6502 page-boundary wraparound bug*/
    cpu.ea = UInt16(read6502(cpu, eahelp)) | (UInt16(read6502(cpu, eahelp2)) << 8)
    cpu.pc += 0x2;
end

function indx(cpu::AbstractCpu) # /* (indirect,X)*/
    local eahelp
    local zp = UInt16(read6502(cpu, cpu.pc))
    TEST_COMPAT && cpu.x != 0 && read6502(cpu, zp)
    eahelp = UInt16((zp + UInt16(cpu.x)) & 0xFF) # /*zero-page wraparound for table pointer*/
    cpu.pc += 0x1
    # todo the first and with 0xFF is redundant because of the first line
    cpu.ea = UInt16(read6502(cpu, eahelp & 0x00FF)) | (UInt16(read6502(cpu, UInt16((eahelp+0x1) & 0x00FF))) << 8);
end

function indy(cpu::AbstractCpu) # /* (indirect),Y*/
    local eahelp, eahelp2, startpage
    eahelp = UInt16(read6502(cpu, cpu.pc));
    cpu.pc += 0x1
    eahelp2 = (eahelp & 0xFF00) | ((eahelp + 0x0001) & 0x00FF) # /*zero-page wraparound*/
    cpu.ea = UInt16(read6502(cpu, eahelp)) | (UInt16(read6502(cpu, eahelp2)) << 8);
    startpage = cpu.ea & 0xFF00;
    cpu.ea += UInt16(cpu.y)

    if startpage != (cpu.ea & 0xFF00) # /*one cycle penlty for page-crossing on some opcodes*/
        cpu.penaltyaddr = 0x1;
        TEST_COMPAT && read6502(cpu, startpage | (cpu.ea & 0xFF))
    end
end

function getvalue(cpu::AbstractCpu)
    addrsyms[cpu.opcode + 1] == :acc && return UInt16(cpu.a)
    return UInt16(read6502(cpu, cpu.ea));
end

function getvalue16(cpu::AbstractCpu)
    return UInt16(read6502(cpu, cpu.ea)) | (UInt16(read6502(cpu, UInt16(cpu.ea+0x1))) << 8);
end


function putvalue(cpu, saveval::UInt16)
    if addrsyms[cpu.opcode + 1] == :acc
        cpu.a = UInt8(saveval & 0x00FF);
    else
        write6502(cpu, cpu.ea, UInt8(saveval & 0x00FF))
    end
    if TEST_COMPAT
        cpu.value = saveval
    end
end

#/*instruction handler functions*/

function adc_nes(cpu::AbstractCpu)
    if (cpu.status & FLAG_DECIMAL) != 0
        local AL, A, result_dec;
        cpu.penaltyop = 0x1
        A = cpu.a
        cpu.value = getvalue(cpu)
        result_dec = UInt16(A) + cpu.value + UInt16(cpu.status & FLAG_CARRY) # /*dec*/
        
        AL = (A & 0x0F) + (cpu.value & 0x0F) + UInt16(cpu.status & FLAG_CARRY) # /*SEQ 1A OR 2A*/
        if AL >= 0xA
            AL = ((AL + 0x06) & 0x0F) + 0x10 # /*SEQ 1B OR SEQ 2B*/
        end
        A = (A & 0xF0) + (cpu.value & 0xF0) + AL # /*SEQ2C OR SEQ 1C*/
        setsignif(cpu, (A & 0x80) != 0) # /*SEQ 2E it says "bit 7"*/
        if A >= 0xA0
            A += 0x60 # /*SEQ 1E*/
        end
        cpu.result = A # /*1F*/
        setoverflowif(cpu, (A & 0xff80) != 0)
        setcarryif(cpu, A >= 0x100) # /*SEQ 1G*/
		
        zerocalc(cpu, result_dec) # /*Original nmos does zerocalc on the binary result.*/
        saveaccum(cpu, cpu.result)
    else 
        adc_non_nes(cpu)
    end
end

function adc_non_nes(cpu::AbstractCpu)
    cpu.penaltyop = 0x1
    cpu.value = getvalue(cpu)
    cpu.result = UInt16(cpu.a) + cpu.value + UInt16(cpu.status & FLAG_CARRY)
    zerocalc(cpu, cpu.result)
    overflowcalc(cpu, cpu.result, cpu.a, cpu.value)
    signcalc(cpu, cpu.result)
    # FAKE: changed for consistency
    if DECIMALMODE && (cpu.status & FLAG_DECIMAL) != 0
        cpu.result += ((((cpu.result + 0x66) ⊻ UInt16(cpu.a) ⊻ cpu.value) >> 3) & 0x22) * 3
    end
    carrycalc(cpu, cpu.result)
    saveaccum(cpu, cpu.result)
end

# to emulate NES, make a custom adc method call adc_nes instead of adc_non_nes
adc(cpu::AbstractCpu) = adc_non_nes(cpu)

# AND oper + set C as ASL, immediate only
function anc(cpu::AbstractCpu)
    and(cpu)
    carrycalc(cpu, cpu.result << 1)
end

function and(cpu::AbstractCpu, reuse_value = false)
    cpu.penaltyop = 0x1
    if !reuse_value
        cpu.value = getvalue(cpu)
    end
    cpu.result = UInt16(cpu.a) & cpu.value
   
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    saveaccum(cpu, cpu.result)
end

function asl(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    TEST_COMPAT && addrsyms[cpu.opcode + 1] != :acc && putvalue(cpu, cpu.value)
    cpu.result = cpu.value << 1

    carrycalc(cpu, cpu.result)
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    putvalue(cpu, cpu.result)
end

function check_cross_page_boundary(cpu::AbstractCpu)
    TEST_COMPAT &&
        read6502(cpu, cpu.oldpc)
    if (cpu.oldpc & 0xFF00) != (cpu.pc & 0xFF00)
        cpu.clockticks6502 += 2 # /*check if jump crossed a page boundary*/
        if TEST_COMPAT
            read6502(cpu, (cpu.oldpc & 0xFF00) | (cpu.pc & 0xFF))
        end
    else
        cpu.clockticks6502 += 1
    end
end

function bcc(cpu::AbstractCpu)
    if cpu.status & FLAG_CARRY == 0
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        check_cross_page_boundary(cpu)
    end
end

function bcs(cpu::AbstractCpu)
    if cpu.status & FLAG_CARRY == FLAG_CARRY
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        check_cross_page_boundary(cpu)
    end
end

function beq(cpu::AbstractCpu)
    if cpu.status & FLAG_ZERO == FLAG_ZERO
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        check_cross_page_boundary(cpu)
    end
end


function bit(cpu::AbstractCpu)
    cpu.value = getvalue(cpu);
    cpu.result = UInt16(cpu.a) & cpu.value
   
    zerocalc(cpu, cpu.result)
    cpu.status = (cpu.status & 0x3F) | (UInt8(cpu.value) & 0xC0);
end

function bmi(cpu::AbstractCpu)
    if cpu.status & FLAG_SIGN == FLAG_SIGN
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        if cpu.oldpc & 0xFF00 != cpu.pc & 0xFF00
            cpu.clockticks6502 += 2 # /*check if jump crossed a page boundary*/
        else
            cpu.clockticks6502 += 1
        end
    end
end

function bne(cpu::AbstractCpu)
    if  cpu.status & FLAG_ZERO == 0
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        check_cross_page_boundary(cpu)
    end
end

function bpl(cpu::AbstractCpu)
    if cpu.status & FLAG_SIGN == 0
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        check_cross_page_boundary(cpu)
    end
end

function brk_6502(cpu)
    TEST_COMPAT && read6502(cpu, cpu.pc)
    cpu.pc += 0x1
    push_6502_16(cpu, cpu.pc)
    push_6502_8(cpu, cpu.status | FLAG_BREAK)
    setinterrupt(cpu)
    cpu.pc = UInt16(read6502(cpu, 0xFFFE)) | (UInt16(read6502(cpu, 0xFFFF)) << 8)
end

function bvc(cpu::AbstractCpu)
    if cpu.status & FLAG_OVERFLOW == 0
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        check_cross_page_boundary(cpu)
    end
end

function bvs(cpu::AbstractCpu)
    if cpu.status & FLAG_OVERFLOW == FLAG_OVERFLOW
        cpu.oldpc = cpu.pc
        cpu.pc += cpu.reladdr
        check_cross_page_boundary(cpu)
    end
end


clc(cpu::AbstractCpu)  = clearcarry(cpu)

cld(cpu::AbstractCpu) = cleardecimal(cpu)

cli(cpu::AbstractCpu) = clearinterrupt(cpu)

clv(cpu::AbstractCpu) = clearoverflow(cpu)

function cmp(cpu::AbstractCpu)
    cpu.penaltyop = 1
    cpu.value = getvalue(cpu)
    cpu.result = UInt16(cpu.a) - cpu.value
   
    setcarryif(cpu, cpu.a >= UInt8(cpu.value & 0x00FF))
    setzeroif(cpu, cpu.a == UInt8(cpu.value & 0x00FF))
    signcalc(cpu, cpu.result)
end


function cpx(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    cpu.result = UInt16(cpu.x) - cpu.value
   
    setcarryif(cpu, cpu.x >= UInt8(cpu.value & 0x00FF))
    setzeroif(cpu, cpu.x == UInt8(cpu.value & 0x00FF))
    signcalc(cpu, cpu.result)
end

function cpy(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    cpu.result = UInt16(cpu.y) - cpu.value
   
    setcarryif(cpu, cpu.y >= UInt8(cpu.value & 0x00FF))
    setzeroif(cpu, cpu.y == UInt8(cpu.value & 0x00FF))
    signcalc(cpu, cpu.result)
end

function dec(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    cpu.result = cpu.value - 0x1
   
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    putvalue(cpu, cpu.result)
end


function dex(cpu::AbstractCpu)
    cpu.x -= 0x1
   
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
end

function dey(cpu::AbstractCpu)
    cpu.y -= 0x1
   
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
end

function eor(cpu::AbstractCpu)
    cpu.penaltyop = 1;
    cpu.value = getvalue(cpu);
    cpu.result = UInt16(cpu.a) ⊻ cpu.value;
   
    zerocalc(cpu, cpu.result);
    signcalc(cpu, cpu.result);
   
    saveaccum(cpu, cpu.result);
end

function inc(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    cpu.result = cpu.value + 0x1
   
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    putvalue(cpu, cpu.result)
end

function inx(cpu::AbstractCpu)
    cpu.x += 0x1
   
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
end

function iny(cpu::AbstractCpu)
    cpu.y += 0x1
   
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
end

function jam(cpu::AbstractCpu)
    cpu.pc -= 1
end

jmp(cpu::AbstractCpu) = cpu.pc = cpu.ea

function jsr(cpu::AbstractCpu)
    local oldsp = cpu.sp
    TEST_COMPAT && (cpu.pc - 0x1) != 0x0100 | oldsp && # not in page zero
        read6502(cpu, 0x0100 | oldsp)
    push_6502_16(cpu, cpu.pc - 0x1)
    if TEST_COMPAT && (cpu.pc - 1) == 0x0100 | oldsp # in page zero
        cpu.ea = (cpu.ea & 0xFF) | (UInt16(read6502(cpu, 0x0100 | oldsp)) << 8)
    end
    cpu.pc = cpu.ea
end

function lda(cpu::AbstractCpu)
    cpu.penaltyop = 0x1
    cpu.value = getvalue(cpu)
    cpu.a = UInt8(cpu.value & 0x00FF)
   
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
end

function ldx(cpu::AbstractCpu)
    cpu.penaltyop = 1
    cpu.value = getvalue(cpu)
    cpu.x = UInt8(cpu.value & 0x00FF)
   
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
end

function ldy(cpu::AbstractCpu)
    cpu.penaltyop = 0x1
    cpu.value = getvalue(cpu)
    cpu.y = UInt8(cpu.value & 0x00FF)
   
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
end

function lsr(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    cpu.result = cpu.value >> 1
   
    setcarryif(cpu, (cpu.value & 0x1) != 0)
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    putvalue(cpu, cpu.result)
end

function nop(cpu::AbstractCpu)
    local op = cpu.opcode
    if op == 0x1C || op == 0x3C || op == 0x5C || op == 0x7C || op == 0xDC || op == 0xFC
        cpu.penaltyop = 0x1
    end
    addr = addrsyms[op + 1]
    #@printf "op: %02x addr: %s\n" op string(addr)
    if TEST_COMPAT
        if addr == :absx || addr == :absy
            cpu.penaltyaddr == 0 &&
                getvalue(cpu)
        elseif addr != :imm && addr != :abso && addr != :imp
            getvalue(cpu)
        end
    end
end

function ora(cpu::AbstractCpu, reuse_value = false)
    cpu.penaltyop = 0x1
    if !reuse_value
        cpu.value = getvalue(cpu)
    end
    cpu.result = UInt16(cpu.a) | cpu.value
   
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    saveaccum(cpu, cpu.result)
end

pha(cpu::AbstractCpu) = push_6502_8(cpu, cpu.a)

function php(cpu::AbstractCpu)
    TEST_COMPAT && read6502(cpu, cpu.pc)
    push_6502_8(cpu, cpu.status | FLAG_BREAK)
end

function pla(cpu::AbstractCpu)
    cpu.a = pull_6502_8(cpu)
   
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
end

plp(cpu::AbstractCpu) = cpu.status = pull_6502_8(cpu) | FLAG_CONSTANT
# FAKE: changed for consistency
#plp(cpu::AbstractCpu) = cpu.status = pull_6502_8(cpu) | FLAG_CONSTANT | FLAG_BREAK

function rol(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    FAKE_COMPAT && putvalue(cpu, cpu.value)
    cpu.result = (cpu.value << 1) | (cpu.status & FLAG_CARRY)
   
    carrycalc(cpu, cpu.result)
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    putvalue(cpu, cpu.result)
end

function ror(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    FAKE_COMPAT && putvalue(cpu, cpu.value)
    cpu.result = (cpu.value >> 1) | ((cpu.status & FLAG_CARRY) << 7)
   
    setcarryif(cpu, (cpu.value & 1) != 0)
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    putvalue(cpu, cpu.result)
end

function rti(cpu::AbstractCpu)
    #cpu.status = pull_6502_8(cpu)
    # FAKE: changed for consistency
    cpu.status = pull_6502_8(cpu) | FLAG_CONSTANT | FLAG_BREAK
    cpu.value = pull_6502_16(cpu)
    cpu.pc = cpu.value;
end

function rts(cpu::AbstractCpu)
    cpu.value = pull_6502_16(cpu)
    cpu.pc = cpu.value + 0x0001
end

function sbc_nes(cpu::AbstractCpu)
    if cpu.status & FLAG_DECIMAL
    	local result_dec, A, AL, B, C;
        cpu.penaltyop = 1
    	A = cpu.a
    	C = UInt16(cpu.status & FLAG_CARRY)
     	cpu.value = getvalue(cpu);B = cpu.value;cpu.value = cpu.value ⊻ 0x00FF
    	result_dec = UInt16(cpu.a) + cpu.value + UInt16(cpu.status & FLAG_CARRY) # /*dec*/
		#/*Both Cmos and Nmos*/
    	carrycalc(cpu, result_dec)
    	overflowcalc(cpu, result_dec, cpu.a, cpu.value)
    	#/*NMOS ONLY*/
    	signcalc(cpu, result_dec)
    	zerocalc(cpu, result_dec)
		#/*Sequence 3 is NMOS ONLY*/
    	AL = (A & 0x0F) - (B & 0x0F) + C -1 # /* 3a*/
    	if AL & 0x8000 != 0
            AL = ((AL - 0x06) & 0x0F) - 0x10 # /*3b*/
        end
    	A = (A & 0xF0) - (B & 0xF0) + AL # /*3c*/
    	if A & 0x8000 != 0
            A = A - 0x60 # /*3d*/
        end
    	cpu.result = A # /*3e*/
        saveaccum(cpu, cpu.result)
    else 
        sbc_non_nes(cpu)
    end
end

function sbc_non_nes(cpu::AbstractCpu)
    cpu.penaltyop = 1
    cpu.value = getvalue(cpu) ⊻ 0x00FF
    if DECIMALMODE && (cpu.status & FLAG_DECIMAL) != 0
        cpu.value -= 0x0066
    end
    cpu.result = UInt16(cpu.a) + cpu.value + UInt16(cpu.status & FLAG_CARRY)
    zerocalc(cpu, cpu.result)
    overflowcalc(cpu, cpu.result, cpu.a, cpu.value)
    signcalc(cpu, cpu.result)
    if DECIMALMODE && (cpu.status & FLAG_DECIMAL) != 0
        cpu.result += ((((cpu.result + 0x66) ⊻ UInt16(cpu.a) ⊻ cpu.value) >> 3) & 0x22) * 0x3
    end
    carrycalc(cpu, cpu.result)
    saveaccum(cpu, cpu.result)
end

# to emulate NES, make a custom adc method call sbc_nes instead of sbc_non_nes
sbc(cpu::AbstractCpu) = sbc_non_nes(cpu::AbstractCpu)

sec(cpu) = setcarry(cpu)

sed(cpu) = setdecimal(cpu)

sei(cpu) = setinterrupt(cpu)

sta(cpu) = putvalue(cpu, UInt16(cpu.a))

stx(cpu) = putvalue(cpu, UInt16(cpu.x))

sty(cpu) = putvalue(cpu, UInt16(cpu.y))

function tax(cpu::AbstractCpu)
    cpu.x = cpu.a
   
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
end

function tay(cpu::AbstractCpu)
    cpu.y = cpu.a
   
    zerocalc(cpu, cpu.y)
    signcalc(cpu, cpu.y)
end

function tsx(cpu::AbstractCpu)
    cpu.x = cpu.sp
   
    zerocalc(cpu, cpu.x)
    signcalc(cpu, cpu.x)
end

function txa(cpu::AbstractCpu)
    cpu.a = cpu.x
   
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
end

txs(cpu::AbstractCpu) = cpu.sp = cpu.x

function tya(cpu::AbstractCpu)
    cpu.a = cpu.y
   
    zerocalc(cpu, cpu.a)
    signcalc(cpu, cpu.a)
end

#/*undocumented instructions~~~~~~~~~~~~~~~~~~~~~~~~~*/
# to disable them, override them to call nop()
function lax(cpu::AbstractCpu)
    lda(cpu)
    ldx(cpu)
end

function sax(cpu::AbstractCpu)
    sta(cpu)
    stx(cpu)
    putvalue(cpu, UInt16(cpu.a & cpu.x))
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end

function dcp(cpu::AbstractCpu)
    dec(cpu)
    cmp(cpu)
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end

function isb(cpu::AbstractCpu)
    inc(cpu)
    sbc(cpu)
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end

function slo(cpu::AbstractCpu)
    asl(cpu)
    ora(cpu, true)
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end

function rla(cpu::AbstractCpu)
    rol(cpu)
    and(cpu, true)
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end

function sre(cpu::AbstractCpu)
    lsr(cpu)
    eor(cpu)
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end

function rra(cpu::AbstractCpu)
    ror(cpu)
    adc(cpu)
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end

function nmi6502(cpu::AbstractCpu)
    push_6502_16(cpu, cpu.pc)
    push_6502_8(cpu, cpu.status  & ~FLAG_BREAK)
    cpu.status |= FLAG_INTERRUPT
    cpu.pc = UInt16(read6502(cpu, 0xFFFA)) | (UInt16(read6502(cpu, 0xFFFB)) << 8)
end

function irq6502(cpu)
	#/*
    #push_6502_16(pc);
    #push_6502_8(status);
    #status |= FLAG_INTERRUPT;
    #pc = (ushort)read6502(0xFFFE) | ((ushort)read6502(0xFFFF) << 8);
    #*/
	if (cpu.status & FLAG_INTERRUPT) == 0
		push_6502_16(cpu, cpu.pc)
		push_6502_8(cpu, cpu.status & ~FLAG_BREAK)
		cpu.status |= FLAG_INTERRUPT
		#/*pc = mem_6502_read16(0xfffe);*/
		cpu.pc = UInt16(read6502(cpu, 0xFFFE)) | (UInt16(read6502(cpu, 0xFFFF)) << 8)
    end
end

function exec6502(cpu::AbstractCpu, tickcount::Int64)
	#/*
	#	BUG FIX:
	#	overflow of unsigned 32 bit integer causes emulation to hang.
	#	An instruction might cause the tick count to wrap around into the billions.

	#	The system is changed so that now clockticks 6502 is reset every single time that exec is called.
	#*/
    cpu.clockgoal6502 = tickcount;
    cpu.clockticks6502 = 0;
    while cpu.clockticks6502 < cpu.clockgoal6502
        inner_step6502(cpu)
    end
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

function address(c)
    o = c.opcode
    if     o==0x00  imp(c) elseif o==0x01  indx(c) elseif o==0x02  imp(c) elseif o==0x03  imp(c)
    elseif o==0x04   zp(c) elseif o==0x05    zp(c) elseif o==0x06   zp(c) elseif o==0x07   zp(c)
    elseif o==0x08  imp(c) elseif o==0x09   imm(c) elseif o==0x0A  acc(c) elseif o==0x0B  imm(c)
    elseif o==0x0C abso(c) elseif o==0x0D  abso(c) elseif o==0x0E abso(c) elseif o==0x0F abso(c)
    elseif o==0x10  rel(c) elseif o==0x11  indy(c) elseif o==0x12  imp(c) elseif o==0x13 indy(c)
    elseif o==0x14  zpx(c) elseif o==0x15   zpx(c) elseif o==0x16  zpx(c) elseif o==0x17  zpx(c)
    elseif o==0x18  imp(c) elseif o==0x19  absy(c) elseif o==0x1A  imp(c) elseif o==0x1B absy(c)
    elseif o==0x1C absx(c) elseif o==0x1D  absx(c) elseif o==0x1E absx(c) elseif o==0x1F absx(c)
    elseif o==0x20 abso(c) elseif o==0x21  indx(c) elseif o==0x22  imp(c) elseif o==0x23 indx(c)
    elseif o==0x24   zp(c) elseif o==0x25    zp(c) elseif o==0x26   zp(c) elseif o==0x27   zp(c)
    elseif o==0x28  imp(c) elseif o==0x29   imm(c) elseif o==0x2A  acc(c) elseif o==0x2B  imm(c)
    elseif o==0x2C abso(c) elseif o==0x2D  abso(c) elseif o==0x2E abso(c) elseif o==0x2F abso(c)
    elseif o==0x30  rel(c) elseif o==0x31  indy(c) elseif o==0x32  imp(c) elseif o==0x33 indy(c)
    elseif o==0x34  zpx(c) elseif o==0x35   zpx(c) elseif o==0x36  zpx(c) elseif o==0x37  zpx(c)
    elseif o==0x38  imp(c) elseif o==0x39  absy(c) elseif o==0x3A  imp(c) elseif o==0x3B absy(c)
    elseif o==0x3C absx(c) elseif o==0x3D  absx(c) elseif o==0x3E absx(c) elseif o==0x3F absx(c)
    elseif o==0x40  imp(c) elseif o==0x41  indx(c) elseif o==0x42  imp(c) elseif o==0x43 indx(c)
    elseif o==0x44   zp(c) elseif o==0x45    zp(c) elseif o==0x46   zp(c) elseif o==0x47   zp(c)
    elseif o==0x48  imp(c) elseif o==0x49   imm(c) elseif o==0x4A  acc(c) elseif o==0x4B  imm(c)
    elseif o==0x4C abso(c) elseif o==0x4D  abso(c) elseif o==0x4E abso(c) elseif o==0x4F abso(c)
    elseif o==0x50  rel(c) elseif o==0x51  indy(c) elseif o==0x52  imp(c) elseif o==0x53 indy(c)
    elseif o==0x54  zpx(c) elseif o==0x55   zpx(c) elseif o==0x56  zpx(c) elseif o==0x57  zpx(c)
    elseif o==0x58  imp(c) elseif o==0x59  absy(c) elseif o==0x5A  imp(c) elseif o==0x5B absy(c)
    elseif o==0x5C absx(c) elseif o==0x5D  absx(c) elseif o==0x5E absx(c) elseif o==0x5F absx(c)
    elseif o==0x60  imp(c) elseif o==0x61  indx(c) elseif o==0x62  imp(c) elseif o==0x63 indx(c)
    elseif o==0x64   zp(c) elseif o==0x65    zp(c) elseif o==0x66   zp(c) elseif o==0x67   zp(c)
    elseif o==0x68  imp(c) elseif o==0x69   imm(c) elseif o==0x6A  acc(c) elseif o==0x6B  imm(c)
    elseif o==0x6C  ind(c) elseif o==0x6D  abso(c) elseif o==0x6E abso(c) elseif o==0x6F abso(c)
    elseif o==0x70  rel(c) elseif o==0x71  indy(c) elseif o==0x72  imp(c) elseif o==0x73 indy(c)
    elseif o==0x74  zpx(c) elseif o==0x75   zpx(c) elseif o==0x76  zpx(c) elseif o==0x77  zpx(c)
    elseif o==0x78  imp(c) elseif o==0x79  absy(c) elseif o==0x7A  imp(c) elseif o==0x7B absy(c)
    elseif o==0x7C absx(c) elseif o==0x7D  absx(c) elseif o==0x7E absx(c) elseif o==0x7F absx(c)
    elseif o==0x80  imm(c) elseif o==0x81  indx(c) elseif o==0x82  imm(c) elseif o==0x83 indx(c)
    elseif o==0x84   zp(c) elseif o==0x85    zp(c) elseif o==0x86   zp(c) elseif o==0x87   zp(c)
    elseif o==0x88  imp(c) elseif o==0x89   imm(c) elseif o==0x8A  imp(c) elseif o==0x8B  imm(c)
    elseif o==0x8C abso(c) elseif o==0x8D  abso(c) elseif o==0x8E abso(c) elseif o==0x8F abso(c)
    elseif o==0x90  rel(c) elseif o==0x91  indy(c) elseif o==0x92  imp(c) elseif o==0x93 indy(c)
    elseif o==0x94  zpx(c) elseif o==0x95   zpx(c) elseif o==0x96  zpy(c) elseif o==0x97  zpy(c)
    elseif o==0x98  imp(c) elseif o==0x99  absy(c) elseif o==0x9A  imp(c) elseif o==0x9B absy(c)
    elseif o==0x9C absx(c) elseif o==0x9D  absx(c) elseif o==0x9E absy(c) elseif o==0x9F absy(c)
    elseif o==0xA0  imm(c) elseif o==0xA1  indx(c) elseif o==0xA2  imm(c) elseif o==0xA3 indx(c)
    elseif o==0xA4   zp(c) elseif o==0xA5    zp(c) elseif o==0xA6   zp(c) elseif o==0xA7   zp(c)
    elseif o==0xA8  imp(c) elseif o==0xA9   imm(c) elseif o==0xAA  imp(c) elseif o==0xAB  imm(c)
    elseif o==0xAC abso(c) elseif o==0xAD  abso(c) elseif o==0xAE abso(c) elseif o==0xAF abso(c)
    elseif o==0xB0  rel(c) elseif o==0xB1  indy(c) elseif o==0xB2  imp(c) elseif o==0xB3 indy(c)
    elseif o==0xB4  zpx(c) elseif o==0xB5   zpx(c) elseif o==0xB6  zpy(c) elseif o==0xB7  zpy(c)
    elseif o==0xB8  imp(c) elseif o==0xB9  absy(c) elseif o==0xBA  imp(c) elseif o==0xBB absy(c)
    elseif o==0xBC absx(c) elseif o==0xBD  absx(c) elseif o==0xBE absy(c) elseif o==0xBF absy(c)
    elseif o==0xC0  imm(c) elseif o==0xC1  indx(c) elseif o==0xC2  imm(c) elseif o==0xC3 indx(c)
    elseif o==0xC4   zp(c) elseif o==0xC5    zp(c) elseif o==0xC6   zp(c) elseif o==0xC7   zp(c)
    elseif o==0xC8  imp(c) elseif o==0xC9   imm(c) elseif o==0xCA  imp(c) elseif o==0xCB  imm(c)
    elseif o==0xCC abso(c) elseif o==0xCD  abso(c) elseif o==0xCE abso(c) elseif o==0xCF abso(c)
    elseif o==0xD0  rel(c) elseif o==0xD1  indy(c) elseif o==0xD2  imp(c) elseif o==0xD3 indy(c)
    elseif o==0xD4  zpx(c) elseif o==0xD5   zpx(c) elseif o==0xD6  zpx(c) elseif o==0xD7  zpx(c)
    elseif o==0xD8  imp(c) elseif o==0xD9  absy(c) elseif o==0xDA  imp(c) elseif o==0xDB absy(c)
    elseif o==0xDC absx(c) elseif o==0xDD  absx(c) elseif o==0xDE absx(c) elseif o==0xDF absx(c)
    elseif o==0xE0  imm(c) elseif o==0xE1  indx(c) elseif o==0xE2  imm(c) elseif o==0xE3 indx(c)
    elseif o==0xE4   zp(c) elseif o==0xE5    zp(c) elseif o==0xE6   zp(c) elseif o==0xE7   zp(c)
    elseif o==0xE8  imp(c) elseif o==0xE9   imm(c) elseif o==0xEA  imp(c) elseif o==0xEB  imm(c)
    elseif o==0xEC abso(c) elseif o==0xED  abso(c) elseif o==0xEE abso(c) elseif o==0xEF abso(c)
    elseif o==0xF0  rel(c) elseif o==0xF1  indy(c) elseif o==0xF2  imp(c) elseif o==0xF3 indy(c)
    elseif o==0xF4  zpx(c) elseif o==0xF5   zpx(c) elseif o==0xF6  zpx(c) elseif o==0xF7  zpx(c)
    elseif o==0xF8  imp(c) elseif o==0xF9  absy(c) elseif o==0xFA  imp(c) elseif o==0xFB absy(c)
    elseif o==0xFC absx(c) elseif o==0xFD  absx(c) elseif o==0xFE absx(c) elseif o==0xFF absx(c)
    end
end

function opcode(c)
    XXX = nop
    o = c.opcode
    if o==0x00 brk_6502(c) elseif o==0x01 ora(c) elseif o==0x02 jam(c) elseif o==0x03 slo(c)
    elseif o==0x04  nop(c) elseif o==0x05 ora(c) elseif o==0x06 asl(c) elseif o==0x07 slo(c)
    elseif o==0x08  php(c) elseif o==0x09 ora(c) elseif o==0x0A asl(c) elseif o==0x0B anc(c)
    elseif o==0x0C  nop(c) elseif o==0x0D ora(c) elseif o==0x0E asl(c) elseif o==0x0F slo(c)
    elseif o==0x10  bpl(c) elseif o==0x11 ora(c) elseif o==0x12 jam(c) elseif o==0x13 slo(c)
    elseif o==0x14  nop(c) elseif o==0x15 ora(c) elseif o==0x16 asl(c) elseif o==0x17 slo(c)
    elseif o==0x18  clc(c) elseif o==0x19 ora(c) elseif o==0x1A nop(c) elseif o==0x1B slo(c)
    elseif o==0x1C  nop(c) elseif o==0x1D ora(c) elseif o==0x1E asl(c) elseif o==0x1F slo(c)
    elseif o==0x20  jsr(c) elseif o==0x21 and(c) elseif o==0x22 jam(c) elseif o==0x23 rla(c)
    elseif o==0x24  bit(c) elseif o==0x25 and(c) elseif o==0x26 rol(c) elseif o==0x27 rla(c)
    elseif o==0x28  pla(c) elseif o==0x29 and(c) elseif o==0x2A rol(c) elseif o==0x2B nop(c)
    elseif o==0x2C  bit(c) elseif o==0x2D and(c) elseif o==0x2E rol(c) elseif o==0x2F rla(c)
    elseif o==0x30  bmi(c) elseif o==0x31 and(c) elseif o==0x32 jam(c) elseif o==0x33 rla(c)
    elseif o==0x34  nop(c) elseif o==0x35 and(c) elseif o==0x36 rol(c) elseif o==0x37 rla(c)
    elseif o==0x38  sec(c) elseif o==0x39 and(c) elseif o==0x3A nop(c) elseif o==0x3B rla(c)
    elseif o==0x3C  nop(c) elseif o==0x3D and(c) elseif o==0x3E rol(c) elseif o==0x3F rla(c)
    elseif o==0x40  rti(c) elseif o==0x41 eor(c) elseif o==0x42 jam(c) elseif o==0x43 sre(c)
    elseif o==0x44  nop(c) elseif o==0x45 eor(c) elseif o==0x46 lsr(c) elseif o==0x47 sre(c)
    elseif o==0x48  pha(c) elseif o==0x49 eor(c) elseif o==0x4A lsr(c) elseif o==0x4B nop(c)
    elseif o==0x4C  jmp(c) elseif o==0x4D eor(c) elseif o==0x4E lsr(c) elseif o==0x4F sre(c)
    elseif o==0x50  bvc(c) elseif o==0x51 eor(c) elseif o==0x52 jam(c) elseif o==0x53 sre(c)
    elseif o==0x54  nop(c) elseif o==0x55 eor(c) elseif o==0x56 lsr(c) elseif o==0x57 sre(c)
    elseif o==0x58  cli(c) elseif o==0x59 eor(c) elseif o==0x5A nop(c) elseif o==0x5B sre(c)
    elseif o==0x5C  nop(c) elseif o==0x5D eor(c) elseif o==0x5E lsr(c) elseif o==0x5F sre(c)
    elseif o==0x60  rts(c) elseif o==0x61 adc(c) elseif o==0x62 jam(c) elseif o==0x63 rra(c)
    elseif o==0x64  nop(c) elseif o==0x65 adc(c) elseif o==0x66 ror(c) elseif o==0x67 rra(c)
    elseif o==0x68  pla(c) elseif o==0x69 adc(c) elseif o==0x6A ror(c) elseif o==0x6B nop(c)
    elseif o==0x6C  jmp(c) elseif o==0x6D adc(c) elseif o==0x6E ror(c) elseif o==0x6F rra(c)
    elseif o==0x70  bvs(c) elseif o==0x71 adc(c) elseif o==0x72 jam(c) elseif o==0x73 rra(c)
    elseif o==0x74  nop(c) elseif o==0x75 adc(c) elseif o==0x76 ror(c) elseif o==0x77 rra(c)
    elseif o==0x78  sei(c) elseif o==0x79 adc(c) elseif o==0x7A nop(c) elseif o==0x7B rra(c)
    elseif o==0x7C  nop(c) elseif o==0x7D adc(c) elseif o==0x7E ror(c) elseif o==0x7F rra(c)
    elseif o==0x80  nop(c) elseif o==0x81 sta(c) elseif o==0x82 nop(c) elseif o==0x83 sax(c)
    elseif o==0x84  sty(c) elseif o==0x85 sta(c) elseif o==0x86 stx(c) elseif o==0x87 sax(c)
    elseif o==0x88  dey(c) elseif o==0x89 nop(c) elseif o==0x8A txa(c) elseif o==0x8B nop(c)
    elseif o==0x8C  sty(c) elseif o==0x8D sta(c) elseif o==0x8E stx(c) elseif o==0x8F sax(c)
    elseif o==0x90  bcc(c) elseif o==0x91 sta(c) elseif o==0x92 nop(c) elseif o==0x93 nop(c)
    elseif o==0x94  sty(c) elseif o==0x95 sta(c) elseif o==0x96 stx(c) elseif o==0x97 sax(c)
    elseif o==0x98  tya(c) elseif o==0x99 sta(c) elseif o==0x9A txs(c) elseif o==0x9B nop(c)
    elseif o==0x9C  nop(c) elseif o==0x9D sta(c) elseif o==0x9E nop(c) elseif o==0x9F nop(c)
    elseif o==0xA0  ldy(c) elseif o==0xA1 lda(c) elseif o==0xA2 ldx(c) elseif o==0xA3 lax(c)
    elseif o==0xA4  ldy(c) elseif o==0xA5 lda(c) elseif o==0xA6 ldx(c) elseif o==0xA7 lax(c)
    elseif o==0xA8  tay(c) elseif o==0xA9 lda(c) elseif o==0xAA tax(c) elseif o==0xAB nop(c)
    elseif o==0xAC  ldy(c) elseif o==0xAD lda(c) elseif o==0xAE ldx(c) elseif o==0xAF lax(c)
    elseif o==0xB0  XXX(c) elseif o==0xB1 lda(c) elseif o==0xB2 nop(c) elseif o==0xB3 lax(c)
    elseif o==0xB4  ldy(c) elseif o==0xB5 lda(c) elseif o==0xB6 ldx(c) elseif o==0xB7 lax(c)
    elseif o==0xB8  clv(c) elseif o==0xB9 lda(c) elseif o==0xBA tsx(c) elseif o==0xBB lax(c)
    elseif o==0xBC  ldy(c) elseif o==0xBD lda(c) elseif o==0xBE ldx(c) elseif o==0xBF lax(c)
    elseif o==0xC0  cpy(c) elseif o==0xC1 cmp(c) elseif o==0xC2 nop(c) elseif o==0xC3 dcp(c)
    elseif o==0xC4  cpy(c) elseif o==0xC5 cmp(c) elseif o==0xC6 dec(c) elseif o==0xC7 dcp(c)
    elseif o==0xC8  iny(c) elseif o==0xC9 cmp(c) elseif o==0xCA dex(c) elseif o==0xCB nop(c)
    elseif o==0xCC  cpy(c) elseif o==0xCD cmp(c) elseif o==0xCE dec(c) elseif o==0xCF dcp(c)
    elseif o==0xD0  bne(c) elseif o==0xD1 cmp(c) elseif o==0xD2 nop(c) elseif o==0xD3 dcp(c)
    elseif o==0xD4  nop(c) elseif o==0xD5 cmp(c) elseif o==0xD6 dec(c) elseif o==0xD7 dcp(c)
    elseif o==0xD8  cld(c) elseif o==0xD9 cmp(c) elseif o==0xDA nop(c) elseif o==0xDB dcp(c)
    elseif o==0xDC  nop(c) elseif o==0xDD cmp(c) elseif o==0xDE dec(c) elseif o==0xDF dcp(c)
    elseif o==0xE0  cpx(c) elseif o==0xE1 sbc(c) elseif o==0xE2 nop(c) elseif o==0xE3 isb(c)
    elseif o==0xE4  cpx(c) elseif o==0xE5 sbc(c) elseif o==0xE6 inc(c) elseif o==0xE7 isb(c)
    elseif o==0xE8  inx(c) elseif o==0xE9 sbc(c) elseif o==0xEA nop(c) elseif o==0xEB sbc(c)
    elseif o==0xEC  cpx(c) elseif o==0xED sbc(c) elseif o==0xEE inc(c) elseif o==0xEF isb(c)
    elseif o==0xF0  beq(c) elseif o==0xF1 sbc(c) elseif o==0xF2 nop(c) elseif o==0xF3 isb(c)
    elseif o==0xF4  nop(c) elseif o==0xF5 sbc(c) elseif o==0xF6 inc(c) elseif o==0xF7 isb(c)
    elseif o==0xF8  sed(c) elseif o==0xF9 sbc(c) elseif o==0xFA nop(c) elseif o==0xFB isb(c)
    elseif o==0xFC  nop(c) elseif o==0xFD sbc(c) elseif o==0xFE inc(c) elseif o==0xFF isb(c)
    end
end

function inner_step6502(cpu::AbstractCpu)
    cpu.opcode = read6502(cpu, cpu.pc)
    cpu.pc += 0x1
    cpu.status |= FLAG_CONSTANT
    cpu.penaltyop = 0
    cpu.penaltyaddr = 0
    address(cpu)
    opcode(cpu)
    cpu.clockticks6502 += ticktable[cpu.opcode + 1]
    #/*The is commented out in Mike Chamber's usage of the 6502 emulator for MOARNES*/
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 += 1
    end
    cpu.instructions += 1
end

function step6502(cpu::AbstractCpu)
    cpu.penaltyop = 0;
    cpu.penaltyaddr = 0;
	#cpu.clockticks6502 = 0;
    inner_step6502(cpu)
end

end # module Fake6502
