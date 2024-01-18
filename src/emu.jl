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

const TEST_COMPAT = true
#const TEST_COMPAT = false
const FAKE_COMPAT = true

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
    pc::UInt16 = 0
    a::UInt8 = 0
    x::UInt8 = 0
    y::UInt8 = 0
    sp::UInt8 = 0
    status::UInt8 = 0
    instructions::Int64 = 0
    clockticks6502::Int64 = 0
    clockgoal6502::Int64 = 0
    oldpc::UInt16 = 0
    # addressing
    ea::UInt16 = 0
    reladdr::UInt16 = 0
    # temporary values
    value::UInt16 = 0
    result::UInt16 = 0
    opcode::UInt8 = 0
    oldstatus::UInt8 = 0
    penaltyop::UInt8 = 0
    penaltyaddr::UInt8 = 0
    memory::Vector{UInt8} = zeros(UInt8, 64K)
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

#/*addressing mode functions, calculate effective addresses*/
function imp end
function acc end
function imm end
function zp end
function zpx end
function zpy end
function rel end
function abso end
function absx end
function absy end
function ind end
function indx end
function indy end

addrtable = Function[
#      |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  |
#=0=#    imp, indx,  imp, indx,   zp,   zp,   zp,   zp,  imp,  imm,  acc,  imm, abso, abso, abso, abso, #=0=#
#=1=#    rel, indy,  imp, indy,  zpx,  zpx,  zpx,  zpx,  imp, absy,  imp, absy, absx, absx, absx, absx, #=1=#
#=2=#   abso, indx,  imp, indx,   zp,   zp,   zp,   zp,  imp,  imm,  acc,  imm, abso, abso, abso, abso, #=2=#
#=3=#    rel, indy,  imp, indy,  zpx,  zpx,  zpx,  zpx,  imp, absy,  imp, absy, absx, absx, absx, absx, #=3=#
#=4=#    imp, indx,  imp, indx,   zp,   zp,   zp,   zp,  imp,  imm,  acc,  imm, abso, abso, abso, abso, #=4=#
#=5=#    rel, indy,  imp, indy,  zpx,  zpx,  zpx,  zpx,  imp, absy,  imp, absy, absx, absx, absx, absx, #=5=#
#=6=#    imp, indx,  imp, indx,   zp,   zp,   zp,   zp,  imp,  imm,  acc,  imm,  ind, abso, abso, abso, #=6=#
#=7=#    rel, indy,  imp, indy,  zpx,  zpx,  zpx,  zpx,  imp, absy,  imp, absy, absx, absx, absx, absx, #=7=#
#=8=#    imm, indx,  imm, indx,   zp,   zp,   zp,   zp,  imp,  imm,  imp,  imm, abso, abso, abso, abso, #=8=#
#=9=#    rel, indy,  imp, indy,  zpx,  zpx,  zpy,  zpy,  imp, absy,  imp, absy, absx, absx, absy, absy, #=9=#
#=A=#    imm, indx,  imm, indx,   zp,   zp,   zp,   zp,  imp,  imm,  imp,  imm, abso, abso, abso, abso, #=A=#
#=B=#    rel, indy,  imp, indy,  zpx,  zpx,  zpy,  zpy,  imp, absy,  imp, absy, absx, absx, absy, absy, #=B=#
#=C=#    imm, indx,  imm, indx,   zp,   zp,   zp,   zp,  imp,  imm,  imp,  imm, abso, abso, abso, abso, #=C=#
#=D=#    rel, indy,  imp, indy,  zpx,  zpx,  zpx,  zpx,  imp, absy,  imp, absy, absx, absx, absx, absx, #=D=#
#=E=#    imm, indx,  imm, indx,   zp,   zp,   zp,   zp,  imp,  imm,  imp,  imm, abso, abso, abso, abso, #=E=#
#=F=#    rel, indy,  imp, indy,  zpx,  zpx,  zpx,  zpx,  imp, absy,  imp, absy, absx, absx, absx, absx  #=F=#
]

function adc end
function and end
function asl end
function bcc end
function bcs end
function beq end
function bit end
function bmi end
function bne end
function bpl end
function brk_6502 end
function bvc end
function bvs end
function clc end
function cld end
function cli end
function clv end
function cmp end
function cpx end
function cpy end
function dec end
function dex end
function dey end
function eor end
function inc end
function inx end
function iny end
function jmp end
function jsr end
function lda end
function ldx end
function ldy end
function lsr end
function nop end
function ora end
function pha end
function php end
function pla end
function plp end
function rol end
function ror end
function rti end
function rts end
function sbc end
function sec end
function sed end
function sei end
function sta end
function stx end
function sty end
function tax end
function tay end
function tsx end
function txa end
function txs end
function tya end
function lax end
function sax end
function dcp end
function isb end
function slo end
function rla end
function sre end
function rra end

optable = Function[
#        |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  |
#=0=# brk_6502,  ora,  nop,  slo,  nop,  ora,  asl,  slo,  php,  ora,  asl,  nop,  nop,  ora,  asl,  slo, #=0=#
#=1=#      bpl,  ora,  nop,  slo,  nop,  ora,  asl,  slo,  clc,  ora,  nop,  slo,  nop,  ora,  asl,  slo, #=1=#
#=2=#      jsr,  and,  nop,  rla,  bit,  and,  rol,  rla,  plp,  and,  rol,  nop,  bit,  and,  rol,  rla, #=2=#
#=3=#      bmi,  and,  nop,  rla,  nop,  and,  rol,  rla,  sec,  and,  nop,  rla,  nop,  and,  rol,  rla, #=3=#
#=4=#      rti,  eor,  nop,  sre,  nop,  eor,  lsr,  sre,  pha,  eor,  lsr,  nop,  jmp,  eor,  lsr,  sre, #=4=#
#=5=#      bvc,  eor,  nop,  sre,  nop,  eor,  lsr,  sre,  cli,  eor,  nop,  sre,  nop,  eor,  lsr,  sre, #=5=#
#=6=#      rts,  adc,  nop,  rra,  nop,  adc,  ror,  rra,  pla,  adc,  ror,  nop,  jmp,  adc,  ror,  rra, #=6=#
#=7=#      bvs,  adc,  nop,  rra,  nop,  adc,  ror,  rra,  sei,  adc,  nop,  rra,  nop,  adc,  ror,  rra, #=7=#
#=8=#      nop,  sta,  nop,  sax,  sty,  sta,  stx,  sax,  dey,  nop,  txa,  nop,  sty,  sta,  stx,  sax, #=8=#
#=9=#      bcc,  sta,  nop,  nop,  sty,  sta,  stx,  sax,  tya,  sta,  txs,  nop,  nop,  sta,  nop,  nop, #=9=#
#=A=#      ldy,  lda,  ldx,  lax,  ldy,  lda,  ldx,  lax,  tay,  lda,  tax,  nop,  ldy,  lda,  ldx,  lax, #=A=#
#=B=#      bcs,  lda,  nop,  lax,  ldy,  lda,  ldx,  lax,  clv,  lda,  tsx,  lax,  ldy,  lda,  ldx,  lax, #=B=#
#=C=#      cpy,  cmp,  nop,  dcp,  cpy,  cmp,  dec,  dcp,  iny,  cmp,  dex,  nop,  cpy,  cmp,  dec,  dcp, #=C=#
#=D=#      bne,  cmp,  nop,  dcp,  nop,  cmp,  dec,  dcp,  cld,  cmp,  nop,  dcp,  nop,  cmp,  dec,  dcp, #=D=#
#=E=#      cpx,  sbc,  nop,  isb,  cpx,  sbc,  inc,  isb,  inx,  sbc,  nop,  sbc,  cpx,  sbc,  inc,  isb, #=E=#
#=F=#      beq,  sbc,  nop,  isb,  nop,  sbc,  inc,  isb,  sed,  sbc,  nop,  isb,  nop,  sbc,  inc,  isb  #=F=#
]

ticktable = UInt32[
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
]

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
imp(::AbstractCpu) = nothing

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
    cpu.ea = (UInt16(read6502(cpu, cpu.pc)) + UInt16(cpu.x)) & 0xFF #/*zero-page wraparound*/
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
    cpu.ea = UInt16(read6502(cpu, cpu.pc)) | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8)
    startpage = cpu.ea & 0xFF00
    cpu.ea += UInt16(cpu.x)

    if (startpage != (cpu.ea & 0xFF00)) #/*one cycle penlty for page-crossing on some opcodes*/
        cpu.penaltyaddr = 0x1
    end

    cpu.pc += 0x2
end

function absy(cpu::AbstractCpu) # /*absolute,Y*/
    local startpage
    cpu.ea = UInt16(read6502(cpu, cpu.pc)) | (UInt16(read6502(cpu, UInt16(cpu.pc+0x1))) << 8);
    startpage = cpu.ea & 0xFF00;
    cpu.ea += UInt16(cpu.y)

    if (startpage != (cpu.ea & 0xFF00)) # /*one cycle penlty for page-crossing on some opcodes*/
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
    zp = UInt16(read6502(cpu, cpu.pc))
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
    addrtable[cpu.opcode + 1] == acc && return UInt16(cpu.a)
    return UInt16(read6502(cpu, cpu.ea));
end

function getvalue16(cpu::AbstractCpu)
    return UInt16(read6502(cpu, cpu.ea)) | (UInt16(read6502(cpu, UInt16(cpu.ea+0x1))) << 8);
end


function putvalue(cpu, saveval::UInt16)
    if addrtable[cpu.opcode + 1] == acc
        cpu.a = UInt8(saveval & 0x00FF);
    else
        write6502(cpu, cpu.ea, UInt8(saveval & 0x00FF))
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

function and(cpu::AbstractCpu)
    cpu.penaltyop = 0x1
    cpu.value = getvalue(cpu)
    cpu.result = UInt16(cpu.a) & cpu.value
   
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    saveaccum(cpu, cpu.result)
end

function asl(cpu::AbstractCpu)
    cpu.value = getvalue(cpu)
    TEST_COMPAT && addrtable[cpu.opcode + 1] != acc && putvalue(cpu, cpu.value)
    cpu.result = cpu.value << 1

    carrycalc(cpu, cpu.result)
    zerocalc(cpu, cpu.result)
    signcalc(cpu, cpu.result)
   
    putvalue(cpu, cpu.result)
end

function check_cross_page_boundary(cpu::AbstractCpu)
    if (cpu.oldpc & 0xFF00) != (cpu.pc & 0xFF00)
        cpu.clockticks6502 += 2 # /*check if jump crossed a page boundary*/
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
        #TEST_COMPAT && cpu.pc & 0xFF00 != (cpu.pc + cpu.reladdr) & 0xFF00 && read6502(cpu, cpu.pc + 1)
        #TEST_COMPAT && read6502(cpu, cpu.pc + 0x1)
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

jmp(cpu::AbstractCpu) = cpu.pc = cpu.ea

function jsr(cpu::AbstractCpu)
    push_6502_16(cpu, cpu.pc - 0x1)
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
end

function ora(cpu::AbstractCpu)
    cpu.penaltyop = 0x1
    cpu.value = getvalue(cpu)
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

#plp(cpu::AbstractCpu) = cpu.status = pull_6502_8(cpu) | FLAG_CONSTANT
# FAKE: changed for consistency
plp(cpu::AbstractCpu) = cpu.status = pull_6502_8(cpu) | FLAG_CONSTANT | FLAG_BREAK

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
    ora(cpu)
    if cpu.penaltyop != 0 && cpu.penaltyaddr != 0
        cpu.clockticks6502 -= 1
    end
end


function rla(cpu::AbstractCpu)
    rol(cpu)
    and(cpu)
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

function inner_step6502(cpu::AbstractCpu)
    cpu.opcode = read6502(cpu, cpu.pc)
    cpu.pc += 0x1
    cpu.status |= FLAG_CONSTANT
    cpu.penaltyop = 0
    cpu.penaltyaddr = 0
    addrtable[cpu.opcode + 1](cpu)
    optable[cpu.opcode + 1](cpu)
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
