"""
    To use this, override write6502 and inner_step6502 using the specialized methods with
the same names that take additional Trails arguments at the start.

# Examples
```julia
write6502(cpu::Cpu{Rewinder}, addr::UInt16, value::UInt8) = write6502(cpu.user_data, cpu, addr, value)

inner_step6502(cpu::Cpu{Rewinder}, temps::Temps) = inner_step6502(cpu.user_data, cpu, temps)
"""
module Rewinding

using ..Fake6502:
    K, M, SVector, Cpu, Temps, base_inner_step6502, rhex, hex, status, mprintln
using ..Fake6502: dbyte, screen2ascii
import ..Fake6502m: write6502, inner_step6502, pc, opsyms

const SNAPLEN = 4M
#! format: off
const NUMSETS = (
    #        |  0  |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |  A |  B |  C |  D |  E |  F |
    ##=0=#:brk_6502,:ora,:jam,:slo,:nop,:ora,:asl,:slo,:php,:ora,:asl,:anc,:nop,:ora,:asl,:slo, # 0
                3,    0,   0,   1,   0,   0,   0,   1,   1,   0,   0,   0,   0,   0,   0,   1,  # 0
    ##=1=#    :bpl, :ora,:jam,:slo,:nop,:ora,:asl,:slo,:clc,:ora,:nop,:slo,:nop,:ora,:asl,:slo, # 1
                0,    0,   0,   1,   0,   0,   0,   1,   0,   0,   0,   0,   0,   0,   0,   1,  # 1
    ##=2=#    :jsr, :and,:jam,:rla,:bit,:and,:rol,:rla,:plp,:and,:rol,:anc,:bit,:and,:rol,:rla, # 2
                2,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   0,   0,   0,   1,   1,  # 2
    ##=3=#    :bmi, :and,:jam,:rla,:nop,:and,:rol,:rla,:sec,:and,:nop,:rla,:nop,:and,:rol,:rla, # 3
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   1,   0,   0,   1,   1,  # 3
    ##=4=#    :rti, :eor,:jam,:sre,:nop,:eor,:lsr,:sre,:pha,:eor,:lsr,:alr,:jmp,:eor,:lsr,:sre, # 4
                0,    0,   0,   1,   0,   0,   1,   1,   1,   0,   0,   0,   0,   0,   1,   1,  # 4
    ##=5=#    :bvc, :eor,:jam,:sre,:nop,:eor,:lsr,:sre,:cli,:eor,:nop,:sre,:nop,:eor,:lsr,:sre, # 5
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   0,   0,   0,   1,   1,  # 5
    ##=6=#    :rts, :adc,:jam,:rra,:nop,:adc,:ror,:rra,:pla,:adc,:ror,:arr,:jmp,:adc,:ror,:rra, # 6
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   0,   0,   0,   1,   1,  # 6
    ##=7=#    :bvs, :adc,:jam,:rra,:nop,:adc,:ror,:rra,:sei,:adc,:nop,:rra,:nop,:adc,:ror,:rra, # 7
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   1,   0,   0,   1,   1,  # 7
    ##=8=#    :nop, :sta,:nop,:sax,:sty,:sta,:stx,:sax,:dey,:nop,:txa,:ane,:sty,:sta,:stx,:sax, # 8
                0,    1,   0,   1,   1,   1,   1,   1,   0,   0,   0,   0,   1,   1,   1,   1,  # 8
    ##=9=#    :bcc, :sta,:jam,:sha,:sty,:sta,:stx,:sax,:tya,:sta,:txs,:tas,:shy,:sta,:shy,:sha, # 9
                0,    1,   0,   1,   1,   1,   1,   1,   0,   1,   0,   1,   1,   1,   1,   1,  # 9
    ##=A=#    :ldy, :lda,:ldx,:lax,:ldy,:lda,:ldx,:lax,:tay,:lda,:tax,:lxa,:ldy,:lda,:ldx,:lax, # A
                0,    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  # A
    ##=B=#    :bcs, :lda,:jam,:lax,:ldy,:lda,:ldx,:lax,:clv,:lda,:tsx,:las,:ldy,:lda,:ldx,:lax, # B
                0,    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  # B
    ##=C=#    :cpy, :cmp,:nop,:dcp,:cpy,:cmp,:dec,:dcp,:iny,:cmp,:dex,:sbx,:cpy,:cmp,:dec,:dcp, # C
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   0,   0,   0,   1,   1,  # C
    ##=D=#    :bne, :cmp,:jam,:dcp,:nop,:cmp,:dec,:dcp,:cld,:cmp,:nop,:dcp,:nop,:cmp,:dec,:dcp, # D
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   1,   0,   0,   1,   1,  # D
    ##=E=#    :cpx, :sbc,:nop,:isc,:cpx,:sbc,:inc,:isc,:inx,:sbc,:nop,:sbc,:cpx,:sbc,:inc,:isc, # E
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   0,   0,   0,   1,   1,  # E
    ##=F=#    :beq, :sbc,:jam,:isc,:nop,:sbc,:inc,:isc,:sed,:sbc,:nop,:isc,:nop,:sbc,:inc,:isc  # F
                0,    0,   0,   1,   0,   0,   1,   1,   0,   0,   0,   1,   0,   0,   1,   1,  # F
)
#! format: on

@kwdef struct CpuSnapshot
    opcode::UInt8 = 0x00
    pc::UInt16 = 0x0000
    a::UInt8 = 0x00
    x::UInt8 = 0x00
    y::UInt8 = 0x00
    sp::UInt8 = 0x00
    status::UInt8 = 0x00
    instructions::Int64 = 0
    clockticks6502::Int64 = 0
    addr::UInt16 = 0x0000 # affected memory address and up to 3 values (BRK pushes 3)
    value::UInt8 = 0x00
    value2::UInt8 = 0x00
    value3::UInt8 = 0x00
end

struct TrailEntry
    statetrail::Vector{CpuSnapshot}
    TrailEntry() = new(zeros(CpuSnapshot, SNAPLEN))
end

@kwdef mutable struct Rewinder
    curtime::Int64 = 0
    nextfree::Int64 = 1
    initialmemory::Vector{UInt8} = UInt8[]
    initialcpu::CpuSnapshot = zero(CpuSnapshot) # memory values don't matter here
    curtrail::TrailEntry = TrailEntry()
    trails::Vector{TrailEntry} = [curtrail]
    # scratch vars used during memory writes
    writenext::Int64 = 1
    a1::UInt16 = 0x0000
    v1::UInt8 = 0x00
    a2::UInt16 = 0x0000
    v2::UInt8 = 0x00
    a3::UInt16 = 0x0000
    v3::UInt8 = 0x00
    bypass::Bool = false
end

struct MemWrite
    addr::UInt16
    value::UInt8
    numwrites::Int
    lastwrite::Int
end

struct UndoSnapshot
    u1::MemWrite # affected memory address and up to 3 values (BRK pushes 3)
    u2::MemWrite
    u3::MemWrite
end

struct UndoTrailEntry
    undotrail::Vector{UndoSnapshot}
    UndoTrailEntry() = new(zeros(UndoSnapshot, SNAPLEN))
end

@kwdef mutable struct RewindSession
    maxtime::Int64 = 0
    curtime::Int64 = 0
    nextfree::Int64 = 1
    curundo::UndoTrailEntry = UndoTrailEntry()
    undos::Vector{UndoTrailEntry} = [curundo]
    memory::Vector{UInt8} = zeros(UInt8, 64K)
    writes::Vector{UInt} = zeros(UInt, 64K)
    lastwrites::Vector{Int} = zeros(Int, 64K) # the last time slice that wrote to each location
end

const mem0 = MemWrite(0x0000, 0x00, 0, 0)

Base.zero(::Type{UndoSnapshot}) = UndoSnapshot(mem0, mem0, mem0)

function Base.show(io::IO, snap::CpuSnapshot)
    local sets = NUMSETS[snap.opcode+1]
    local str = [
        "CpuSnapshot($(opsyms[snap.opcode + 1])",
        "PC: $(rhex(snap.pc))",
        "A: $(rhex(snap.a))",
        "X: $(rhex(snap.x))",
        "Y: $(rhex(snap.y))",
        "SP: $(rhex(snap.sp))",
        "P: $(status(snap.status))",
    ]

    sets > 0 && push!(str, "$(rhex(snap.addr)): $(dbyte(snap.value))")
    sets > 1 && push!(str, "$(dbyte(snap.value2))")
    sets > 2 && push!(str, "$(dbyte(snap.value3))")
    push!(str, pop!(str) * ")")
    print(io, join(str, ", "))
end

function init_undo_session(rewinder::Rewinder, session::RewindSession)
    session.memory = copy(rewinder.initialmemory)
end

const BLANKSNAP = CpuSnapshot(; opcode = 0xAA) # 0xAA is TAX, which does not affect memory

function update_undo_session(rewinder::Rewinder, session::RewindSession)
    rewinder.curtime == session.curtime && return
    local m1, m2, m3 = mem0, mem0, mem0
    local mem = session.memory
    local writes = session.writes
    local lastwrites = session.lastwrites
    local function undofor(time, snap, offset)
        local v = mem[snap.addr+offset]
        local w = writes[snap.addr+offset]
        local l = lastwrites[snap.addr+offset]
        mem[snap.addr+offset] = snap.value
        writes[snap.addr+offset] += 1
        lastwrites[snap.addr+offset] = time
        return MemWrite(snap.addr + offset - 1, v, w, l)
    end

    # reproduce memory snapshot at session.curtime
    for i = session.maxtime+1:rewinder.curtime
        #local snap = i == 1 ? BLANKSNAP : rewinder[i - 1]
        local snap = rewinder[i]
        local sets = NUMSETS[snap.opcode+1]
        # compute undo record
        if sets > 0
            m1 = undofor(i - 1, snap, 1)
            if sets > 1
                m2 = undofor(i - 1, snap, 2)
                if sets > 2
                    m3 = undofor(i - 1, snap, 3)
                end
            end
        end
        # append undo record
        if session.nextfree > SNAPLEN
            local ent = UndoTrailEntry()
            push!(session.undos, ent)
            session.curundo = ent
            session.nextfree = 1
        end
        local undo = UndoSnapshot(m1, m2, m3)
        session.curundo.undotrail[session.nextfree] = undo
        sets > 0 && println("Update ", diag(snap, undo))
        session.nextfree += 1
    end
    if session.curtime == session.maxtime
        session.curtime = rewinder.curtime
    end
    session.maxtime = rewinder.curtime
    println(
        "maxtime $(session.maxtime), curtime $(session.curtime), nextfree $(session.nextfree)",
    )
end

function diag(snap::CpuSnapshot, undo::UndoSnapshot, forward = true)
    local writes = NUMSETS[snap.opcode+1]
    local msg = "$(opsyms[snap.opcode + 1])"
    function values(sv, u)
        return forward ? "$(dbyte(u.value)) -> $(dbyte(sv))" :
               "$(dbyte(u.value)) <- $(dbyte(sv))"
    end

    if writes > 0
        msg *= " $(rhex(snap.addr)): $(values(snap.value, undo.u1))"
        if writes > 1
            msg *= ", $(values(snap.value2, undo.u2))"
            if writes > 2
                msg *= ", $(values(snap.value3, undo.u3))"
            end
        end
    end
    return msg
end

function back(cpu::Cpu, rewinder::Rewinder, session::RewindSession)
    session.curtime == 1 && return
    #local snap = rewinder[session.curtime - 1]
    local snap = rewinder[session.curtime]
    local undorec = session[session.curtime]
    local writes = NUMSETS[snap.opcode+1]
    local function undo(u)
        write6502(cpu, u.addr, u.value)
        session.writes[u.addr+1] = u.numwrites
        session.lastwrites[u.addr+1] = u.lastwrite
    end

    restore(cpu, snap)
    rewinder.bypass = true
    if writes > 0
        println("undo ", diag(snap, undorec, false))
        undo(undorec.u1)
        if writes > 1
            undo(undorec.u2)
            if writes > 2
                undo(undorec.u3)
            end
        end
    end
    rewinder.bypass = false
    session.curtime -= 1
end

function forward(cpu::Cpu, rewinder::Rewinder, session::RewindSession)
    session.curtime == rewinder.curtime && return
    session.curtime += 1
    local snap = rewinder[session.curtime]
    local writes = NUMSETS[snap.opcode+1]
    restore(cpu, snap)
    rewinder.bypass = true
    if writes > 0
        local undo = session[session.curtime]
        println("redo ", diag(snap, undo))
        snap.addr != undo.u1.addr && error(
            "Snapshot address $(rhex(snap.addr)) != undo address $(rhex(undo.u1.addr))",
        )
        cpu.memory[snap.addr+1] != undo.u1.value && error(
            "memory value $(rhex(cpu.memory[snap.addr + 1])) != undo value $(rhex(undo.u1.value))",
        )
        write6502(cpu, snap.addr, snap.value)
        if writes > 1
            cpu.memory[snap.addr+2] != undo.u2.value && error(
                "memory value $(rhex(cpu.memory[snap.addr + 2])) != undo value $(rhex(undo.u2.value))",
            )
            write6502(cpu, snap.addr + 1, snap.value2)
            if writes > 2
                cpu.memory[snap.addr+2] != undo.u2.value && error(
                    "memory value $(rhex(cpu.memory[snap.addr + 3])) != undo value $(rhex(undo.u3.value))",
                )
                write6502(cpu, snap.addr + 2, snap.value3)
            end
        end
    end
    rewinder.bypass = false
end

CpuSnapshot(
    cpu::Cpu,
    temps::Temps,
    opcode::UInt8,
    addr::UInt16,
    value::UInt8,
    value2::UInt8,
    value3::UInt8,
) = CpuSnapshot(
    opcode,
    pc(cpu, temps),
    cpu.a,
    cpu.x,
    cpu.y,
    cpu.sp,
    cpu.status,
    cpu.instructions,
    cpu.clockticks6502,
    addr,
    value,
    value2,
    value3,
)

Base.zero(::Type{CpuSnapshot}) =
    CpuSnapshot(0x00, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0, 0, 0x0000, 0x00, 0x00, 0x00)
function restore(cpu::Cpu, snap::CpuSnapshot)
    cpu.a = snap.a
    cpu.x = snap.x
    cpu.y = snap.y
    cpu.sp = snap.sp
    cpu.status = snap.status
    cpu.instructions = snap.instructions
    cpu.clockticks6502 = snap.clockticks6502
    return Temps(; snap.pc)
end

Base.getindex(r::Rewinder, idx::Int64) =
    r.trails[(idx-1)÷SNAPLEN+1].statetrail[(idx-1)%SNAPLEN+1]
Base.getindex(r::RewindSession, idx::Int64) =
    r.undos[(idx-1)÷SNAPLEN+1].undotrail[(idx-1)%SNAPLEN+1]

#memvalue(t::Rewinder, addr::UInt16, value::UInt8) =
#    MemValue(value, t.memorysets[addr + 1], t.memorysetcounts[addr + 1])

"""
    time travel from current state at fromtime to state at totime
"""
function travel(t::Rewinder, cpu::Cpu, fromtime::Int64, totime::Int64)
    local memory = cpu.memory
    for mem = fromtime:sign(fromtime - totime):totime
        local ent = t[mem]
        ent.prevset == 0 && continue
        memory[ent.addr+1] = ent.value
    end
    return restore(cpu, t[totime])
end

##
## HOOKS
##
"""
    call the write6502 and inner_step6502 hooks from the main methods to use the rewinder
"""
function write6502(trails::Rewinder, cpu, addr::UInt16, value::UInt8)
    if !trails.bypass
        mprintln(cpu, "RECORD WRITE")
        if trails.writenext == 1
            trails.a1 = addr
            trails.v1 = value
        elseif trails.writenext == 2
            trails.a2 = addr
            trails.v2 = value
        elseif trails.writenext == 3
            trails.a3 = addr
            trails.v3 = value
        end
        trails.writenext += 1
    end
    cpu.memory[addr+1] = value
end

sort2(a1, v1, a2, v2) = a1 < a2 ? (v1, v2) : (v2, v1)

function sort3(a1, v1, a2, v2, a3, v3)
    amin = min(a1, a2, a3)
    return amin == a1 ? (v1, sort2(a2, v2, a3, v3)...) :
        amin == a2 ? (v2, sort2(a1, v1, a3, v3)...) :
        (v3, sort2(a1, v1, a2, v2)...)
end

"""
    call the write6502 and inner_step6502 hooks from the main methods to use the rewinder
"""
function inner_step6502(trails::Rewinder, cpu, temps::Temps)
    local opcode = cpu.memory[pc(cpu, temps)+1]
    local expectedsets = NUMSETS[opcode+1]
    local addr, v1, v2, v3
    trails.writenext = 1
    temps = base_inner_step6502(cpu, temps)
    trails.bypass && return temps
    if trails.writenext - 1 != expectedsets
        error(
            "Error in expected sets for opcode $(rhex(opcode)): expecting $expectedsets sets but got $(trails.writenext - 1)",
        )
    end
    if trails.writenext == 1
        addr = 0x0000
        v1 = v2 = v3 = 0x00
    elseif trails.writenext == 2
        addr = trails.a1
        v1 = trails.v1
        v2 = v3 = 0x00
    elseif trails.writenext == 3
        addr = min(trails.a1, trails.a2)
        v1, v2 = sort2(trails.a1, trails.v1, trails.a2, trails.v2)
        v3 = 0x00
    else
        addr = min(trails.a1, trails.a2, trails.a3)
        v1, v2, v3 = sort3(trails.a1, trails.v1, trails.a2, trails.v2, trails.a3, trails.v3)
    end
    if trails.nextfree > SNAPLEN
        local ent = TrailEntry()
        push!(trails.trails, ent)
        trails.curtrail = ent
        trails.nextfree = 1
    end
    trails.curtime += 1
    local snap = CpuSnapshot(cpu, temps, opcode, addr, v1, v2, v3)
    trails.writenext > 1 && mprintln(cpu, snap)
    trails.curtrail.statetrail[trails.nextfree] = snap
    trails.nextfree += 1
    temps
end

#"Shift time from from-time to to-time"
#function shift(rewinder::Rewinder, cpu::Cpu, from, to)
#    rewinder.modified .= false
#end

function init(rewinder::Rewinder, cpu::Cpu, temps::Temps)
    rewinder.initialcpu = CpuSnapshot(cpu, temps, 0x00, 0x0000, 0x00, 0x00, 0x00)
    rewinder.initialmemory = copy(cpu.memory)
    rewinder.curtime = 0
    rewinder.nextfree = 1
    rewinder.writenext = 1
end

end # module rewinder
