""" A 6502 worker node
A worker can manage several 6502 instances.

Workers provide remote assembly and Julia execution services.

Since assembly pollutes the namespace with generated modules,
it's nice to be able to scrap workers to prevent permanent garbage from piling up.
"""
module Workers

using Distributed, SharedArrays
using ..Fake6502:
    Fake6502,
    K,
    Machine,
    EDIR,
    hex,
    ROM,
    intrange,
    A,
    char_defs,
    log,
    AddrRange,
    VIC_BANK,
    BANKS_BASIC,
    BANKS_CHARS,
    BANKS_KERNAL,
    BankSettings
using ..C64:
    C64,
    C64_machine,
    @io,
    @printf,
    BASIC_ROM,
    c64,
    initstate,
    BASIC_ROM,
    CHAR_ROM,
    KERNAL_ROM
using ..Asm: Asm, CodeContext, getvar
using ..Fake6502m: Fake6502m, Cpu, Temps, setpc
using StructIO: StructIO

import ..Fake6502: initprg, updatesettings
import ..Fake6502m:
    inner_step6502,
    base_inner_step6502,
    exec6502,
    reset6502,
    ticks,
    setticks,
    read6502,
    write6502,
    jsr,
    jmp

StructIO.@io struct CpuState
    pc::UInt16
    a::UInt8
    x::UInt8
    y::UInt8
    sp::UInt8
    status::UInt8
    instructions::Int64
    clockticks6502::Int64
    slack::Float64
    running::Bool
    banks::UInt8
    vicmem::UInt8
    update::Int
end align_packed

# extra data after memory
const OFFSET_DIRTY = 64K + 1
const OFFSET_COUNTL = 64K + 2
const OFFSET_COUNTH = 64K + 3
const OFFSET_KEY = 64K + 4
const OFFSET_CHR = 64K + 5
const OFFSET_STATE = 64K + 6
const SHARED_END = 64K + 6 + sizeof(CpuState)
const UPDATE_PERIOD = 10000

# this worker
private = nothing

@kwdef mutable struct Worker
    id::Int
    run_channel::RemoteChannel{Channel{Any}} = RemoteChannel() do
        Channel{Any}(10)
    end
    memory::SharedVector{UInt8} = SharedVector{UInt8}((SHARED_END - 1,); pids = procs())
end

@kwdef mutable struct WorkerPrivate
    worker::Worker
    lock::ReentrantLock = ReentrantLock()
    cmds::Channel{Function} = Channel{Function}(10)
    ctx::Union{CodeContext,Nothing} = nothing
    cpu::Union{Cpu{WorkerPrivate},Nothing} = nothing
    running::Bool = false
    temps::Temps = Temps()
    slack::Float64 = 0
    state::C64_machine = C64_machine()
    updatecount::Int = 0
end

Base.show(io::IO, ::WorkerPrivate) = print(io, "WorkerPrivate")

C64.c64(worker::WorkerPrivate) = worker.state

const workers = Dict{Int,Worker}()

function add_worker()
    old = procs()
    id = only(setdiff(addprocs(1), old))
    eval(:(@everywhere using Fake6502))
    fetch(@spawnat id Fake6502.Workers.init())
end

function remove_worker(w::Worker)
    w.id == -1 &&
        return
    rmprocs(w.id)
    w.id = -1
end

function init()
    global private = WorkerPrivate(; worker = Worker(; id = myid()))
    #log("PRIVATE WORKER DIRTY CHARACTERS: $(private.state.dirty_characters)")
    return private.worker
end

#function loadprg(filename, labelfile)
#    labels = Dict()
#    addrs = Dict()
#    off, len = Fake6502.loadprg(filename, private.worker.memory, labels, addrs; labelfile)
#    labels, addrs, off, len
#end

asmfile(filename::AbstractString) = invokelatest(innerasmfile, filename)

function innerasmfile(filename::AbstractString)
    try
        global private
        local ctx = private.ctx = Asm.asmfile(filename)

        private.worker.memory[ctx.min+1:ctx.max+1] .= ctx.memory[ctx.min+1:ctx.max+1]
        #log("ASSEMBLED CONTEXT, SETTING WORKERS TO $Workers")
        #try
        #    invokelatest(Asm.eval(ctx, :(function(workers) global Workers = workers end)), Workers)
        #catch err
        #    log(err)
        #end
        local listing = sprint(io-> Asm.listings(io, ctx))
        ctx.labels, listing
    catch err
        log(err)
        rethrow()
    end
end

"Asynchronously execute a function during emulator execution"
function async(func::Function; onlyifrunning = false)
    global private
    if private.running
        put!(private.cmds, func)
    elseif !onlyifrunning
        error("Program is not running")
    end
end

"Synchronously execute a function during emulator execution"
function sync(func::Function; onlyifrunning = false)
    local result_chan = Channel{Any}()
    local thrown = nothing
    async(; onlyifrunning) do
        try
            put!(result_chan, func())
        catch err
            thrown = err
            put!(result_chan, nothing)
        end
    end
    local res = take!(result_chan)
    !isnothing(thrown) && throw(thrown)
    return res
end

# EXTERNAL API
stop(w::Worker) = remotecall(w.id) do
    global private

    if private.running
        log("STOPPING")
    else
        log("NOT RUNNING")
    end
    private.running = false
end

asmfile(w::Worker, filename) = remotecall_fetch(asmfile, w.id, filename)

clear(w::Worker) =
    remotecall_wait(w.id) do
        global private
        private.ctx = nothing
        private.cpu = nothing
    end

function updatesettings(bs::BankSettings, cpustate::CpuState)
    updatesettings(bs, cpustate.banks, cpustate.vicmem)
end

function privateupdate()
    global private
    local c64state::C64_machine = c64(private)
    local workermem = private.worker.memory
    local cpumem = private.cpu.memory

    if cpumem[OFFSET_COUNTL] == 0xFF
        cpumem[OFFSET_COUNTL] = 0x00
        if cpumem[OFFSET_COUNTH] == 0xFF
            cpumem[OFFSET_COUNTH] = 0x00
        else
            cpumem[OFFSET_COUNTH] += 1
        end
    else
        cpumem[OFFSET_COUNTL] += 1
    end
    private.updatecount += 1
    workermem .= cpumem
    for bank in c64(c64state).banks
        (@view workermem[intrange(bank)]) .= @view ROM[intrange(bank)]
    end
    local chars = @view workermem[c64state.character_mem:A(c64state.character_mem.value - 1 + 0x07FF)]
    #log("worker 30 CHAR BYTES: $(hex(chars[1:30]))")
    #log("CHAR MEM: $(c64state.character_mem:A(c64state.character_mem.value - 1 + 0x07FF))")
    if any(c64state.dirty_characters) || any(c64state.dirty_character_defs) || c64state.all_dirty
        workermem[OFFSET_DIRTY] = 1
        c64state.dirty_characters .= false
        c64state.dirty_character_defs .= false
        c64state.all_dirty = false
    end
    local banks = 0x00
    for (flag, bank) in
        (BANKS_BASIC => BASIC_ROM, BANKS_CHARS => CHAR_ROM, BANKS_KERNAL => KERNAL_ROM)
        if bank ∈ c64state.banks
            banks |= flag
        end
    end
    local cpustate = CpuState(Fake6502m.state(private.cpu, private.temps)...,
                              private.slack,
                              private.running,
                              banks | (private.cpu.memory[VIC_BANK] & 0x03),
                              c64state.vic_bank_summary,
                              private.updatecount,
                              )
    StructIO.pack(IOBuffer(@view workermem[OFFSET_STATE:SHARED_END-1]; write=true), cpustate)
end

cpustate(w::Worker) = StructIO.unpack(IOBuffer(@view w.memory[OFFSET_STATE:SHARED_END-1]), CpuState)

function read6502(cpu::Cpu{WorkerPrivate}, addr::UInt16)
    #log("READ $addr, $(c64(cpu))")
    read6502(c64(cpu), cpu, addr)
end

function write6502(cpu::Cpu{WorkerPrivate}, addr::UInt16, byte::UInt8)
    #log("WRITE $addr")
    write6502(c64(cpu), cpu, addr, byte)
end

function jsr(cpu::Cpu{WorkerPrivate}, temps)
    #log("WORKER JSR $(Fake6502m.state(cpu, temps))")
    jsr(c64(cpu), cpu, temps)
end

function jmp(cpu::Cpu{WorkerPrivate}, temps)
    #log("WORKER JMP $(Fake6502m.state(cpu, temps))")
    jmp(c64(cpu), cpu, temps)
end

initprg(memrange::AddrRange, mach::Machine, worker::WorkerPrivate) =
    initprg(memrange, mach, worker.state)

function exec(
    w::Worker,
    label::Symbol;
    tickcount = Base.max_values(Int),
)
    #log("EXEC 1")
    remote_do(w.id, label, tickcount) do label, tickcount
        invokelatest(innerexec, label, tickcount)
    end
end

function innerexec(label, tickcount)
    global private

    private.running &&
        return :running
    try
        local worker = private.worker

        #log("EXEC 2")
        private.cpu = Cpu(; memory = [worker.memory...], user_data = private)
        initstate(private.state, private.cpu; clearscreen = false)
        local memrange = A(private.ctx.min:private.ctx.max)
        #log("EXEC 3")
        private.cpu.memory[memrange] .= worker.memory[memrange]
        initprg(memrange, private.cpu, c64(private))
        for (label, (i, routine)) in private.ctx.fakes
            c64(private).fake_routines[A(i)] = routine
            #log("FAKE $label[$i] => $routine")
        end
        #log("EXEC 4")
        private.temps = reset6502(private.cpu, private.temps)
        #log("RUNNING $(getvar(private.ctx, label))")
        private.temps = setpc(private.cpu, Temps(), getvar(private.ctx, label))
        private.temps = setticks(private.cpu, private.temps, 0)
        privateupdate()
        @spawn cont(tickcount)
    catch err
        @error err exception=(err,catch_backtrace())
        log(err)
        privateupdate()
    end
end

function cont(
    w::Worker;
    tickcount = Base.max_values(Int),
)
    remote_do(cont, w.id, tickcount)
end

function cont(tickcount = Base.max_values(Int))
    #log("CONT")
    invokelatest(innercont, tickcount)
end

function innercont(tickcount)
    global private

    #log("INNERCONT 1")
    return lock(private.lock) do
        #log("INNERCONT 2")
        isnothing(private.ctx) && error("No program")
        private.running && error("Program is already running")
        private.running = true
        local instructions = 0
        try
            local temps = private.temps
            local nextupdate = UPDATE_PERIOD
            local updatetime = UPDATE_PERIOD / 1000000
            local checkpoint = time()
            #local nextlog = 5000
            while private.running && ticks(private.cpu, temps) < tickcount && private.cpu.sp != 0xFF
                temps = base_inner_step6502(private.cpu, temps)
                instructions += 1
                local tick = ticks(private.cpu, temps)
                if tick > nextupdate
                    nextupdate += UPDATE_PERIOD
                    #if tick > nextlog
                    #    while tick > nextlog
                    #        nextlog += 5000
                    #    end
                    #    log("TICK COUNT $tick SLACK $(private.slack)")
                    #end
                    #log("STEP $(Fake6502m.state(private.cpu, temps))")
                    private.temps = temps
                    while isready(private.cmds)
                        try
                            private.cpu.instructions = instructions
                            take!(private.cmds)()
                        catch err
                            log("Error in worker command")
                            log(err)
                            @error "Error in worker command: $err" exception = (err, catch_backtrace())
                        end
                    end
                    privateupdate()
                    local seconds = time() - checkpoint
                    if seconds < updatetime
                        # throttle execution
                        private.slack += updatetime - seconds
                        sleep(updatetime - seconds)
                    else
                        yield()
                    end
                    checkpoint = time()
                end
            end
        catch err
            log(err)
        finally
            # done running -- copy memory over
            log("FINISHED EXECUTING")
            private.cpu.instructions = instructions
            private.running = false
            privateupdate()
        end
    end
end

state(w::Worker) = remotecall_fetch(w.id) do
    global private
    sync() do
        Fake6502m.state(private.cpu, private.temps)
    end
end

#function loadprg(w::Worker, mach::Machine, filename; labelfile = "")
#    w.memory .= mach.newcpu.memory
#    labels, addrs, off, total = fetch(@spawnat w.id loadprg(filename, labelfile))
#    mach.newcpu.memory = mach.mem = w.memory
#    merge!(mach.labels, labels)
#    merge!(mach.addrs, addrs)
#    initprg(off:off+total-1, mach, mach.newcpu.user_data)
#    off, total
#end

function SCNKEY(cpu::Cpu, temps::Temps)
    return temps
end

prevgetin = 0x00

function GETIN(cpu::Cpu, temps::Temps)
    global private, prevgetin
    local chr = private.worker.memory[OFFSET_CHR]

    chr != 0 && chr != prevgetin && log("GETIN $(repr(chr))")
    prevgetin = chr
    cpu.a = chr
    private.worker.memory[OFFSET_CHR] = 0x00
    return temps
end

end
