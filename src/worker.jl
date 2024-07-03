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

# extra data after memory
const OFFSET_DIRTY = 64K + 1
# basic:1, chars:1, kernal:1, vicbank:2
const OFFSET_BANKS = 64K + 2
# [char-mem:3][scr-mem:4]
const OFFSET_VIC_MEM = 64K + 3
const OFFSET_KEY = 64K + 4
const SHARED_END = 64K + 5

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
    state::C64_machine = C64_machine()
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

function init()
    global private = WorkerPrivate(; worker = Worker(; id = myid()))
    return private.worker
end

#function loadprg(filename, labelfile)
#    labels = Dict()
#    addrs = Dict()
#    off, len = Fake6502.loadprg(filename, private.worker.memory, labels, addrs; labelfile)
#    labels, addrs, off, len
#end

function asmfile(filename::AbstractString)
    global private
    local ctx = private.ctx = Asm.asmfile(filename)

    private.worker.memory[ctx.min+1:ctx.max+1] .= ctx.memory[ctx.min+1:ctx.max+1]
    #log("ASSEMBLED CONTEXT, SETTING WORKERS TO $Workers")
    try
        invokelatest(Asm.eval(ctx, :(function(workers) global Workers = workers end)), Workers)
    catch err
        log(err)
    end
    local listing = sprint(io-> Asm.listings(io, ctx))
    ctx.labels, listing
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

# instructions is only used if there is a command waiting
function worker_step(cpu::Cpu{WorkerPrivate}, temps::Temps, instructions)
    global private

    println("STEP")
    while isready(private.cmds)
        try
            private.temps = temps
            private.cpu.instructions = instructions
            take!(private.cmds)()
        catch err
            @error "Error in worker command: $err" exception = (err, catch_backtrace())
        end
    end
    return base_inner_step6502(cpu, temps)
end

# EXTERNAL API

asmfile(w::Worker, filename) = remotecall_fetch(asmfile, w.id, filename)

clear(w::Worker) =
    remotecall_wait(w.id) do
        global private
        private.ctx = nothing
        private.cpu = nothing
    end

function updatesettings(w::Worker, bs::BankSettings)
    updatesettings(bs, w.memory[OFFSET_BANKS], w.memory[OFFSET_VIC_MEM])
end

function updatememory(w::Worker, bs::BankSettings)
    remotecall_wait(w.id) do
        sync(; onlyifrunning = true) do
            privateupdate()
        end
    end
    updatesettings(w, bs)
end

function privateupdate()
    global private
    local state::C64_machine = c64(private)
    private.worker.memory .= private.cpu.memory
    for bank in [c64(state).banks..., char_defs]
        (@view private.worker.memory[intrange(bank)]) .= @view ROM[intrange(bank)]
    end
    private.worker.memory[OFFSET_DIRTY] =
        any(state.dirty_characters) || any(state.dirty_character_defs)
    local banks = 0x00
    for (flag, bank) in
        (BANKS_BASIC => BASIC_ROM, BANKS_CHARS => CHAR_ROM, BANKS_KERNAL => KERNAL_ROM)
        if bank ∈ state.banks
            banks |= flag
        end
    end
    private.worker.memory[OFFSET_BANKS] = banks | (private.cpu.memory[VIC_BANK] & 0x03)
    private.worker.memory[OFFSET_VIC_MEM] = state.vic_bank_summary
end

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
    bs::Union{Nothing,BankSettings} = nothing,
)
    #log("EXEC 1")
    local result = remotecall(w.id, label, tickcount) do label, tickcount
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
                log("FAKE $label[$i] => $routine")
            end
            #log("EXEC 4")
            private.temps = reset6502(private.cpu, private.temps)
            #log("RUNNING $(getvar(private.ctx, label))")
            private.temps = setpc(private.cpu, Temps(), getvar(private.ctx, label))
            private.temps = setticks(private.cpu, private.temps, 0)
            return cont(tickcount)
        catch err
            @error err exception=(err,catch_backtrace())
            log(err)
            return Fake6502m.state(private.cpu, private.temps)
        end
    end
    isnothing(bs) && return
    local state = fetch(result)
    updatesettings(w, bs)
    return state
end

function cont(
    w::Worker;
    tickcount = Base.max_values(Int),
    bs::Union{Nothing,BankSettings} = nothing,
)
    local result = remotecall(cont, w.id, tickcount)
    isnothing(bs) && return
    local state = fetch(result)
    updatesettings(w, bs)
    return state
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
        try
            local temps = private.temps
            local instructions = 0
            while ticks(private.cpu, temps) < tickcount && private.cpu.sp != 0xFF
                #log("STEP $(Fake6502m.state(private.cpu, temps))")
                temps = worker_step(private.cpu, temps, instructions)
                instructions += 1
            end
            # done running -- copy memory over
            privateupdate()
            private.cpu.instructions = instructions
            return Fake6502m.state(private.cpu, private.temps)
        catch err
            log(err)
        finally
            private.running = false
        end
    end
end

state(w::Worker) =
    remotecall_fetch(w.id) do
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
    log("SCNKEY")
    return temps
end

function GETIN(cpu::Cpu, temps::Temps)
    log("GETIN")
    return temps
end

end
