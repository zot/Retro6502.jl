""" A 6502 worker node
A worker can manage several 6502 instances.

Workers provide remote assembly and Julia execution services.

Since assembly pollutes the namespace with generated modules,
it's nice to be able to scrap workers to prevent permanent garbage from piling up.
"""
module Workers

using Distributed, SharedArrays
using ..Fake6502: Fake6502, K, Machine, EDIR, hex, ROM
using ..C64: C64, @io, @printf, BASIC_ROM
using ..Asm: Asm, CodeContext, getvar
using ..Fake6502m: Fake6502m, Cpu, Temps, setpc
import ..Fake6502m:
    inner_step6502, base_inner_step6502, exec6502, reset6502, ticks, setticks

# this worker
private = nothing

@kwdef mutable struct Worker
    id::Int
    run_channel::RemoteChannel{Channel{Any}} = RemoteChannel() do
        Channel{Any}(10)
    end
    memory::SharedVector{UInt8} = SharedVector{UInt8}((64K,); pids = procs())
end

@kwdef mutable struct WorkerPrivate
    worker::Worker
    lock::ReentrantLock = ReentrantLock()
    cmds::Channel{Function} = Channel{Function}(10)
    ctx::Union{CodeContext,Nothing} = nothing
    cpu::Union{Cpu{Worker},Nothing} = nothing
    running::Bool = false
    temps::Temps = Temps()
end

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

function loadprg(filename, labelfile)
    labels = Dict()
    addrs = Dict()
    off, len = Fake6502.loadprg(filename, private.worker.memory, labels, addrs; labelfile)
    labels, addrs, off, len
end

function asmfile(filename::AbstractString)
    global private
    local ctx = private.ctx = Asm.asmfile(filename)

    private.worker.memory[ctx.min+1:ctx.max+1] .= ctx.memory[ctx.min+1:ctx.max+1]
    ctx.labels
end

"Asynchronously execute a function during emulator execution"
function async(func::Function)
    global private
    !private.running && error("Program is not running")
    put!(private.cmds, func)
end

"Synchronously execute a function during emulator execution"
function sync(func::Function)
    local result_chan = Channel{Any}()
    local thrown = nothing
    async() do
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
function worker_step(cpu::Cpu{Worker}, temps::Temps, instructions)
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

updatememory(w::Worker) =
    remotecall_wait(w.id) do
        sync() do
            global private
            private.worker.memory .= private.cpu.memory
        end
    end

function exec(w::Worker, runid::String, label::Symbol; tickcount = Base.max_values(Int))
    remotecall(w.id, label, tickcount) do label, tickcount
        global private
        local worker = private.worker
        lock(private.lock) do
            isnothing(private.ctx) && error("No program")
            private.running && error("Program is already running")
            private.cpu = Cpu(; memory = [worker.memory...], user_data = worker)
            private.running = true
            @spawn try
                local temps = reset6502(private.cpu, private.temps)
                local instructions = 0
                temps = setpc(private.cpu, Temps(), getvar(private.ctx, label))
                temps = setticks(private.cpu, temps, 0)
                while ticks(private.cpu, temps) < tickcount && private.cpu.sp != 0xFF
                    temps = worker_step(private.cpu, temps, instructions)
                    instructions += 1
                end
                private.cpu.instructions = instructions
                temps
            finally
                lock(private.lock) do
                    private.running = false
                end
                try
                    put!(
                        worker.run_channel,
                        (; runid, status = Fake6502m.state(private.cpu, private.temps)),
                    )
                catch err
                    @error "Error getting state" exception = (err, catch_backtrace())
                end
            end
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

function loadprg(w::Worker, mach::Machine, filename; labelfile = "")
    w.memory .= mach.newcpu.memory
    labels, addrs, off, total = fetch(@spawnat w.id loadprg(filename, labelfile))
    mach.newcpu.memory = mach.mem = w.memory
    merge!(mach.labels, labels)
    merge!(mach.addrs, addrs)
    off, total
end

end
