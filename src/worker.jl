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
using ..Asm: Asm, CodeContext

# this worker
worker = nothing
ctx = Ref{Union{CodeContext,Nothing}}(nothing)

@kwdef mutable struct Worker
    id::Int
    memory::SharedVector{UInt8}
end

const workers = Dict{Int,Worker}()

function add_worker()
    old = procs()
    id = only(setdiff(addprocs(1), old))
    eval(:(@everywhere using Fake6502))
    fetch(@spawnat id Fake6502.Workers.init())
end

function init()
    global worker = Worker(myid(), SharedVector{UInt8}((64K,); pids=procs()))
end

function loadprg(filename, labelfile)
    labels = Dict()
    addrs = Dict()
    off, len = Fake6502.loadprg(filename, worker.memory, labels, addrs; labelfile)
    labels, addrs, off, len
end

function asmfile(filename)
    global ctx = Asm.asmfile(filename)
    worker.memory[ctx.min:ctx.max] = ctx.memory[ctx.min:ctx.max]
    nothing
end

# EXTERNAL API

function asmfile(w::Worker, filename)
    fetch(@spawnat w.id asmfile(filename))
end

function loadprg(w::Worker, mach::Machine, filename; labelfile = "")
    w.memory .= mach.newcpu.memory
    labels, addrs, off, total = fetch(@spawnat w.id loadprg(filename, labelfile))
    mach.newcpu.memory = mach.mem = w.memory
    merge!(mach.labels, labels)
    merge!(mach.addrs, addrs)
    off, total
end

# TESTING

load_condensed(w::Worker) = (mach::Machine)-> begin
    off, total = loadprg(w, mach, "$EDIR/condensed.prg"; labelfile = "$EDIR/condensed.labels")
    @io println(
        "Loaded ",
        total,
        " bytes at 0x",
        string(off; base = 16, pad = 4),
        ", ",
        length(mach.labels),
        " labels",
    )
    @io print("labels:")
    for name in sort([keys(mach.labels)...])
        @printf "\n  %04x %s" mach.labels[name].value - 1 name
    end
    @io println()
    @io println("ROM MEM: ", hex(ROM[BASIC_ROM.first.value]))
end

function test_worker()
    w = add_worker()
    C64.test_c64(load_condensed(w))
end

end
