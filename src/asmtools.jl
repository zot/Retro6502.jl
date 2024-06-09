"""
Tools automatically imported by jas files
"""
module AsmTools

using Printf: @printf, @sprintf
using ..Fake6502: hex, rhex, asmerr, matches, ascii2screen

export align, data, word, words, @rel_str, @label_str, @printf, @sprintf

context = nothing

macro label_str(str)
    global context
    context.pass < 3 && return 0x0000
    str == "*" &&
        return :($AsmTools.context.offset)
    matches(r"^[-+]+$", str) &&
        return rel(str)
    :($(esc(Symbol(str))))
end

macro rel_str(str)
    rel(str)
end

function rel(str)
    global context
    context.pass < 3 && return 0x0000
    local delta = sum((c-> c == '-' ? -1 : c == '+' ? 1 : 0).(collect(str)))
    local forward = delta > 0
    delta -= sign(delta)
    quote
        local ctx = $AsmTools.context
        if ctx.stability == 1
            ctx.offset
        else
            local off = ctx.offset
            local pos = $delta + $(forward ?
                :(searchsortedfirst(context.relative_labels, off + 1)) :
                :(searchsortedlast(context.relative_labels, off - 1)))
            println("OFFSET: " * hex(off) * " LABELS: " * join(hex.(ctx.relative_labels), " ") * " POS: " * string(pos) * ", DELTA: " * $(string(delta)))
            if pos < 1 || pos > length(ctx.relative_labels)
                asmerr(ctx, "No matching relative label " * $(forward ? "after" : "before") *
                    " 0x" * string(off))
            end
            ctx.relative_labels[pos]
        end
    end
end

function withcontext(func, ctx)
    global context
    local old = context
    context = ctx
    local result = func()
    context = old
    result
end

"""
Align the current context's output to a byte width
"""
function align(amount::Int)
    global context
    context.offset += amount - context.offset % amount
end

# like data but screen-encode it
function screen(args...)
    local bytes = data(args...)
    for (i, b) in enumerate(bytes)
        bytes[i] = ascii2screen(b)
    end
end

word(value) = UInt16(value & 0xFFFF)

words(args...) = UInt16[word(v) for v in args]

function data(tup::Tuple)
    local len = 0
    for t in tup
        len += valuesize(t)
    end
    local bytes = zeros(UInt8, len)
    local offset = 1
    for t in tup
        println("STORE: $t at $offset in $bytes")
        local stored = store(bytes, offset, t)
        offset += length(stored)
    end
    bytes
end

data(bytes::Vector{UInt8}) = bytes

data(str::AbstractString) = Vector{UInt8}(str)

function data(a::A) where {T,A<:Vector{T}}
    global context
    !isbitstype(T) && asmerr(context, "Cannot convert $T to bytes: $a")
    if htol(1) == 1
        @inbounds reinterpret(UInt8, a)
    else
        @inbounds reinterpret(UInt8, htol.(a))
    end
end

function data(value::T) where {T}
    global context
    !isbitstype(T) && asmerr(context, "Cannot convert $T to bytes: $value")
    @inbounds reinterpret(UInt8, [htol(value)])
end

valuesize(value::AbstractString) = length(value)
valuesize(value::AbstractVector{T}) where {T} = sizeof(T) * length(value)
valuesize(value) = sizeof(value)

function store(bytes::AbstractVector{UInt8}, offset, value::AbstractVector{UInt8})
    #@view(bytes[offset:offset+length(value) - 1]) .= value
    copyto!(bytes, offset, value, 1, length(value))
    value
end

store(bytes::AbstractVector{UInt8}, offset, value::UInt8) =
    store(bytes, offset, UInt8[value])

store(bytes::AbstractVector{UInt8}, offset, value::UInt16) =
    store(bytes, offset, UInt8[(value >> 8) & 0xFF, value & 0xFF])

store(bytes::AbstractVector{UInt8}, offset, value::Int8) =
    store(bytes, offset, value & 0xFF)

store(bytes::AbstractVector{UInt8}, offset, value::AbstractString) =
    store(bytes, offset, Vector{UInt8}(value))

store(bytes::AbstractVector{UInt8}, offset, value::AbstractVector{T}) where {T} =
    store(bytes, offset, data(value))

function store(bytes::AbstractVector{UInt8}, offset, value::T) where {T}
    !isbitstype(T) && asmerr(context, "Cannot convert $T to bytes: $bytes")
    store(bytes, offset, @inbounds reinterpret(UInt8, [htol(value)]))
end

end
