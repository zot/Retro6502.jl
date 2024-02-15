"""
Tools automatically imported by jas files
"""
module AsmTools

export align, data

context = nothing

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

function data(tup::Tuple)
    local len = 0
    for t in tup
        len += sizeof(t)
    end
    local bytes = zeros(UInt8, len)
    local offset = 1
    for t in tup
        store(bytes, offset, t)
        !isbits(t) &&
            error("Cannot convert data to bytes")
        len += sizeof(t)
    end
    bytes
end

data(bytes::Vector{UInt8}) = bytes

data(str::AbstractString) = Vector{UInt8}(str)

function data(a::A) where {T, A <: Vector{T}}
    !isbitstype(T) &&
        error("Cannot convert $T to bytes")
    if htol(1) == 1
        @inbounds reinterpret(UInt8, a)
    else
        @inbounds reinterpret(UInt8, htol.(a))
    end
end

function data(value::T) where {T}
    !isbitstype(T) &&
        error("Cannot convert $T to bytes")
    @inbounds reinterpret(UInt8, [htol(value)])
end

store(bytes::AbstractVector{UInt8}, offset, value::UInt8) =
    bytes[offset] = value

store(bytes::AbstractVector{UInt8}, offset, value::Int8) =
    bytes[offset] = reinterpret(UInt8, value)

store(bytes::AbstractVector{UInt8}, offset, value::AbstractVector{UInt8}) =
    @view(bytes[offset:end]) .= value

store(bytes::AbstractVector{UInt8}, offset, value::AbstractString) =
    store(bytes, offset, Vector{UInt8}(value))

store(bytes::AbstractVector{UInt8}, offset, value::AbstractVector{T}) where {T} =
    store(bytes, offset, data(value))

function store(bytes::AbstractVector{UInt8}, offset, value::T) where T
    !isbitstype(T) &&
        error("Cannot convert $T to bytes")
    store(bytes, offset, @inbounds reinterpret(UInt8, [htol(value)]))
end

end
