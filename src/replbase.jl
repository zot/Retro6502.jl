function color(rgb::UInt32...)
    extract(rgb) = rgb >> 16, (rgb >> 8) & 0xFF, rgb & 0xFF
    color(round.(Ref(UInt8), Base.Iterators.flatten(extract.(rgb)))...)
end

color(args...) = color(round.(Ref(UInt8), args)...)

function color(r::UInt8, g::UInt8, b::UInt8, args...)
    isempty(args) && return (; r, g, b)
    local all =
        vcat([r g b], ([r1 g1 b1] for (r1, g1, b1) in Iterators.partition(args, 3))...)
    avg(items) = sum(items) / length(items)
    color(avg.(eachcol(all))...)
end

color2int(rgb::@NamedTuple{r::UInt8, g::UInt8, b::UInt8}) = (rgb...,)

#const BORDER = color(0x9f87ef, 0x8978cd, 0x8877cb)
const BORDER = color(0x9a83e8)
#const BG = color(0x6347c2, 0x543ea3, 0x614bb4)
const BG = color(0x55409f)
#const FG = color(0x9884e3, 0x8673ce, 0x8472c8)
const FG = color(0x9983ea, 0xFFFFFF)

# currently unused
#"""
#Alternating input and output paragraphs
#The user can use the arrows and control keys to edit and move between input paragraphs
#"""
#@kwdef struct Page
#    para_layout::Layout = Layout(; widgets = [], constraints = Constraint[])
#    scroll_layout::Layout
#end

@kwdef struct FWidget
    func::Function
end

render(fw::FWidget, area::Rect, buffer::Buffer) = fw.func(area, buffer)

mutable struct ReplContext{Specialization} <: CompletionProvider
    asmlines::Vector{String}
    cmd_handler::Function
    pending_asm_prefix::String
    pending_asm_expr::String
    mode::REPL.LineEdit.Prompt
    input_file::Union{String,Nothing}
    input_file_channel::Union{Channel{Nothing},Nothing}
    input_file_changed::Bool
    worker::Worker
    labels::Set{Symbol}
    settings::Dict{Symbol}
    dirty::Bool
    runid::Int
    ReplContext{T}() where {T} = new{T}()
end

struct Completer <: CompletionProvider
end
