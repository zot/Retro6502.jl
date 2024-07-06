###
### ALERT!!
### replbase detected!
### INFORM THE EMPIRE!
###

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
const screen_style = Crayon(; foreground = color2int(FG), background = color2int(BG))
const pc_style = merge(screen_style, Crayon(; underline = true, bold = true))
const border_style = Crayon(; foreground = color2int(BG), background = color2int(BORDER))
const dim_style = merge(screen_style, Crayon(; faint = true))

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
    props::NamedTuple = (;)
end

render(fw::FWidget, area::Rect, buffer::Buffer) = fw.func(area, buffer)

#const EXAMPLE = ["READY!", ".load examples/statichello.jas"]
#const EXAMPLE = ["READY!", ".load examples/move.jas"]
const EXAMPLE = ["READY!", ".load examples/macro.jas"]

"""
Screen structure, parameterized to allow customization
"""
@kwdef struct Screen{T} <: TUI.Model
    regs::Ref{String} = Ref("")
    monitor::Paragraph =
        Paragraph(Block(; title = "", border = 0), make_words("C64 state", screen_style), 1)
    status::Ref{AbstractString} = "CTRL-Q: QUIT"
    regsblock::Ref{FWidget} = Ref{FWidget}()
    layout::Layout = Layout(; widgets = [], constraints = Constraint[])
    quit::Ref{Bool} = Ref(false)
    repllines::Vector{String} = EXAMPLE
    wrappedlines::Vector{Vector{String}} = [[e] for e in EXAMPLE]
    screenlines::Vector{String} = String[]
    cursor::Ref{Int} = Ref(0) # col
    diag::Ref{Bool} = Ref(false)
    repl::Any
    mode::Ref{Symbol} = Ref(:repl)
    cpu::Union{Nothing,Cpu} = nothing
    screenloc::Ref{Tuple{UInt16,UInt16}} = Ref((0x0001, 0x0001)) # row, col
    areas::Dict{Symbol,Rect} = Dict{Symbol,Rect}(
        :repl => Rect(),
        :registers => Rect(),
        :monitor => Rect(),
        :status => Rect(),
    )
    mouse_area::Ref{Symbol} = Ref(:repl)
    screenwid::Ref{Int} = Ref(0)
    screenfile::Tuple{String,IO} = mktemp()
    image::Vector{UInt8} = mmap(screenfile[2], Vector{UInt8}, (320 * 200 * 3,))
    imageshown::Ref{Bool} = Ref(false)
    # snapshot of previous screen and char defs to eliminate computing redundant images
    lastscreen::Ref{Vector{UInt8}} = Ref(UInt8[])
    lastchars::Ref{Vector{UInt8}} = Ref(UInt8[])
    lastcmd::Ref{String} = Ref("")
    lastevt::Ref{Crossterm.Event} = Ref{Crossterm.Event}(
        TUI.KeyEvent(
            Crossterm.EventTag.KEY,
            Crossterm.KeyEvent("", String[], "", String[]),
        ),
    )
end

mutable struct Repl{Specialization}
    asmlines::Vector{String}
    asmlist::String
    cmd_handler::Function
    pending_asm_prefix::String
    pending_asm_expr::String
    mode::REPL.LineEdit.Prompt
    input_file::Union{String,Nothing}
    input_file_channel::Union{Channel{Nothing},Nothing}
    input_file_changed::Bool
    worker::Worker
    state::Workers.CpuState
    labels::Set{Symbol}
    settings::Dict{Symbol}
    dirty::Bool
    runid::Int
    banksettings::BankSettings
    screen::Screen{Specialization}
    lastupdate::UInt16
    Repl{T}() where {T} = new{T}()
end

struct Completer <: CompletionProvider end
