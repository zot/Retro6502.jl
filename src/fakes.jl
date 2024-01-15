###
### FAKE ROUTINES
###

function print_n(mach::Machine)
    println("PRINT CHARACTER $(hex(mach.cpu.a)), $(hex(mach.cpu.x)) TIMES")
    #for _ in 1:mach.cpu.x
    #    call_6502(mach, :chrout)
    #end
end
