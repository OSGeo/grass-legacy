#!/bin/sh
# the next line restarts using wish \
exec wish $0 $*

# called by script d.siter

if {$argc < 9} {
    error "tclsiter.tcl SYNTAX ERROR"
}

set site_file [lindex $argv 0]

set i $argc
incr i -1

frame .sliders -relief raised -bd 4

while {$i > 1} {
    set alabel [lindex $argv $i]
    incr i -1
    set amax [lindex $argv $i]
    incr i -1
    set amin [lindex $argv $i]
    incr i -1
    set k [lindex $argv $i]
    incr i -1

    set resol 1
    set amaxi [expr ceil($amax)]
    set amini [expr floor($amin)]
    if {$amin != $amini || $amax != $amaxi} {
         set resol [expr ($amax - $amin)/250.]
         set amin [expr ($amin - $resol)]
         set amax [expr ($amax + $resol)]
    }

    scale .sliders.min$k -variable minval($k) -orient horizontal \
	  -from $amin -to $amax -label $alabel -width 10 -resolution $resol \
	  -length 250
    scale .sliders.max$k -variable maxval($k) -orient horizontal \
	  -from $amin -to $amax -width 10 -resolution $resol \
	  -length 250
    set minval($k) $amin
    set maxval($k) $amax
    lappend akeys $k

    set minorig($k) $amin
    set maxorig($k) $amax
}

frame .controls

frame .controls.status -relief raised -bd 4
label .controls.status.numd 

frame .controls.params -relief raised -bd 4
tk_optionMenu .controls.params.colors scolor grey red orange yellow green blue \
	      indigo white black brown magenta aqua
tk_optionMenu .controls.params.types stype x diamond box +
scale .controls.params.sizes -variable ssiz -orient vertical \
      -from 100 -to 1 -label size
set ssiz 5

frame .controls.cmds -relief raised -bd 4
button .controls.cmds.doit -text "display" -command "do_display" 
button .controls.cmds.erase -text "erase" -command "exec d.erase" 
button .controls.cmds.save -text "save as:" -command "do_save" 
entry .controls.cmds.savename -textvariable save_name 
button .controls.cmds.rescale -text "rescale" -command "do_rescale" 
button .controls.cmds.reset -text "reset scales" -command "do_reset" 
button .controls.cmds.quit -text "quit" -command exit 
set save_name $site_file.sub1
bind .controls.cmds.savename <Return> {
# somehow get rid of input focus
}


pack .sliders -side left
pack .controls -side left 
pack .controls.status -fill x
pack .controls.params -fill x
pack .controls.cmds -fill x

foreach i $akeys { 
    pack .sliders.min$i .sliders.max$i 
}

pack .controls.status.numd

pack .controls.params.sizes .controls.params.colors .controls.params.types

pack .controls.cmds.doit .controls.cmds.erase \
     .controls.cmds.save .controls.cmds.savename \
     .controls.cmds.rescale .controls.cmds.reset .controls.cmds.quit -fill x

proc do_display {} {
    global akeys minval maxval site_file scolor stype ssiz 
    foreach i $akeys { 
	append r [format "%s.%f-%f," $i $minval($i) $maxval($i)]
    }
    set numd [exec d.sites.qual -n $site_file color=$scolor \
	      type=$stype rules=$r size=$ssiz]
    .controls.status.numd configure -text "$numd sites drawn"
}

proc do_save {} {
    global akeys minval maxval site_file scolor stype ssiz save_name
    foreach i $akeys { 
	append r [format "%s.%f-%f," $i $minval($i) $maxval($i)]
    }
    set numd [exec d.sites.qual -n $site_file color=$scolor type=$stype \
		      rules=$r size=$ssiz out=$save_name]
    tk_dialog .saved saved \
	    "Current subset saved as sites file named '$save_name' \
	    with $numd sites qualifying" "" 0 OK
}

proc do_rescale {} {
    global akeys minval maxval
    
    foreach k $akeys {
	set resol 1
	if { $maxval($k) > $minval($k) } {
	    set amax $maxval($k)
	    set amin $minval($k)
	} else {
	    set amax $minval($k)
	    set amin $maxval($k)
	}
	set amaxi [expr ceil($amax)]
	set amini [expr floor($amin)]
	if {$amin != $amini || $amax != $amaxi} {
	     set resol [expr ($amax - $amin)/250.]
	     set amin [expr ($amin - $resol)]
	     set amax [expr ($amax + $resol)]
	}

	.sliders.min$k configure -from $amin \
		       -to $amax -resolution $resol
	.sliders.max$k configure -from $amin \
		       -to $amax -resolution $resol
    }
}

proc do_reset {} {
    global akeys minorig maxorig minval maxval
    
    foreach k $akeys {
	set resol 1
	set amin $minorig($k)
	set amax $maxorig($k)
	set amaxi [expr ceil($amax)]
	set amini [expr floor($amin)]
	if {$amin != $amini || $amax != $amaxi} {
	     set resol [expr ($amax - $amin)/250.]
	     set amin [expr ($amin - $resol)]
	     set amax [expr ($amax + $resol)]
	}

	.sliders.min$k configure -from $amin \
		       -to $amax -resolution $resol
	.sliders.max$k configure -from $amin \
		       -to $amax -resolution $resol
	set minval($k) $amin
	set maxval($k) $amax
    }
}

;# added by al 10/2000
proc error { errmsg } {
    puts "d.siter error in: tclsiter.tcl"
    puts "$errmsg\n"
    exit 1
}