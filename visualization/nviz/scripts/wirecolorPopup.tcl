# mkWireColorPopup w name color
#
# Create a dialog box with sliders and buttons to adjust current color
# of wire mesh for current surface 
#
# Arguments:
#    w -	Name to use for new top-level window.
#    name -     Label for ColorPopup
#    color -    CurrentColor (set to UseMap if map is used)


global CurrWireColor

proc mkWireColorPopup {w name {color "#000000"} {mode 0}} {
    global CurrWireColor

    catch {destroy $w}
    toplevel $w -class Dialog
    wm title $w "Select Color"
    wm iconname $w "Color"
    wm geometry $w 300x450
    wm minsize $w 50 100
    wm resizable $w false false

    if {"$color" != "UseMap"} then {
	set tmp [tcl_to_rgb $color]
	set r [expr ($tmp&0xff0000)>>16]
	set g [expr ($tmp&0x00ff00)>>8]
	set b [expr ($tmp&0x0000ff)]
    } else {
	set r 0
	set g 0
	set b 0
    }

    set tmp $color

    # Create two frames in the main window. The top frame will hold the
    # sliders that interactively change color and the bottom one will hold 
    # the buttons for predefined color.  Below the two frames we create
    # a checkbutton widget allowing the color to be set to the colormap
    # for the surface

    frame $w.top -relief raised -border 1
    frame $w.bot -relief raised -border 1
    checkbutton $w.usemap -text "Use Surface Color" -onvalue UseMap \
	-offvalue 0 -variable CurrWireColor
    pack $w.top $w.bot $w.usemap -side top -fill both -expand yes

    frame $w.top.left
    Nv_mkWireColorScale $w.top.left Red $r red  $w.top.color
    Nv_mkWireColorScale $w.top.left Green $g green  $w.top.color
    Nv_mkWireColorScale $w.top.left Blue $b blue  $w.top.color

    pack $w.top.left.red $w.top.left.green $w.top.left.blue -side top -expand 1
    set CurrWireColor $color
    if {"$color" != "UseMap"} then {
	label $w.top.color -bg $color -width 5
    } else {
	label $w.top.color -bg \#FFFFFF -width 5
    }
    pack $w.top.left  -side left -expand 1
    pack $w.top.color -side left  -padx 10 -pady 10 -fill both -expand yes

    tkwait visibility $w
    
    frame $w.bot.buttonframe
    button $w.bot.buttonframe.ok -text OK -command "destroy $w"
    button $w.bot.buttonframe.cancel  \
	-text Cancel  -command "set CurrWireColor $tmp; destroy $w"
    label $w.bot.buttonframe.label -text $name
    pack $w.bot.buttonframe.label  -side left -expand yes -padx 10
    pack $w.bot.buttonframe.cancel $w.bot.buttonframe.ok -side right -expand 1
    pack $w.bot.buttonframe -side bottom -fill x 
    mkWireColorButtons $w.bot.bf $w.top.left $w.top.color
    pack $w.bot.bf -padx 10 -pady 20 -side top -expand 1
    bind $w <Any-Enter> [list focus $w]
    focus $w
    if {$mode} {grab $w}

    tkwait window $w

    return $CurrWireColor
}

proc mkWireColorButtons { B S L} {

    global CurrWireColor
    
    frame $B
    set clist [mkWireColorList]

    for {set i 0; set k 0} {$i < 9 } {incr i} {
	# make frame to hold buttons in this row 
	frame $B.f$i
	for {set j 0} {$j < 5 } {incr j; incr k} {
	    set color [lindex $clist $k]
	    button $B.f$i.$j -bg $color \
		-activeforeground $color \
		-activebackground $color \
		-width 0 -height 0 \
		-highlightthickness 1 \
		-padx 7 -pady 0 \
                -command "setWireScales $S $color; $L config -bg $color; set CurrWireColor $color"
	    pack $B.f$i.$j -padx 1 -pady 1 -side top -expand 1
	}
	pack $B.f$i -side left
    }
}

proc getWireColorfromScales {S} {
   

    set r [$S.red.scale get]
    set g [$S.green.scale get]
    set b [$S.blue.scale get]

    set r [hexval $r]
    set g [hexval $g]
    set b [hexval $b]

    
    return #$r$g$b
}




proc setWireLabelfromScales {S L args} {
    global CurrWireColor

    set r [$S.red.scale get]
    set g [$S.green.scale get]
    set b [$S.blue.scale get]

    set r [hexval $r]
    set g [hexval $g]
    set b [hexval $b]

    $L config -bg #$r$g$b
    set CurrWireColor #$r$g$b
}

proc setWireScales {S c} {
    set color [tcl_to_rgb $c]
    set r [expr ($color&0xff0000)>>16]
    set g [expr ($color&0x00ff00)>>8]
    set b [expr ($color&0x0000ff)]

    $S.red.scale set $r
    $S.green.scale set $g
    $S.blue.scale set $b
}

proc mkWireColorList {} {

    set ramp 0
    set colorlist {}
    set maxval ff
    set minval 00
    for {set r 4; set i 0} {$r < 16} {incr r 5} {
        for {set g 4} {$g < 16} {incr g 5} {
            for {set b 4} {$b < 16 && $i < 40} {incr b 5} {
		set tmpr [hexval [expr $r/15.0*255]]
		set tmpg [hexval [expr $g/15.0*255]]
		set tmpb [hexval [expr $b/15.0*255]]
		set color #$tmpr$tmpg$tmpb
		set colorlist [concat $colorlist $color]
		incr i
	    }
	    if {$ramp == 0} {
		set colorlist [concat $colorlist #ff0000 #00ffff]
		incr ramp 2
		incr i 2
	    } elseif {$ramp < 10} {
	        set tmpg [hexval [expr $ramp/9.0*255]]
		set color #$maxval$tmpg$minval
		set colorlist [concat $colorlist $color]
		incr ramp
		incr i

	        set tmpg [hexval [expr (1.0 - $ramp/9.0)*255]]
		set color #$minval$tmpg$maxval
		set colorlist [concat $colorlist $color]
		incr ramp
		incr i

	    } elseif {$ramp < 16} {
	        set tmpb [hexval [expr ($ramp-10)/7.0*255]]
		set color #$maxval$maxval$tmpb
		set colorlist [concat $colorlist $color]
		incr ramp
		incr i
	        set tmpb [hexval [expr (1.0 - ($ramp-10)/7.0)*255]]
		set color #$minval$minval$tmpb
		set colorlist [concat $colorlist $color]
		incr ramp
		incr i
	    }
	}
    }
    for {set gray 0} {$gray < 5} {incr gray} {
	set g [hexval [expr (1.0 - $gray/4.0)*255]]
	set color #$g$g$g
	set colorlist [concat $colorlist $color]
    }


    return $colorlist

}

##########################################################################
# procedure to make sliders
##########################################################################
proc Nv_mkWireColorScale { P {name " "} {curr 200}\
    {color ""} {chip ""}} {

    set S $P.$color
    frame $S
    frame $S.f

    scale $S.scale -from 0 -length 140 -showvalue 0 -orient h\
        -tickinterval 0 -to 255 -width 13 \
        -command "Nv_scaleCallback $S s 0 null; setWireLabelfromScales $P $chip " \
        -activebackground gray80 -background gray90 -bg $color
       
    label $S.f.label -text $name
    $S.scale set $curr
    entry $S.f.entry -width 5 -borderwidth 2 -relief sunken
    bind $S.f.entry <Return> \
	"Nv_scaleCallback $S e 0 null; setWireLabelfromScales $P $chip"
    pack $S.scale $S.f -side top
    pack $S.f.label $S.f.entry -side left

    return $S
}



