##########################################################################
# Default Priority for this panel
#
# priority is from 0 to 10
#  the lower the number, the quicker it will be bumped
#  10 cannot be bumped
#  Panels will be loaded by the greater of 5 or their current priority

#########################################################################
#create Color panel
##########################################################################

global BGColor

proc mkcolorPanel { BASE } {
    global Nv_

    catch {destroy $BASE}

    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
        set panel [St_create {window name size priority} $BASE "Color" 1 5]
    } else {
	set panel $Nv_($BASE)
    }

    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Color Panel"
    
    button $BASE.background -text Background -bg white -fg grey \
	-activebackground gray20 -activebackground white\
	-command "set_background_color $BASE.background"\
	-height 3 -width 12
    # place $BASE.background -rely .33 -relx .33 -relheight .20 -relwidth .40
    pack $BASE.background -padx 5 -pady 5
    frame $BASE.closef
    button $BASE.closef.close -text Close -command "Nv_closePanel $BASE" \
	-anchor se
    pack $BASE.closef.close -side right
    pack $BASE.closef -side bottom -fill x

    return $panel
}

proc set_background_color {W} {
    global BGColor

    set BGColor [mkColorPopup .colorPop Background $BGColor]
    $W config -bg $BGColor
    $W config -activebackground $BGColor
    Nbackground $BGColor
    Nquick_draw
}

# Reset procedure for color panel
proc Nviz_color_reset {} {
    Nbackground "#FFFFFF"
    Nquick_draw
}

# Load procedure for loading state of Nviz
proc Nviz_color_load {file_hook} {
    global BGColor
    
    # Nothing fancy here, just load the background color
    gets $file_hook BGColor
    Nbackground $BGColor
    Nquick_draw

}

# Save procedure for saving state of Nviz
proc Nviz_color_save {file_hook} {
    global BGColor

    # Nothing fancy here, just save the background color
    puts $file_hook "$BGColor"
}
