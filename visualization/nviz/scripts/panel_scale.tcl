#!../glnviz/nvwish -f
# 4/4/95
# M. Astley
# USACERL, blah blah blah
##########################################################################
# 
# Panel to facilitate scale placement for finishing images produced
# by nviz.
#
##########################################################################

# Changes
#

# Panel specific globals
global Nv_

# Font Type: Times, Helvetica, Courier
# set Nv_(labelFontType) Times

# Font Weight: Italic, Bold
# set Nv_(labelFontWeight) Bold

# Font Point Size: varies
# set Nv_(labelFontSize) 12

##########################################################################

proc mkscalePanel { BASE } {
    global Nv_

    set panel [St_create {window name size priority} $BASE "Scale" 2 5]
    frame $BASE -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Scale Panel"
    
    ##########################################################################
    # This section contains widgets for placing a scale object
    frame $BASE.place_scale -relief groove -bd 5
    set rbase $BASE.place_scale

    button $rbase.place -text "Place Scale Object"
    frame $rbase.types
    pack $rbase.place $rbase.types
    frame $rbase.types.left
    frame $rbase.types.right
    radiobutton $rbase.types.left.solid  -text "Solid" -anchor w
    radiobutton $rbase.types.left.wire   -text "Wire"  -anchor w
    radiobutton $rbase.types.right.cube  -text "Cube"  -anchor w
    radiobutton $rbase.types.right.plane -text "Plane" -anchor w
    pack $rbase.types.left.solid $rbase.types.left.wire   -anchor w
    pack $rbase.types.right.cube $rbase.types.right.plane -anchor w
    pack $rbase.types.left $rbase.types.right -side left
    frame $rbase.bottom
    pack $rbase.bottom -side bottom
    button $rbase.bottom.place2 -text "Place Arrow" \
	-command "bind $Nv_(TOP).canvas <Button> {place_narrow %W %x %y }"
    checkbutton $rbase.bottom.narrow -text "North Arrow" -anchor w \
	-variable n_arrow -onvalue 1 -offvalue 0 
    pack $rbase.bottom.place2 $rbase.bottom.narrow -expand no -side left

    ##########################################################################
    # Separator
    Nv_makeSeparator $BASE.sep1

    ##########################################################################
    # This section contains widgets for draw ruler functionality
    frame $BASE.draw_ruler -relief groove -bd 5
    set rbase $BASE.draw_ruler

    button $rbase.draw -text "Draw Ruler" -command "draw_fringe"

    frame $rbase.where
    frame $rbase.where.top
    frame $rbase.where.bot
    label $rbase.where.top.nwl -text "NW"
    label $rbase.where.top.nel -text "NE"
    label $rbase.where.bot.swl -text "SW"
    label $rbase.where.bot.sel -text "SE"
    checkbutton $rbase.where.top.nwc -width 0 \
        -variable fringe_nw -onvalue 1 -offvalue 0
    checkbutton $rbase.where.top.nec -width 0 \
        -variable fringe_ne -onvalue 1 -offvalue 0
    checkbutton $rbase.where.bot.swc -width 0 \
        -variable fringe_sw -onvalue 1 -offvalue 0
    checkbutton $rbase.where.bot.sec -width 0 \
        -variable fringe_se -onvalue 1 -offvalue 0
    pack $rbase.where.top.nwl $rbase.where.top.nwc \
	$rbase.where.top.nec $rbase.where.top.nel -side left
    pack $rbase.where.bot.swl $rbase.where.bot.swc \
	$rbase.where.bot.sec $rbase.where.bot.sel -side left
    pack $rbase.where.top $rbase.where.bot 
    
    frame $rbase.elev
    frame $rbase.elev.entries
    frame $rbase.elev.text

    entry $rbase.elev.entries.min -width 8 -relief sunken
    entry $rbase.elev.entries.max -width 8 -relief sunken
    pack $rbase.elev.entries.min $rbase.elev.entries.max

    label $rbase.elev.text.one -text "<- elev ->" -width 10
    label $rbase.elev.text.two -text "size"       -width 10
    pack $rbase.elev.text.one $rbase.elev.text.two

    checkbutton $rbase.elev.auto -text "Auto"

    pack $rbase.elev.entries $rbase.elev.text \
	$rbase.elev.auto -side left -anchor n

    button $rbase.color -text "Color" \
	-bg \#ffffff -width 8 \
	-command "change_label_color $rbase.color"

    pack $rbase.draw $rbase.where $rbase.elev $rbase.color -expand no

    ##########################################################################
    # Pack all frames and exit
    pack $BASE.place_scale -fill x
    pack $BASE.sep1 -fill x
    pack $BASE.draw_ruler -fill x

    return $panel
}
#############################################################

######################
proc draw_fringe {} {
global Nv_
global fringe_nw fringe_ne fringe_sw fringe_se
    
set surf [Nget_current surf]
Ndraw_fringe $surf $fringe_nw $fringe_ne $fringe_sw $fringe_se
    
} 

###########################
proc place_narrow {W x y} {
global Nv_ n_arrow

set y [expr $Nv_(height) - $y]

#Draw North Arrow at selected point
if {$n_arrow == 1} {
    set curr [Nget_current surf]
    if {$curr} {
    Ndraw_Narrow $x $y $curr 
    }
#remove canvas binding
    bind $W <Button> {}
}

}








