##########################################################################
# 
# Panel to facilitate scale placement for finishing images produced
# by nviz.
#
# 4/4/95
# M. Astley
# U.S. Army Construction Engineering Research Laboratory
#
# Major update of GUI Nov 2006, Michael Barton, Arizona State University
#
##########################################################################
# COPYRIGHT:	(C) 2006 by Michael Barton and the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
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

proc mkfringePanel { BASE } {
    global Nv_
    global n_arrow_size n_arrow
	global fringe_nw fringe_ne fringe_sw fringe_se

    set panel [St_create {window name size priority} $BASE "Fringe" 2 5]
    frame $BASE -relief flat -borderwidth 0
    Nv_mkPanelname $BASE "Fringe Panel"

    set rbase1 [frame $BASE.edges]
    Label $rbase1.lbl -text "Edges with fringe: " -fg black
    checkbutton $rbase1.nwc -width 0 \
        -variable fringe_nw -onvalue 1 -offvalue 0 \
        -text "N&W"
    checkbutton $rbase1.nec -width 0 \
        -variable fringe_ne -onvalue 1 -offvalue 0 \
        -text "N&E"
    checkbutton $rbase1.swc -width 0 \
        -variable fringe_sw -onvalue 1 -offvalue 0 \
        -text "S&W"
    checkbutton $rbase1.sec -width 0 \
        -variable fringe_se -onvalue 1 -offvalue 0 \
        -text "S&E"
    pack $rbase1.lbl $rbase1.nwc $rbase1.nec $rbase1.swc \
    	$rbase1.sec -side left -expand yes -fill none -anchor w
    pack $rbase1 -side top -expand yes -fill both -padx 3 -pady 4

    set rbase2 [frame $BASE.color_elev]
    LabelEntry $rbase2.entry -width 8 -relief sunken \
		-entrybg white -textvariable Nv_(fringe_elev) \
		-label "Fringe elevation: "
    Button $rbase2.color -text "Color" \
		-bg "#aaaaaa" -width 8 -bd 1 \
		-command "change_fringe_color $rbase2.color" \
		-fg "#000000"
    pack $rbase2.entry -side left \
    	-expand yes -fill none -anchor w
    pack $rbase2.color -side right \
    	-expand yes -fill none -anchor e
	pack $rbase2 -side top -expand yes -fill both -padx 3 -pady 4

    set rbase3 [frame $BASE.button]
    Button $rbase3.draw -text "Draw Fringe" -command "draw_fringe" -bd 1
    # close panel section
    button $rbase3.close -text "Close" -command "Nv_closePanel $BASE" \
		-anchor se -bd 1
    pack $rbase3.draw -side left -expand yes -fill none -anchor w
    pack $rbase3.close -side right -fill none -expand yes -anchor e
	pack $rbase3 -side top -expand yes -fill both -padx 3 -pady 4

    set Nv_(fringe_elev) [lindex [Nget_zrange] 0]

    return $panel
}
#############################################################

# Simple routine to change the color of fringe
proc change_fringe_color { me } {
	global Nv_
	
	# set color button background to match fringe color
    set clr [lindex [$me configure -bg] 4]
    set clr [mkColorPopup .colorpop Fringe_Color $clr 1]
    set Nv_(fringe_color) $clr
    $me configure -bg $clr

	# set color button text to black or white depending on
	# darkness of color
    set clrnum [split $clr {}]
    set rhex "0x[lindex $clrnum 1][lindex $clrnum 2]"
    set ghex "0x[lindex $clrnum 3][lindex $clrnum 4]"
    set bhex "0x[lindex $clrnum 5][lindex $clrnum 6]"
    set clrsum [expr $rhex + $ghex +$bhex]
   
    if {$clrsum < 400 } {
	   $me configure -fg "white"
	 } else {
	   $me configure -fg "black"
	 }
}


######################
proc draw_fringe {} {
	global Nv_
	global fringe_nw fringe_ne fringe_sw fringe_se
	global fringe
	global Nauto_draw
		
	set surf [Nget_current surf]
	set fringe 1
		
	Ndraw_fringe $surf $Nv_(fringe_color) $Nv_(fringe_elev) $fringe_nw $fringe_ne $fringe_sw $fringe_se

	if {$Nauto_draw == 1} {
		Nset_cancel 0
		Ndraw_all
	} 
		
} 

###########################
proc place_narrow {W x y} {

global Nv_ n_arrow n_arrow_size
global n_arrow_x n_arrow_y n_arrow_z

set y [expr $Nv_(height) - $y]

#Draw North Arrow at selected point
    set curr [Nget_current surf]
    if {$curr} {
        set location [Nset_Narrow $x $y $curr $n_arrow_size]
        set n_arrow_x [lindex $location 0]
        set n_arrow_y [lindex $location 1]
        set n_arrow_z [lindex $location 2]

        Ndraw_Narrow $n_arrow_x $n_arrow_y $n_arrow_z $n_arrow_size
        #set chuckbutton
        set n_arrow 1
    }

#remove canvas binding
    bind $W <Button> {}

}








