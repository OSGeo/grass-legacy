##########################################################################
# 
# Panel to facilitate scale placement for finishing images produced
# by nviz.
#
##########################################################################
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


##########################################################################

proc mkscalePanel { BASE } {
    global Nv_
    global scalebar_size scalebar
    global bar_clr bar_text_clr
    global nviztxtfont

    # defaults
    set scalebar_size 1000
    set bar_clr "#00000"
    set bar_text_clr "#DDDDDD"

    set panel [St_create {window name size priority} $BASE "Scale bar" 2 5]
    frame $BASE -relief flat -borderwidth 0
    Nv_mkPanelname $BASE "Scale Bar Panel"

    # This section contains widgets for placing the scale bar
    set rbase1 [frame $BASE.scale]
    LabelEntry $rbase1.scalebar_size -relief sunken -entrybg white \
        -textvariable scalebar_size -width 8 \
        -label "Scale bar size (in map units): " -fg black
    pack $rbase1.scalebar_size -side left -expand no -fill none
    
    $rbase1.scalebar_size bind <Key> {if {$Nauto_draw == 1} {Ndraw_all}} 


    Button $rbase1.color -text "Color" \
		-bg $bar_clr -width 8 -bd 1 \
		-command "change_scale_color $rbase1.color" \
		-fg "#ffffff"
    pack $rbase1.color -side right \
    	-expand yes -fill none -anchor e

	pack $rbase1 -side top -expand yes -fill both -padx 3 -pady 4

    # close panel section
    set rbase2 [frame $BASE.button]
    Button $rbase2.place -text "Place scale" -bd 1 \
	 -command "sb_bind_mouse $Nv_(TOP).canvas"
    pack $rbase2.place -expand yes -side left -expand no -fill none

    button $rbase2.close -text "Close" -command "Nv_closePanel $BASE" \
		-anchor se -bd 1
	pack $rbase2.close -side right -fill none -expand no
	pack $rbase2 -side top -fill both -expand yes -padx 3 -pady 4

    return $panel
}

proc sb_bind_mouse { W } {
	bind $W <1> {
		place_scale %W %x %y 
		if {$Nauto_draw == 1} {
			#Nset_cancel 0
			Ndraw_all
		} 
	}
}

#############################################################

# Simple routine to change the colors
# text color not yet user settable.
proc change_scale_color { me } {
	global Nv_
	global bar_clr bar_text_clr
	global Nauto_draw
	
	# set color button background to match arrow color
    set clr [lindex [$me configure -bg] 4]
    set clr [mkColorPopup .colorpop bar_clr $clr 1]
    set bar_clr $clr
#    set bar_text_clr $clr
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
	if {$Nauto_draw == 1} {
		Ndraw_all
	}
 
}
###########################
proc place_scale {W x y} {

global Nv_ scalebar scalebar_size
global scalebar_x scalebar_y scalebar_z
global bar_clr bar_text_clr
global Nauto_draw

set y [expr $Nv_(height) - $y]

#Draw scale bar at selected point
    set curr [Nget_current surf]
    if {$curr} {
        set location [Nset_ScaleBar $x $y $curr $scalebar_size]
        set scalebar_x [lindex $location 0]
        set scalebar_y [lindex $location 1]
        set scalebar_z [lindex $location 2]

        Ndraw_ScaleBar $scalebar_x $scalebar_y $scalebar_z $scalebar_size \
		$bar_clr $bar_text_clr
        #set chuckbutton
        set scalebar 1
    }

#remove canvas binding
    bind $W <1> {}

}
