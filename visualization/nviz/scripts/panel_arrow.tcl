##########################################################################
# 
# Panel to facilitate north arrow placement for finishing images produced
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

proc mkarrowPanel { BASE } {
    global Nv_
    global n_arrow_size n_arrow arw_text_size
    global arw_clr arw_text_clr
    global nviztxtfont

    # defaults
    set n_arrow_size 1000
    set arw_clr "#000000"
    set arw_text_clr "#DDDDDD"
    #This doesn't do anything currently
    set arw_text_size "not funct." 

    set panel [St_create {window name size priority} $BASE "North arrow" 2 5]
    frame $BASE -relief flat -borderwidth 0
    Nv_mkPanelname $BASE "North Arrow Panel"

    # This section contains widgets for placing the north arrow
    set rbase1 [frame $BASE.arrow]
    Label $rbase1.arrow_lbl -text "Arrow: " -fg black
    LabelEntry $rbase1.arrow_size -relief sunken -entrybg white \
        -textvariable n_arrow_size -width 8 -justify right\
        -label "size (in map units) " -fg black -labelfont $nviztxtfont
    pack $rbase1.arrow_lbl $rbase1.arrow_size -side left -expand no -fill none
    
    $rbase1.arrow_size bind <Key> {if {$Nauto_draw == 1} {Ndraw_all}} 

    Button $rbase1.color -text "Color" \
		-bg $arw_clr -width 8 -bd 1 \
		-command "change_arrow_color $rbase1.color arrow" \
		-fg "#ffffff"
    pack $rbase1.color -side right \
    	-expand yes -fill none -anchor e

	pack $rbase1 -side top -expand yes -fill both -padx 3 -pady 4

    # This section contains widgets for north text
    set rbase2 [frame $BASE.txt]
    Label $rbase2.txt_lbl -text "North text: " -fg black
    LabelEntry $rbase2.txt_size -relief sunken -entrybg grey \
        -textvariable arw_text_size -width 8 -justify right\
        -label "size " -fg black -labelfont $nviztxtfont
    pack $rbase2.txt_lbl $rbase2.txt_size -side left -expand no -fill none
    
    $rbase2.txt_size bind <Key> {if {$Nauto_draw == 1} {Ndraw_all}} 

    Button $rbase2.color -text "Color" \
		-bg $arw_text_clr -width 8 -bd 1 \
		-command "change_arrow_color $rbase2.color text" \
		-fg "#ffffff"
    pack $rbase2.color -side right \
    	-expand yes -fill none -anchor e

	pack $rbase2 -side top -expand yes -fill both -padx 3

    # close panel section
    set rbase3 [frame $BASE.button]
    Button $rbase3.place -text "Place arrow" -bd 1 \
	 -command "bind_mouse $Nv_(TOP).canvas"
    pack $rbase3.place -expand yes -side left -expand no -fill none

    button $rbase3.close -text "Close" -command "Nv_closePanel $BASE" \
		-anchor se -bd 1
	pack $rbase3.close -side right -fill none -expand no
	pack $rbase3 -side top -fill both -expand yes -padx 3 -pady 4

    return $panel
}

proc bind_mouse { W } {
	bind $W <1> {
		place_narrow %W %x %y 
		if {$Nauto_draw == 1} {
			#Nset_cancel 0
			Ndraw_all
		} 
	}
}

#############################################################

# Simple routine to change the color of Arrow.
# text color not yet user settable.
proc change_arrow_color { me type } {
	global Nv_
	global arw_clr arw_text_clr
	global Nauto_draw
	
	# set color button background to match arrow color
    set clr [lindex [$me configure -bg] 4]
    set clr [mkColorPopup .colorpop arw_clr $clr 1]
    if {$type == "arrow"} {
	    set arw_clr $clr
	} elseif {$type == "text"} {
		set arw_text_clr $clr
	}
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
proc place_narrow {W x y} {

global Nv_ n_arrow n_arrow_size
global n_arrow_x n_arrow_y n_arrow_z
global arw_clr arw_text_clr
global Nauto_draw

set y [expr $Nv_(height) - $y]

#Draw North Arrow at selected point
    set curr [Nget_current surf]
    if {$curr} {
        set location [Nset_Narrow $x $y $curr $n_arrow_size]
        set n_arrow_x [lindex $location 0]
        set n_arrow_y [lindex $location 1]
        set n_arrow_z [lindex $location 2]

        Ndraw_Narrow $n_arrow_x $n_arrow_y $n_arrow_z $n_arrow_size \
		$arw_clr $arw_text_clr
        #set chuckbutton
        set n_arrow 1
    }

#remove canvas binding
    bind $W <1> {}

}
