##########################################################################
# Default Priority for this panel
#
# priority is from 0 to 10
#  the lower the number, the quicker it will be bumped
#  10 cannot be bumped
#  Panels will be loaded by the greater of 5 or their current priority
##########################################################################
# global variables
#	Nv_(CurrCutPlane) 	0-5
#	Nv_(CutPlaneFence)	{ NONE, TOP, BOTTOM, BLEND, GREY }
#	Nv_(CutPlaneRotate)
#	Nv_(CutPlaneTilt)
#	Nv_(CutPlaneX)
#	Nv_(CutPlaneY)
#	Nv_(CutPlaneZ)
#       Nv_(CutPlanesMade)
#      Nv_(CutPlaneBase)
##########################################################################
# Procedure to make cutting planes panel
##########################################################################
set Nv_(CutPlanesMade) 0
set Nv_(CutPlaneFence) NONE
set Nv_(CurrCutPlane) -1

proc mkcutplanePanel { BASE } {
    global Nv_

    #  Initialize panel info
    set panel [St_create {window name size priority} $BASE "Cutting Planes" 1 5]
    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Cutting Planes Panel"
    set Nv_(CutPlaneBase) $BASE

    set update_routine xyupdate
    append update_routine $BASE
    set ucmd "proc $update_routine \{ x y \} \{ cutplaneXYTrans $BASE \$x \$y \}"
    uplevel #0 $ucmd
    #Create XY canvas
    set pos [Nv_mkXYScale $BASE.pos cross CPLANE_POS 125 125 63 63 $update_routine $update_routine]
    pack $pos -side top
    
    frame $BASE.right
    pack $BASE.right -side right -fill both -before $pos -padx 3 -pady 3 -anchor ne
    frame $BASE.left
    pack $BASE.left -side left -fill both -before $pos -padx 3 -pady 3 -anchor nw
    frame $BASE.bottom
    pack $BASE.bottom -side bottom -before $BASE.left -fill both -padx 3 -pady 3 -anchor sw
    
    # Create X,Y, and Z entry widgets along with
    # Reset, all off and close buttons
    frame $BASE.coords
    label $BASE.coords.z_lbl -text "Z:"
    label $BASE.coords.x_lbl -text "X:"
    label $BASE.coords.y_lbl -text "Y:"
    entry $BASE.coords.z_ent -width 5 -relief sunken
    entry $BASE.coords.x_ent -width 5 -relief sunken
    entry $BASE.coords.y_ent -width 5 -relief sunken
    bind $BASE.coords.x_ent <Return> "cutplaneSetTransFromEntry $BASE x"
    bind $BASE.coords.y_ent <Return> "cutplaneSetTransFromEntry $BASE y"
    bind $BASE.coords.z_ent <Return> "cutplaneSetTransFromEntry $BASE z"
    
    pack $BASE.coords.z_lbl $BASE.coords.z_ent \
	$BASE.coords.x_lbl $BASE.coords.x_ent \
	$BASE.coords.y_lbl $BASE.coords.y_ent -side left
    pack $BASE.coords -side top -in $BASE.bottom -fill both
    
    frame $BASE.commands
    button $BASE.commands.reset 	-text "Reset" 	-command "cutplaneReset $BASE"
    button $BASE.commands.all_off 	-text "All Off" \
	-command "cutplaneAllOff; cutplaneSetPlane $BASE -1"
    button $BASE.commands.close		-text "Close"	-command "Nv_closePanel $BASE" 
    pack $BASE.commands.reset $BASE.commands.all_off $BASE.commands.close \
	-side left -fill both -padx 3 -pady 3 -expand yes
    pack $BASE.commands -in $BASE.bottom -fill both
    
    # Create Z slider
    set update_routine zupdate
    append update_routine $BASE
    set ucmd "proc $update_routine \{ z \} \{ cutplaneZTrans $BASE \$z \}"
    uplevel #0 $ucmd
    set range [Nget_zrange]
#    set range [list 0 1000]
    scale $BASE.zslide -orient vertical -to [expr int([lindex $range 0])] \
	-from [expr int([lindex $range 1])] -showvalue false \
	-activebackground gray80 -background gray90 -command $update_routine
    pack $BASE.zslide -side top -in $BASE.left
    
    # Create the current plane pulldown mechanism
    frame $BASE.current
    label $BASE.current.lbl -text "Current:"
    label $BASE.current.cpl -text "None" -relief raised
    pack $BASE.current.lbl $BASE.current.cpl -side left -fill both
    pack $BASE.current -side top -in $BASE.right
    
    menu $BASE.cut_plane_menu
    set rname $BASE.cut_plane_menu
    $rname add command -label "None"    -command "cutplaneSetPlane $BASE -1"
    $rname add command -label "Plane 0" -command "cutplaneSetPlane $BASE 0"
    $rname add command -label "Plane 1" -command "cutplaneSetPlane $BASE 1"
    $rname add command -label "Plane 2" -command "cutplaneSetPlane $BASE 2"
    $rname add command -label "Plane 3" -command "cutplaneSetPlane $BASE 3"
    $rname add command -label "Plane 4" -command "cutplaneSetPlane $BASE 4"
    $rname add command -label "Plane 5" -command "cutplaneSetPlane $BASE 5"

    if {$Nv_(CutPlanesMade) == 0} then {
	for {set i 0} {$i < 6} {incr i} {
	    Nnew_cutplane_obj $i
	}
    }
    
    bind $BASE.current.cpl <1> "$rname post %X %Y"
    set Nv_(CutPlaneFence) OFF
    Nset_fence_color OFF
    
    # Create radio buttons for selecting buffers
    frame $BASE.buffers
    radiobutton $BASE.buffers.t 	-text "T" 	-width 3 -anchor w \
	-command "Nset_fence_color ABOVE" -variable Nv_(CutPlaneFence) -value "ABOVE"
    radiobutton $BASE.buffers.b		-text "B" 	-width 3 -anchor w \
	-command "Nset_fence_color BELOW" -variable Nv_(CutPlaneFence) -value "BELOW"
    radiobutton $BASE.buffers.bl	-text "BL"	-width 3 -anchor w \
	-command "Nset_fence_color BLEND" -variable Nv_(CutPlaneFence) -value "BLEND"
    radiobutton $BASE.buffers.gr	-text "GR"	-width 3 -anchor w \
	-command "Nset_fence_color GREY" -variable Nv_(CutPlaneFence) -value "GREY"
    radiobutton $BASE.buffers.n		-text "N"	-width 3 -anchor w \
	-command "Nset_fence_color OFF" -variable Nv_(CutPlaneFence) -value "OFF"

    pack $BASE.buffers.t $BASE.buffers.b $BASE.buffers.bl $BASE.buffers.gr $BASE.buffers.n -side top
    pack $BASE.buffers -in $BASE.right -side right -anchor ne
    
    # Create rotate and tilt sliders, labels and text entry widgets
    set update_routine rot_update
    append update_routine $BASE
    set ucmd "proc $update_routine \{ r \} \{ cutplaneUpdateRotation $BASE \}"
    uplevel #0 $ucmd
    frame $BASE.rotate
    scale $BASE.rotate.scl -orient vertical -from 0 -to 360 -showvalue false \
	-activebackground gray80 -background gray90 -command $update_routine
    label $BASE.rotate.lbl -text "Rotate"
    entry $BASE.rotate.val -width 3 -relief sunken
    pack $BASE.rotate.scl $BASE.rotate.lbl $BASE.rotate.val
    bind $BASE.rotate.val <KeyPress-Return> "cutplaneUpdateRotation2 $BASE"
    
    set update_routine tilt_update
    append update_routine $BASE
    set ucmd "proc $update_routine \{ t \} \{ cutplaneUpdateTilt $BASE \}"
    uplevel #0 $ucmd
    frame $BASE.tilt
    scale $BASE.tilt.scl -orient vertical -from 0 -to 360 -showvalue false \
	-activebackground gray80 -background gray90 -command $update_routine
    label $BASE.tilt.lbl -text "Tilt"
    entry $BASE.tilt.val -width 3 -relief sunken
    pack $BASE.tilt.scl $BASE.tilt.lbl $BASE.tilt.val
    bind $BASE.tilt.val <KeyPress-Return> "cutplaneUpdateTilt2 $BASE"
    
    pack $BASE.tilt $BASE.rotate -side right -in $BASE.right -before $BASE.buffers -anchor e
    $BASE.tilt.val insert 0 0 
    $BASE.rotate.val insert 0 0
    # cutplaneUpdateRotation $BASE
    # cutplaneUpdateTilt $BASE

    cutplaneSetPlane $BASE $Nv_(CurrCutPlane)

    return $panel
}

# Update routine - sets panel from gsf library
proc cutplaneUpdateFromGSF { BASE } {
    global Nv_

    set curr $Nv_(CurrCutPlane)
    set fence [Nget_fence_color]
    
    if { $curr != -1 } then {
	set rot [Ncutplane$curr get_rot]
	set trans [Ncutplane$curr get_trans]
    } else {
	set rot [list 0 0 0]
	set trans [list 0.5 0.5 0]
    }

    $BASE.rotate.scl set [lindex $rot 2]
    $BASE.tilt.scl set [lindex $rot 1]
    cutplaneXYTrans $BASE [lindex $trans 0] [lindex $trans 1]
    cutplaneZTrans $BASE [lindex $trans 2]
    set Nv_(CutPlaneFence) $fence
}

# Reset routine for cutplane panel
proc Nviz_cutplane_reset {} {
    global Nv_

    set Nv_(CurrCutPlane)  -1
    set Nv_(CutPlaneFence) NONE
    
    for {set i 0} {$i < 6} {incr i} {
	Ncutplane$i off
	Ncutplane$i set_rot 0 0 0
	Ncutplane$i set_trans 0 0 0
    }

    cutplaneSetPlane $Nv_(CutPlaneBase) $Nv_(CurrCutPlane)
    cutplaneUpdateFromGSF $Nv_(CutPlaneBase)
}

# Save routine for saving state of Nviz
proc Nviz_cutplane_save { file_hook } {
    global Nv_
return
    # Collect and save all the attributes from the six cutplanes
    # Plus save which one happens to be active
    puts $file_hook "$Nv_(CurrCutPlane)"
    puts $file_hook "$Nv_(CutPlaneFence)"

    for {set i 0} {$i < 6} {incr i} {
	puts $file_hook "[Ncutplane$i state]"
	puts $file_hook "[Ncutplane$i get_rot]"
	puts $file_hook "[Ncutplane$i get_trans]"
    }

}

# Load routine for loading state of Nviz
proc Nviz_cutplane_load { file_hook } {
    global Nv_
return
    gets $file_hook "$Nv_(CurrCutPlane)"
    gets $file_hook "$Nv_(CutPlaneFence)"

    for {set i 0} {$i < 6} {incr i} {
	gets $file_hook cstate
	if {"$cstate" == "on"} then {
	    Ncutplane$i on
	} else {
	    Ncutplane$i off
	}

	gets $file_hook crot
	set crot [split "$crot"]
	Ncutplane$i set_rot [lindex $crot 0] [lindex $crot 1] [lindex $crot 2]

	gets $file_hook ctrans
	set ctrans [split "$ctrans"]
	Ncutplane$i set_trans [lindex $ctrans 0] [lindex $ctrans 1] [lindex $ctrans 2]
    }

    cutplaneSetPlane $Nv_(CutPlaneBase) $Nv_(CurrCutPlane)
    
}

##########################################################################
# Callbacks to set current cut plane
##########################################################################
proc cutplaneSetPlane { BASE plane } {
    global Nv_

    $BASE.cut_plane_menu unpost
    if {$plane == -1} then {
	$BASE.current.cpl configure -text "None"
    } else {
	$BASE.current.cpl configure -text "Plane $plane"
    }
    for {set i 0} {$i < [Nnum_cutplane_obj]} {incr i} {
	if {$plane == $i} then {
	    Ncutplane$i on
	} else {
	    Ncutplane$i off
	}
    }
    
    set Nv_(CurrCutPlane) $plane
    
    set curr [Nget_current_cutplane]
    if {$curr != "None"} then {
	$curr draw
    }
    
}

##########################################################################
# Callbacks to update slider displays for rotate and tilt
##########################################################################
proc cutplaneUpdateRotation { BASE } {
	set value [$BASE.rotate.scl get]
	$BASE.rotate.val delete 0 end
	$BASE.rotate.val insert end $value

	# Call the rotation/tilt routine
	cutplaneUpdateRT $BASE
}

proc cutplaneUpdateRotation2 { BASE } {
	set value [$BASE.rotate.val get]
	if {[catch "expr int($value)"] == 1} then {
		set value 0
	} else {
		set value [expr int($value)]
	}
	$BASE.rotate.scl set $value

	# Call the rotation/tilt routine
	cutplaneUpdateRT $BASE
}

proc cutplaneUpdateTilt { BASE } {
	set value [$BASE.tilt.scl get]
	$BASE.tilt.val delete 0 end
	$BASE.tilt.val insert end $value

	# Call the rotation/tilt routine
	cutplaneUpdateRT $BASE
}

proc cutplaneUpdateTilt2 { BASE } {
	set value [$BASE.tilt.val get]
	if {[catch "expr int($value)"] == 1} then {
		set value 0
	} else {
		set value [expr int($value)]
	}
	$BASE.tilt.scl set $value

	# Call the rotation/tilt routine
	cutplaneUpdateRT $BASE
}

proc cutplaneUpdateRT { BASE } {
	set curr [Nget_current_cutplane]

	if {$curr != "None"} then {
		set tilt [$BASE.tilt.val get]
		set rot  [$BASE.rotate.val get]
		$curr set_rot 0 $tilt $rot
		$curr draw
	}
}

##########################################################################
# Routine to reset the current cutplane
##########################################################################
proc cutplaneReset { BASE } {
    global Nv_
    set curr [Nget_current_cutplane]
    set Nv_(CurrCutPlane) [string range $curr 9 end]

    if {$curr != "None"} then {
	$curr set_trans 0 0 0
	$curr set_rot 0 0 0
	cutplaneSetTrans $BASE 0 0 0
	
	# Now update the interface
	$BASE.rotate.val delete 0 end
	$BASE.rotate.val insert 0 0
	$BASE.tilt.val delete 0 end
	$BASE.tilt.val insert 0 0
	$BASE.zslide set 0
	cutplaneUpdateRotation2 $BASE
	cutplaneUpdateTilt2 $BASE
	
	Nv_itemDrag $BASE.pos $Nv_(CPLANE_POS) 63 63
    }
}

##########################################################################
# Routine to turn off all cutplanes
##########################################################################
proc cutplaneAllOff {} {
	for {set i 0} {$i < [Nnum_cutplane_obj]} {incr i} {
		Ncutplane$i off
	}

	Nquick_draw
}

##########################################################################
# Routine to set position of cutplane based on XY position
##########################################################################
proc cutplaneXYTrans { w x y } {
	set curr [Nget_current_cutplane]

	if {"$curr" != "None"} then {
		# Figure out translation coordinates
		set new_x [expr ($x - 0.5) * [Nget_xyrange]]
		set new_y [expr ($y - 0.5) * [Nget_xyrange]]
		set new_z [lindex [$curr get_trans] 2]

		# Call the general translation routine
		cutplaneSetTrans $w $new_x $new_y $new_z
	}
}

##########################################################################
# Routine to set position of cutplane based on Z position
##########################################################################
proc cutplaneZTrans { w z } {
	set curr [Nget_current_cutplane]

	if {"$curr" != "None"} then {
		# Figure out translation coordinates
		set old [$curr get_trans]
		set new_x [lindex $old 0]
		set new_y [lindex $old 1]

		# Call the general translation routine
		cutplaneSetTrans $w $new_x $new_y $z
	}
}

##########################################################################
# Routine to set cutplane translation from an entry widget
##########################################################################
proc cutplaneSetTransFromEntry { BASE coord } {
	global Nv_

	set curr [Nget_current_cutplane]

	if {"$curr" != "None"} then {
		# Get old translation coordinates
		set old_coords [$curr get_trans]
		set new_x [lindex $old_coords 0]
		set new_y [lindex $old_coords 1]
		set new_z [lindex $old_coords 2]

		# Get the appropriate new coordinate
		switch $coord {
			x { set new_x [$BASE.coords.x_ent get] }
			y { set new_y [$BASE.coords.y_ent get] }
			z { set new_z [$BASE.coords.z_ent get] }
		}

		# Make sure user entered a numerical value
		# if not force a numerical value
		if {[catch "expr $new_x + 0"] != 0} then { set new_x 0 }
		if {[catch "expr $new_y + 0"] != 0} then { set new_y 0 }
		if {[catch "expr $new_z + 0"] != 0} then { set new_z 0 }

		#Update Z-scale to match entry value
		#Reset to / from limits if required
		if {$new_z < [lindex [$BASE.zslide configure -to] 4]} {
		$BASE.zslide configure -to [expr int($new_z - 1)]
		}
		if {$new_z > [lindex [$BASE.zslide configure -from] 4]} {
                $BASE.zslide configure -from [expr int($new_z + 1)]
                }
		$BASE.zslide set $new_z

		#Update Canvas position based on entered XY
		if { [Nget_xyrange] > 0} {
		set dis_x [expr int( ($new_x/([Nget_xyrange]/2))*63)+63]
		set dis_y [expr int( ($new_y/([Nget_xyrange]/2))*63)+63]
		Nv_itemDrag $BASE.pos $Nv_(CPLANE_POS) $dis_x $dis_y
		}

		# Finally set the translation
		cutplaneSetTrans $BASE $new_x $new_y $new_z
	}
}

##########################################################################
# Routine to set position (general)
##########################################################################
proc cutplaneSetTrans { w x y z } {
	set curr [Nget_current_cutplane]

	if {"$curr" != "None"} then {
		$w.coords.x_ent delete 0 end
		$w.coords.y_ent delete 0 end
		$w.coords.z_ent delete 0 end

		$w.coords.x_ent insert 0 $x
		$w.coords.y_ent insert 0 $y
		$w.coords.z_ent insert 0 $z

		$curr set_trans $x $y $z
		$curr draw
	}
}

















