# mkAttPopup w name att
#
# Create a dialog box change current attribute 
#
# Arguments:
#    w -	Name to use for new top-level window.
#    att  -   	Current Att 
#    mode - 	Disable events in other windows ?

proc mkAttPopup {w att {mode 0}} {
	
    global attPopup_InvertMask attPopup_Status attPopup_Type \
	attPopup_Color attPopup_UseColor
    
    set attPopup_Status ""
    set attPopup_Type "constant"
    set attPopup_Color ""

    # Need to make sure there is something current to change
    # the attribute of.
    if {[Nget_current surf] == 0} then {
	return
    }
    
    catch {destroy $w}
    toplevel $w -class Dialog 
    tkwait visibility $w
    
    wm positionfrom $w program
    wm sizefrom $w program
    wm title $w "Change Attribute"
    wm iconname $w "Attribute"
    wm geometry $w 200x300
    wm minsize $w 50 100
    
    # Callbacks depend on the attribute
    switch "$att" {
	topography { 
	    set cb1 ap_get_topofile
	    set cb2 "ap_show_newconst topo"
	}
	color	{
	    set cb1 ap_get_colorfile
	    set cb2 ap_get_colorconst
	}
	mask {
	    set cb1 "ap_get_rasterfile mask"
	    set cb2 ap_remove_mask
	}
	transparency {
	    set cb1 "ap_get_rasterfile transp"
	    set cb2 "ap_show_slideconst transp"
	}
	shininess	{
	    set cb1 "ap_get_rasterfile shin"
	    set cb2 "ap_show_slideconst shin"
	}
	emission {
	    set cb1 "ap_get_rasterfile emi"
	    set cb2 "ap_show_slideconst emi"
	}
    }
    
    frame $w.f1 -relief raised -border 1
    frame $w.f2 -relief raised -border 1
    frame $w.f3 -relief raised -border 1
    frame $w.f4 -relief raised -border 1
    pack $w.f1 $w.f2 $w.f3 $w.f4 -side top -fill both -expand yes
    
    label $w.f1.name -text "Change Attribute: $att" 
    pack $w.f1.name -side top -fill x -expand yes 
    button $w.f2.map -text "New Map" -command "$cb1"
    
    if {"$att" == "mask"} then {
	button $w.f2.const -text "Remove Mask" -command "$cb2"
	checkbutton $w.f2.invert -text "Invert Mask" -onvalue 1 \
	    -offvalue 0 -variable attPopup_InvertMask
	set curr [Nget_current surf]
	set attPopup_InvertMask [Nsurf$curr get_mask_mode]
    } elseif {"$att" == "topography" } then {
	button $w.f2.const -text "New Constant"	-command "$cb2"
	checkbutton $w.f2.use_color -text "Use as color" -onvalue 1 \
	    -offvalue 0 -variable attPopup_UseColor
	set attPopup_UseColor 1
    } else {
	button $w.f2.const -text "New Constant"	-command "$cb2"
    }
    
    pack $w.f2.map $w.f2.const -side left -expand yes
    
    if {"$att" == "mask"} then {
	pack $w.f2.invert -side bottom -expand yes -before $w.f2.map
    } elseif {"$att" == "topography"} then {
	pack $w.f2.use_color -side bottom -expand yes -before $w.f2.map
    }
    
    label $w.f3.status -text "Status: "
    set attPopup_Status [get_curr_status $att]
    label $w.f3.info -textvariable attPopup_Status
    pack $w.f3.status $w.f3.info -side top -fill x -expand yes
    
    button $w.f4.accept -text "Accept" \
	-command "ap_check_invert $att ; destroy $w"
    button $w.f4.cancel -text "Cancel" \
	-command "set attPopup_Status \"no_change\" ; destroy $w"
    pack $w.f4.accept $w.f4.cancel -side left -expand yes
    
    bind $w <Any-Enter> [list focus $w]
    focus $w
    if {$mode} {grab $w}
    
    
    # tkspecial_wait window $w
    tkwait window $w
    puts "Exiting from attPopup"

    # Signal to user that we are busy
    appBusy
    
    # Invoke the changes (or lack thereof)
    if { ("$attPopup_Status" != "no_change") } then {
	if { ("$attPopup_Status" != "$att not set") } then {
	    set curr [Nget_current surf]
	    if {0 == $curr} return
	    if {"$attPopup_Type" != "constant"} then {
		switch "$att" {
		    topography {
			Nsurf$curr set_att topo $attPopup_Status
			if {$attPopup_UseColor} then {
			    Nsurf$curr set_att color $attPopup_Status
			}
		    }
		    color {
			Nsurf$curr set_att color $attPopup_Status
		    }
		    mask {
			Nsurf$curr set_att mask $attPopup_Status
		    }
		    transparency {
			Nsurf$curr set_att transp $attPopup_Status
		    }
		    shininess	{
			Nsurf$curr set_att shin $attPopup_Status
		    }
		    emission {
			Nsurf$curr set_att emi $attPopup_Status
		    }
		}
	    } else {
		switch "$att" {
		    topography { 
			Nsurf$curr set_att topo constant $attPopup_Status
			if {$attPopup_UseColor} then {
			    Nsurf$curr set_att color constant $attPopup_Status
			}
		    }
		    color {
			Nsurf$curr set_att color constant $attPopup_Color
		    }
		    mask {
			Nsurf$curr set_att mask constant 0
			Nsurf$curr unset_att mask
		    }
		    transparency {
			Nsurf$curr set_att transp constant $attPopup_Status
		    }
		    shininess {
			Nsurf$curr set_att shin constant $attPopup_Status
		    }
		    emission {
			Nsurf$curr set_att emi constant $attPopup_Status
		    }
		}
	    }
	} else { Nsurf$curr unset_att $att }
    }

    # Make sure we update the display so the changes take effect
    set curr [Nget_current surf]
    if {0 == $curr} return
    set_new_curr surf $curr
    set_display_from_curr
    #	Nquick_draw

    # Show that we are no longer busy
    appNotBusy
    
}

proc get_curr_status {att} {
    global attPopup_Color 

    set curr [Nget_current surf]
    
    if {$curr == 0} then { 
	set txt "$att not set"
    } else { 
	set map_pair [Nsurf$curr get_att $att] 
	if {[lindex $map_pair 0] == "unset"} then {
	    set txt "$att not set"
	} else {
	    set txt [lindex $map_pair 1]
	}
    }
    
    if {"$att" == "color"} then {
	if {![regexp {[a-z,A-Z]} $txt]} then {
	    set txt [expr int($txt)]
	    set red   [expr int($txt & 0x0000ff)]
	    set green [expr int(($txt & 0x00ff00)>>8)]
	    set blue  [expr int(($txt & 0xff0000)>>16)]
	    set attPopup_Color [expr ($red<<16) + ($green<<8) + $blue]
	    set txt "color: R$red, G$green, B$blue"
	}
    }
    return $txt
}

###############################################################
#	ap_get_topofile -
#		Routine to get a new topography file
#
#	Arguments:
#		None
#	Returns:
#		None
#	Side Effects:
#		None
###############################################################
proc ap_get_topofile {} {
    global attPopup_Status attPopup_Type
    
    set new [create_map_browser .browse_topo_file surf 1]
    if { $new == "" } then { return }
    
    puts "returned from create_map_browser"
    set attPopup_Type non_constant
    set attPopup_Status $new
}

###############################################################
#	ap_get_colorfile -
#		Routine to get a new color file
#
#	Arguments:
#		None
#	Returns:
#		None
#	Side Effects:
#		None
###############################################################
proc ap_get_colorfile {} {
    global attPopup_Status attPopup_Type

    set new [create_map_browser .browse_color_file surf 1]
    if { $new == "" } then { return }

    set attPopup_Type non_constant
    set attPopup_Status $new
}

###############################################################
#	ap_get_colorconst -
#		Routine to get a color constant (i.e. popup the color dialog)
#
#	Arguments:
#		None
#	Returns:
#		None
# 	Side Effects:
#		None
###############################################################
proc ap_get_colorconst {} {
    global attPopup_Status attPopup_Type attPopup_Color 

    # Create the color popup to get the new color
    set curr_color $attPopup_Status
    if {[regexp color: $curr_color]} then {
	set curr_color $attPopup_Color
	set red   [hexval [expr ($curr_color & 0xff0000)>>16]]
	set green [hexval [expr ($curr_color & 0x00ff00)>>8]]
	set blue  [hexval [expr ($curr_color & 0x0000ff)]]
	set curr_color "#"
	append curr_color $red $green $blue
    } else {
	set curr_color "#000000"
    }
	
    set new_color [mkColorPopup .color_browse "Surface Color" $curr_color 1]
    
    # Finally pass the constant on to change the surface
    set new_color [expr [tcl_to_rgb $new_color] + 0]
    
    set red   [expr int(($new_color & 0xff0000)>>16)]
    set green [expr int(($new_color & 0x00ff00)>>8)]
    set blue  [expr int($new_color & 0x0000ff)]
    set attPopup_Status "color: R$red, G$green, B$blue"
    set attPopup_Color $new_color
    set attPopup_Type constant
}

###############################################################
#	ap_remove_mask -
#		Routine to remove a mask from a surface
#
#	Arguments:
#		None
#	Returns:
#		None
#	Side Effects:
#		None
###############################################################
proc ap_remove_mask {} {
    global attPopup_Status attPopup_Type

    set attPopup_Status "mask not set"
    set attPopup_Type constant
}

###############################################################
# 	ap_get_rasterfile -
#		Routine to get a new raster file
#
#	Arguments:
#		att - attribute to load raster file into
#	Returns:
#		None
#	Side Effects:
#		None
###############################################################
proc ap_get_rasterfile {att} {
    global attPopup_Status attPopup_Type

    set new [create_map_browser .browse_rast_file surf 1]
    if { $new == "" } then { return }

    set attPopup_Type non_constant    
    set attPopup_Status $new
}

###############################################################
#	ap_check_invert -
#		A quick update routine called before we accept a mask
#
#	Arguments:
#		att - attribute we are modifying
#	Returns:
#		None
#	Side Effects:
#		None
###############################################################
proc ap_check_invert {att} {
    global attPopup_InvertMask attPopup_Type attPopup_Status
    
    if {"$attPopup_Status" == "mask not set"} then {
	return
    }

    if {"$att" == "mask"} then {
	set curr [Nget_current surf]
	Nsurf$curr set_mask_mode $attPopup_InvertMask
    }
}

###############################################################
#	ap_show_newconst -
#		Routine to change a constant in an attribute
#
#	Arguments:
#		att - which attribute to change 
#	Returns:
#		None
#	Side Effects:
#		None
###############################################################
proc ap_show_newconst {att} {
    global attPopup_Status attPopup_Type

    set curr [Nget_current surf]
    
    # Create a quick popup for entering a constant value
    set const [create_constant_popup .new_const 1]
    
    set attPopup_Status $const
    set attPopup_Type constant
}

###############################################################
#	ap_show_slideconst -
#		Routine to change a constant in an attribute
#               which only accepts a fixed range of values.
#
#	Arguments:
#		att - which attribute to change 
#	Returns:
#		None
#	Side Effects:
#		None
###############################################################
proc ap_show_slideconst {att} {
    global attPopup_Status attPopup_Type

    set curr [Nget_current surf]
    
    # Create a quick popup for entering a constant value
    set const [create_slideconstant_popup .new_const 1]
    
    set attPopup_Status $const
    set attPopup_Type constant
}

###############################################################
#	create_constant_popup -
#		Create a popup for entering a constant value
#
#	Arguments:
#		w - widget name to use for dialog
#		mode - true if grab focus
#	Returns:
#		The constant entered by the user
#	Side Effects:
#		None
###############################################################
proc create_constant_popup {{w .enter_constant} {mode 0}} {
    global cp_done
    
    set cp_done 0
    
    toplevel $w
    tkwait visibility $w

    puts "CONSTANT: $w MODE: $mode"
    label $w.title -bd 2 -text "Enter a constant value :" -relief raised
    entry $w.constant -bd 2 -relief sunken
    bind $w.constant <Return> "set cp_done 1"
    pack $w.title $w.constant -fill both 
    
    if {$mode} then {grab $w}
    
    tkwait variable cp_done
    set return_val [$w.constant get]
    destroy $w
    return $return_val
}

###############################################################
#	create_slideconstant_popup -
#		Create a popup for entering a constant value
#               Values are constrained by a slider from 0 to 255
#
#	Arguments:
#		w - widget name to use for dialog
#		mode - true if grab focus
#	Returns:
#		The constant entered by the user
#	Side Effects:
#		None
###############################################################
proc create_slideconstant_popup {{w .enter_constant} {mode 0}} {
    global cp_done
    
    set cp_done 0
    
    toplevel $w
    tkwait visibility $w

    puts "CONSTANT: $w MODE: $mode"
    label $w.title -bd 2 -text "Use slider to select constant value :" -relief raised
    scale $w.constant -from 0 -to 255 -showvalue yes \
	-orient horizontal -activebackground gray80 \
	-background gray90
    button $w.close -text "Done" -command "set cp_done 1"
    pack $w.title $w.constant $w.close -fill both 
    
    if {$mode} then {grab $w}
    
    tkwait variable cp_done
    set return_val [$w.constant get]
    destroy $w
    return $return_val
}










