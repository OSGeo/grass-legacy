# mkIsosurfAttPopup w name att
#
# Create a dialog box change current isosurface attribute
#
# Arguments:
#    w - Name to use for new top-level window.
#    id  - Isosurface id
#    att  - Current Att
#    mode - Disable events in other windows ?

#If set to 1 output debug statements
global DEBUG
set DEBUG 0

proc mkIsosurfAttPopup {w id att {mode 0}} {

    global attIsoPopup_InvertMask attIsoPopup_Status attIsoPopup_Type \
    attIsoPopup_Color attIsoPopup_UseColor

    # set current volume
    set curr [Nget_current vol]
    if {$curr == 0} return

    # legal isosurface id
    if {$id == -1} return

    catch {destroy $w}
    toplevel $w -class Dialog
    tkwait visibility $w

    wm positionfrom $w program
    wm sizefrom $w program
    wm title $w "Change Attribute"
    wm iconname $w "Attribute"
    wm geometry $w ""
    wm maxsize $w 400 400
    wm minsize $w 50 100

    # Callbacks depend on the attribute
    switch "$att" {
    threshold {
        set cb1 "aip_show_newconst threshold"
        set cb2 "aip_show_newconst threshold"
    }
    color    {
        set cb1 "aip_get_rasterfile color"
        set cb2 "aip_get_colorconst"
    }
    mask {
        set cb1 "aip_get_rasterfile mask"
        set cb2 "aip_remove_mask"
    }
    transparency {
        set cb1 "aip_get_rasterfile transp"
        set cb2 "aip_show_slideconst transp"
    }
    shininess    {
        set cb1 "aip_get_rasterfile shin"
        set cb2 "aip_show_slideconst shin"
    }
    emission {
        set cb1 "aip_get_rasterfile emi"
        set cb2 "aip_show_slideconst emi"
    }
    }

    # popup frames
    frame $w.f1 -relief raised -border 1
    frame $w.f2 -relief raised -border 1
    frame $w.f3 -relief raised -border 1
    frame $w.f4 -relief raised -border 1
    pack $w.f1 $w.f2 $w.f3 $w.f4 -side top -fill both -expand yes

    # label
    label $w.f1.name -text "Change Attribute: $att"
    pack $w.f1.name -side top -fill both -expand yes

    # left button
    button $w.f2.map -text "New Map" -command "$cb1"

    # right button
    if {"$att" == "mask"} then {
    button $w.f2.const -text "Remove Mask" -command "$cb2"
    checkbutton $w.f2.invert -text "Invert Mask" -onvalue 1 \
        -offvalue 0 -variable attIsoPopup_InvertMask
    set attIsoPopup_InvertMask [Nvol$curr isosurf get_mask_mode $id]
    } elseif {"$att" == "threshold" } then {
    button $w.f2.const -text "New Constant"    -command "$cb2"
    checkbutton $w.f2.use_color -text "Use volume as color" -onvalue 1 \
        -offvalue 0 -variable attIsoPopup_UseColor
    set attIsoPopup_UseColor 1
    } else {
    button $w.f2.const -text "New Constant"    -command "$cb2"
    }

    # pack buttons
    if {"$att" == "threshold"} then {
    pack $w.f2.const -side left -fill both -expand yes
    pack $w.f2.use_color -side bottom -expand yes -fill both -before $w.f2.const
    } elseif {"$att" == "mask"} then {
    pack $w.f2.map $w.f2.const -side left -fill both -expand yes
    pack $w.f2.invert -side bottom -expand yes -fill both -before $w.f2.map
    } else {
    pack $w.f2.map $w.f2.const -side left -fill both -expand yes
    }

    # set current isosurface status
    label $w.f3.status -text "Status: "
    set_isosurf_status $id $att
    label $w.f3.info -textvariable attIsoPopup_Status
    pack $w.f3.status $w.f3.info -side top -fill both  -expand yes

    # Accep, Cancel buttons
    button $w.f4.accept -text "Accept" \
    -command "aip_check_invert $id $att ; destroy $w"
    button $w.f4.cancel -text "Cancel" \
    -command "set attIsoPopup_Status \"no_change\" ; destroy $w"
    pack $w.f4.accept $w.f4.cancel -side left -fill both -expand yes

    bind $w <Any-Enter> [list focus $w]
    focus $w
    if {$mode} {grab $w}

    # tkspecial_wait window $w
    tkwait window $w
    if {$DEBUG} {puts "Exiting from attIsoPopup"}

    # Signal to user that we are busy
    appBusy

    # Invoke the changes (or lack thereof)
    if { ("$attIsoPopup_Status" != "no_change") } then {
    if { ("$attIsoPopup_Status" != "$att not set") } then {
        if {"$attIsoPopup_Type" != "constant"} then {
        switch "$att" {
            color {
            Nvol$curr isosurf set_att $id color $attIsoPopup_Status
            }
            mask {
            Nvol$curr isosurf set_att $id mask $attIsoPopup_Status
            }
            transparency {
            Nvol$curr isosurf set_att $id transp $attIsoPopup_Status
            }
            shininess    {
            Nvol$curr isosurf set_att $id shin $attIsoPopup_Status
            }
            emission {
            Nvol$curr isosurf set_att $id emi $attIsoPopup_Status
            }
        }
        } else {
        switch "$att" {
            threshold {
            Nvol$curr isosurf set_att $id threshold constant $attIsoPopup_Status
            if {$attIsoPopup_UseColor} then {
                set volname [Nget_map_name $curr vol]
                Nvol$curr isosurf set_att $id color $volname
            }
            }
            color {
            Nvol$curr isosurf set_att $id color constant $attIsoPopup_Color
            }
            mask {
            Nvol$curr isosurf set_att $id mask constant 0
            Nvol$curr isosurf unset_att $id mask
            }
            transparency {
            Nvol$curr isosurf set_att $id transp constant $attIsoPopup_Status
            }
            shininess {
            Nvol$curr isosurf set_att $id shin constant $attIsoPopup_Status
            }
            emission {
            Nvol$curr isosurf set_att $id emi constant $attIsoPopup_Status
            }
        }
        }
    } else { Nvol$curr isosurf unset_att $id $att }
    }

    # Show that we are no longer busy
    appNotBusy
}

###############################################################
#    set_isosurf_status -
#        Routine to get a current status string
#
#    Arguments:
#        id - isosurface id
#        att - attribute to load raster file into
#    Returns:
#        current status string
#    Side Effects:
#        None
###############################################################
proc set_isosurf_status {id att} {
    global attIsoPopup_Color attIsoPopup_Type attIsoPopup_Status

    set curr [Nget_current vol]
    set map_pair [Nvol$curr isosurf get_att $id $att]

    set att_type [lindex $map_pair 0]
    set att_value [lindex $map_pair 1]

    set attIsoPopup_Color 0

    if {$att_type == "unset"} then {
        set attIsoPopup_Status "$att not set"
    } elseif {$att_type == "map"} then {
        set attIsoPopup_Type non_constant
        set attIsoPopup_Status $att_value
    } else {
        set attIsoPopup_Type constant
        if {"$att" == "color"} then {
            set tmp [expr int($att_value)]

            set red   [expr int($tmp & 0x0000ff)]
            set green [expr int(($tmp & 0x00ff00)>>8)]
            set blue  [expr int(($tmp & 0xff0000)>>16)]

            set attIsoPopup_Color [expr ($red<<16) + ($green<<8) + $blue]
            set attIsoPopup_Status "R$red, G$green, B$blue"
        } else {
            set attIsoPopup_Status $att_value
        }
    }
}

###############################################################
#     aip_get_rasterfile -
#        Routine to get a new raster file
#
#    Arguments:
#        att - attribute to load raster file into
#    Returns:
#        None
#    Side Effects:
#        None
###############################################################
proc aip_get_rasterfile {att} {
    global attIsoPopup_Status attIsoPopup_Type

    set new [create_map_browser .browse_rast_file vol 1]
    if { $new == "-1" } then { return }

    set attIsoPopup_Type non_constant
    set attIsoPopup_Status $new
}

###############################################################
#    aip_remove_mask -
#        Routine to remove a mask from a surface
#
#    Arguments:
#        None
#    Returns:
#        None
#    Side Effects:
#        None
###############################################################
proc aip_remove_mask {} {
    global attIsoPopup_Status attIsoPopup_Type

    set attIsoPopup_Status "remove mask"
    set attIsoPopup_Type constant
}

###############################################################
#    aip_check_invert -
#        A quick update routine called before we accept a mask
#
#    Arguments:
#        att - attribute we are modifying
#    Returns:
#        None
#    Side Effects:
#        None
###############################################################
proc aip_check_invert {id att} {
    global attIsoPopup_InvertMask attIsoPopup_Type attIsoPopup_Status

    if {"$attIsoPopup_Status" == "mask not set"} then {
    return
    }

    if {"$att" == "mask"} then {
    set curr [Nget_current vol]
    Nvol$curr isosurf set_mask_mode $id $attIsoPopup_InvertMask
    }
}

###############################################################
#    aip_show_newconst -
#        Routine to change a constant in an attribute
#
#    Arguments:
#        att - which attribute to change
#    Returns:
#        None
#    Side Effects:
#        None
###############################################################
proc aip_show_newconst {att} {
    global attIsoPopup_Status attIsoPopup_Type

    # Create a quick popup for entering a constant value
    set const [create_constant_popup .new_const 1]

    set attIsoPopup_Status $const
    set attIsoPopup_Type constant
}

###############################################################
#    aip_show_slideconst -
#        Routine to change a constant in an attribute
#               which only accepts a fixed range of values.
#
#    Arguments:
#        att - which attribute to change
#    Returns:
#        None
#    Side Effects:
#        None
###############################################################
proc aip_show_slideconst {att} {
    global attIsoPopup_Status attIsoPopup_Type

    # Create a quick popup for entering a constant value
    set const [create_slideconstant_popup .new_const 1]

    set attIsoPopup_Status $const
    set attIsoPopup_Type constant
}

###############################################################
#    aip_get_colorconst -
#        Routine to get a color constant (i.e. popup the color dialog)
#
#    Arguments:
#        None
#    Returns:
#        None
#     Side Effects:
#        None
###############################################################
proc aip_get_colorconst {} {
    global attIsoPopup_Status attIsoPopup_Type attIsoPopup_Color

    set curr_color $attIsoPopup_Color
    set red   [hexval [expr ($curr_color & 0xff0000)>>16]]
    set green [hexval [expr ($curr_color & 0x00ff00)>>8]]
    set blue  [hexval [expr ($curr_color & 0x0000ff)]]
    set curr_color "#"
    append curr_color $red $green $blue

    set new_color [mkColorPopup .color_browse "Isosurface Color" $curr_color 1]
    if {$new_color == "-1"} return

    # Finally pass the constant on to change the surface
    set new_color [expr [tcl_to_rgb $new_color] + 0]
    set red   [expr int(($new_color & 0xff0000)>>16)]
    set green [expr int(($new_color & 0x00ff00)>>8)]
    set blue  [expr int($new_color & 0x0000ff)]

    set attIsoPopup_Status "R$red, G$green, B$blue"
    set attIsoPopup_Color $new_color
    set attIsoPopup_Type constant
}

###############################################################
#    create_constant_popup -
#        Create a popup for entering a constant value
#
#    Arguments:
#        w - widget name to use for dialog
#        mode - true if grab focus
#    Returns:
#        The constant entered by the user
#    Side Effects:
#        None
###############################################################
proc create_constant_popup {{w .enter_constant} {mode 0}} {
    global cp_done

    set cp_done 0

    toplevel $w
    tkwait visibility $w

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
#    create_slideconstant_popup -
#        Create a popup for entering a constant value
#               Values are constrained by a slider from 0 to 255
#
#    Arguments:
#        w - widget name to use for dialog
#        mode - true if grab focus
#    Returns:
#        The constant entered by the user
#    Side Effects:
#        None
###############################################################
proc create_slideconstant_popup {{w .enter_constant} {mode 0}} {
    global cp_done

    set cp_done 0

    toplevel $w
    tkwait visibility $w

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
