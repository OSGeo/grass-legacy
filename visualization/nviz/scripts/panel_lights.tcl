##########################################################################
# Default Priority for this panel
#
# priority is from 0 to 10
#  the lower the number, the quicker it will be bumped
#  10 cannot be bumped
#  Panels will be loaded by the greater of 5 or their current priority

##########################################################################

global Light

proc mklightsPanel { BASE } {
    global Nv_

    set Nv_(FollowView) 0
    set Nv_(ShowModel) 1
    set Nv_(LIGHT_XY) $BASE.bottom.xy
    set Nv_(LIGHT_HGT) $BASE.bottom.height
    set Nv_(LIGHT_BASE) $BASE
    
    catch {destroy $BASE}
    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
        set panel [St_create {window name size priority} $BASE "Lighting" 2 5]
    } else {
	set panel $Nv_($BASE)
    }
    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Lighting Panel"
    
    frame $BASE.top 
    frame $BASE.bottom
    frame $BASE.top.right
    frame $BASE.top.left 
    
    # Set up a binding so we do ShowModel correctly
    bind $BASE <Map> {if {$Nv_(FollowView) == 0} { do_light_draw } }

    checkbutton $BASE.top.left.follow -relief flat -text "Follow ViewPoint" \
	-variable Nv_(FollowView) -command follow -onvalue 1 -offvalue 0
    checkbutton $BASE.top.left.show -relief flat -text "Show Model" \
	-variable Nv_(ShowModel)
    Nv_mkScale $BASE.top.left.bright h Brightness 100 0 80 set_brt 2 
    Nv_mkScale $BASE.top.left.ambient h Ambient 100 0 20 set_amb 2
    pack $BASE.top.left.follow $BASE.top.left.show \
        $BASE.top.left.bright $BASE.top.left.ambient \
	-side top -fill x -expand 1
    
    Nv_mkScale $BASE.top.right.red h Red 100 0 100 set_red 2
    Nv_mkScale $BASE.top.right.green h Green 100 0 100 set_green 2
    Nv_mkScale $BASE.top.right.blue h Blue 100 0 100 set_blue 2
    pack $BASE.top.right.red $BASE.top.right.green $BASE.top.right.blue \
	-side top -expand 1
    
    pack \
    [Nv_mkXYScale $BASE.bottom.xy puck LIGHT_POS 125 125 105 105 \
	set_lgt_position unset_follow]  \
    -side right -expand 1

    pack [Nv_mkScale $BASE.bottom.height v Height 100 0 80 set_lgt_hgt 2] \
    -side left -expand 1 -fill y

    pack $BASE.top.left -side left -expand 1
    pack $BASE.top.right -side right -expand 1
    pack $BASE.top -side top -fill x -expand 1
    pack $BASE.bottom -side top -fill x -expand 1

    button $BASE.close -text Close -command "Nv_closePanel $BASE" -anchor s
    pack $BASE.close -side right
    return $panel
}

# Reset procedure for lights panel
proc Nviz_lights_reset {} {
    global Light Nv_
   
    set light_pos {{0.68 -0.68 0.8 0} {0.0 0.0 1.0 0}} 

    # Reset attributes of both lights
    for {set i 1} {$i < 3} {incr i} {
	if {$i == 1} then {
		set brt 0.8
		set amb 0.2
	} else {
		set brt 0.5
		set amb 0.3
	}

	$Light($i) set_ambient $amb $amb $amb
	$Light($i) set_bright $brt
	$Light($i) set_color 1.0 1.0 1.0

	set pos_data [lindex $light_pos [expr $i - 1]]
	$Light($i) set_position [lindex $pos_data 0] [lindex $pos_data 1] [lindex $pos_data 2] [lindex $pos_data 3]
    }
    
    set Nv_(FollowView) 0
    set Nv_(ShowModel)  1

    update_light_panel
}

# Save procedure for saving state of Nviz lights
proc Nviz_lights_save {file_hook} {
    global Light Nv_

    # Fairly straightforward, save the attributes of the two lights
    # plus the status of the panel
    for {set i 1} {$i < 3} {incr i} {
	puts $file_hook "[$Light($i) get_ambient]"
	puts $file_hook "[$Light($i) get_bright]"
	puts $file_hook "[$Light($i) get_color]"
	puts $file_hook "[$Light($i) get_position]"
    }
    
    puts $file_hook "$Nv_(FollowView)"
    puts $file_hook "$Nv_(ShowModel)"
}

# Load procedure for loading state of Nviz lights
proc Nviz_lights_load {file_hook} {
    global Light Nv_

    # Fairly straightforward, load the attributes of the two lights
    # plus the status of the panel
    for {set i 1} {$i < 3} {incr i} {
	gets $file_hook amb_data
	set amb_data [split $amb_data]
	$Light($i) set_ambient [lindex $amb_data 0] [lindex $amb_data 1] [lindex $amb_data 2]

	gets $file_hook brt_data
	$Light($i) set_bright $brt_data

	gets $file_hook col_data
	set col_data [split $col_data]
	$Light($i) set_color [lindex $col_data 0] [lindex $col_data 1] [lindex $col_data 2]

	gets $file_hook pos_data
	set pos_data [split $pos_data]
	$Light($i) set_position [lindex $pos_data 0] [lindex $pos_data 1] [lindex $pos_data 2] [lindex $pos_data 3]
    }
    
    gets $file_hook Nv_(FollowView)    
    gets $file_hook Nv_(ShowModel)

    update_light_panel
}

# Quicky procedure to update the light panel based
# on the current settings of light attributes
proc update_light_panel {} {
    global Nv_ Light

    set BASE $Nv_(LIGHT_BASE)

    # brightness
    set val [$Light(1) get_bright]
    Nv_scaleCallback $BASE.top.left.bright b 2 null [expr int($val * 100)]

    # ambient
    set val [$Light(1) get_ambient]
    set val [lindex $val 0]
    Nv_scaleCallback $BASE.top.left.ambient b 2 null [expr int($val * 100)]

    # color
    set val [$Light(1) get_color]
    Nv_scaleCallback $BASE.top.right.red b 2 null [expr int([lindex $val 0] * 100)]
    Nv_scaleCallback $BASE.top.right.green b 2 null [expr int([lindex $val 1] * 100)]
    Nv_scaleCallback $BASE.top.right.blue b 2 null [expr int([lindex $val 2] * 100)]

    # height + XY position (puck)
    set val [$Light(1) get_position]
    set x [expr [lindex $val 0] / 2.0 + 0.5]
    set y [expr [lindex $val 1] / -2.0 + 0.5]
    set z [lindex $val 2]
    Nv_scaleCallback $BASE.bottom.height b 2 null [expr int($z * 100)]
    Nv_itemDrag $BASE.bottom.xy $Nv_(LIGHT_POS) [expr int(125 * $x)] [expr int(125 * $y)]

}

proc do_light_draw {} {
    global Nv_
    
    if {$Nv_(ShowModel)} {
            Nset_draw front
            Nready_draw
            Ndraw_model
            Ndone_draw
            Nset_draw back
    }
}

proc set_red {val} {set_lgt_color r $val}
proc set_green {val} {set_lgt_color g $val}
proc set_blue {val} {set_lgt_color b $val}

proc set_lgt_color {c val} {
    global Light

    set c_list [$Light(1) get_color]

    if {[llength $c_list] == 3} {
        if {$c == "r"} {set r $val} else {set r [lindex $c_list 0]}
        if {$c == "g"} {set g $val} else {set g [lindex $c_list 1]}
        if {$c == "b"} {set b $val} else {set b [lindex $c_list 2]}
        $Light(1) set_color $r $g $b

        do_light_draw
    } else { return -code error "light colors not set $c_list"}

}

proc set_amb {a} {
    global Light

    $Light(1) set_ambient $a $a $a
#    $Light(2) set_ambient $a $a $a

    do_light_draw
}
proc set_brt {b} {
    global Light

    $Light(1) set_bright $b

    do_light_draw
}
proc set_lgt_position {x y} {
    global Light
    global Nv_

    set list [$Light(1) get_position]
    if {[llength $list] != 4} {return -code error "unable to get old position"}
    # origin at top right
    set x [expr -1.0 + 2.0*$x]
    set y [expr -1.0 + 2.0*(1.0 - $y)]
    set z [lindex $list 2]
    set w [lindex $list 3]
    $Light(1)  set_position $x $y  $z $w
    if {$Nv_(FollowView) == 0} { do_light_draw}
}
proc set_lgt_hgt  {z} {  
    global Light
    global Nv_


    set list [$Light(1)  get_position]
    if {[llength $list] != 4} {return -code error "unable to get old position"}
    set x [lindex $list 0]
    set y [lindex $list 1]
    set w [lindex $list 3]
    $Light(1)  set_position $x $y  $z $w
    if {$Nv_(FollowView) == 0} { do_light_draw}
}

proc follow {} {
    global Nv_

    if {$Nv_(FollowView)} { Nset_light_to_view}
}
proc unset_follow {args} {
    global Nv_
    $Nv_(LIGHT_BASE).top.left.follow deselect
}
