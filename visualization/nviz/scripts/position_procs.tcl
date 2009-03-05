proc look_here {W x y} {
    global Nv_ WhatsHere

    set WhatsHere(on) 0
    set y [expr $Nv_(height) - $y]
    Nlook_here $x $y
    inform  "New center of view has been set"
    Nquick_draw
    bind $W <Button> {}
}

proc look_center {{M 0}} {

    if {$M == 0} { set M [Nget_current surf]}
    Nset_focus_map surf $M
    inform  "New center of view has been set"
    Nquick_draw
}

proc no_focus {} {
    Nset_focus_state 0
}

##############################
# Click on canvas to reCenter
# C2C code inspired from panel_pos.tcl and panel_rquery.tcl
proc C2C { on } {
	global Nv_

	if {$on} {
		change_display 0
		bind $Nv_(TOP).canvas <Button> {click_recenter %x %y}
		bind $Nv_(TOP).canvas <MouseWheel> {scroll_zoom %D}
		if {[tk windowingsystem] eq {x11}} {
			bind $Nv_(TOP).canvas <Button-4> {scroll_zoom  1}
			bind $Nv_(TOP).canvas <Button-5> {scroll_zoom -1}
		}
	} else {
		bind $Nv_(TOP).canvas <Button> {}
		bind $Nv_(TOP).canvas <MouseWheel> {}
		bind $Nv_(TOP).canvas <Button-4> {}
		bind $Nv_(TOP).canvas <Button-5> {}
	}
	inform "Click to change scene center position, scroll to zoom in and out"
}

# Zoom in and out with mouse scroller
proc scroll_zoom { amount } {
	global Nv_
	global Nauto_draw
	
	set fov [Nget_fov]
	if {[tk windowingsystem] eq "win32"} {
		set newzoom [expr {(-$amount/120)+$fov}]
	} else {
		set newzoom [expr {(-1*$amount)+$fov}]
	}
	
	# Field of view is limited to range 1..120
	if { $newzoom < 1   } { set newzoom 1 }
	if { $newzoom > 120 } { set newzoom 120 }
	
	Nv_scaleCallback $Nv_(main_BASE).bframe.cframe.pers b 0 Nchange_persp $newzoom
	inform "Perspective has been changed"
	if {$Nauto_draw == 1} {
		Ndraw_all
	}
	update
}

# Move camera and center according to mouse click on canvas
# Camera position is not rotated but just shifted by old/new center deltaX and deltaY
proc click_recenter {cx cy} {
	global Nauto_draw
	global Nv_
	
	# Get real position
	set cy [expr $Nv_(height) - $cy]
	set list [Nget_point_on_surf $cx $cy]
	if {[llength $list] < 4} {
		inform "Point not on surface"
		return
	}
	set cx [lindex $list 0]
	set cy [lindex $list 1]
	
	# Calculate deltas for shifting
	set from_loc [Nget_real_position 1]
	set to_loc [Nget_real_position 2]
	set dx [ expr { $cx - [lindex $to_loc 0] } ]
	set dy [ expr { $cy - [lindex $to_loc 1] } ]
	set fe [ expr { $dx + [lindex $from_loc 0] }]
	set fn [ expr { $dy + [lindex $from_loc 1] }]
	
	Nset_focus_real $cx $cy [lindex $to_loc 2]	
	Nmove_to_real $fe $fn [lindex $from_loc 2]
	
	inform  "New center of view has been set"
	move_position
	if {$Nauto_draw == 1} {Ndraw_all}
	update
}

