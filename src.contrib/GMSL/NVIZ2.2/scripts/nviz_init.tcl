global Init
global Light

set Init 0

proc init_graphics {} {
    
	global Init
	global Light

	if {$Init == 0} {
		Nset_fov 3
		Ninit_view
		Nset_focus_map
		set $Init 1
	}
	set p [expr 20.0/125.0]
	Nchange_position $p $p

	set Light(1) [Nnew_light]
	$Light(1) set_position -1.0 1.0 1.0 0
	$Light(1) set_color  0.8 0.8 0.8 
	$Light(1) set_ambient 0.2 0.2 0.2 

	set Light(2) [Nnew_light]
	$Light(2) set_position 0.0 0.0 1.0 0
	$Light(2) set_color  0.25 0.25 0.25 
	$Light(2) set_ambient 0.1 0.1 0.1 
}
