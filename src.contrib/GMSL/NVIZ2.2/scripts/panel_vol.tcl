##########################################################################
##########################################################################
# Default Priority for this panel
# 
# priority is from 0 to 10
#  the lower the number, the quicker it will be bumped
#  10 cannot be bumped
#  Panels will be loaded by the greater of 5 or their current priority

##########################################################################

############################################################################
# procedure to make main control area
###########################################################################


proc mkvolPanel { BASE } {
    global Nv_
    
    catch {destroy $BASE}
    
    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} $BASE "Volume" 1 5]
    } else {
	set panel $Nv_($BASE)
    }
    frame $BASE -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Volume Panel"
    
    set tmp [frame $BASE.top]
    label $tmp.current -text "Current:" -anchor nw
    mkMapList $tmp.list vol 
    button $tmp.new -text New -anchor ne -command "add_map vol" 
    button $tmp.delete -text Delete -anchor ne -command "delete_map vol"

    pack $tmp.current -side left -expand 1
    pack $tmp.list -side left 
    pack $tmp.delete $tmp.new -side right -expand 1
    
    pack $tmp -side top -fill x -expand 1
    
    frame $BASE.f 
    button $BASE.f.close -text Close -command "Nv_closePanel $BASE" -anchor s
    pack $BASE.f.close -side right
    pack $BASE.f -side bottom -fill x -expand 1
    
    set curr [Nget_current vol]
    
    if {0 != $curr}  {
	set width [Nvol$curr get_att width]
	set maplist [Nget_map_list surf]
    } else {
	set width 1 
	set maplist {}
    }

    set tmp [frame $BASE.left]
    Nv_mkArrows $tmp.linewidth "Line Width" [concat set_width vol] $width
#	checkbutton $tmp.load -relief flat -text "Load to memory"
    button $tmp.color -text Color \
	-command "change_color vol $tmp.color"
    button $tmp.draw_current -text {Draw Current} \
	-command {Nvol_draw_one [Nget_current vol]}
    bind $tmp.color <Expose> \
	"$tmp.color configure -bg \[get_curr_sv_color vol\]"

    pack $tmp.linewidth $tmp.color $tmp.draw_current -anchor w \
	-padx 2 -pady 2 -side top -expand 1
    pack $tmp -side left  -fill y -expand 1

    set tmp [frame $BASE.right]
    label $tmp.label -text "Display on surface(s):"
    Nv_mkSurfacelist $tmp.list $maplist Nvol$curr vol
    pack $tmp.label $tmp.list -expand 1
    pack $tmp -side right -fill y -expand 1

    return $panel
}

# Reset procedure for this panel
proc Nviz_vol_reset {} {
    set vol_list [Nget_vol_list]

    foreach i $vol_list {
	Nvol$i delete
    }

    set_new_curr vol 0
}

# Save procedure for saving state of Nviz vol files
proc Nviz_vol_save {file_hook} {
    # For each vol file we write out all of its attribute information. 
    # Vectors are referenced by logical name so that they are reloadable
    # (otherwise, they may be assigned different id's each time they are loaded
    # and scripts won't work correctly).

    # Get the list of vol files
    set vol_list [Nget_vol_list]

    # Get the list of surfaces for checking draping
    set surf_list [Nget_surf_list]
    puts "Surf list is $surf_list"

    # Write out the total number of vol files
    puts $file_hook "[llength $vol_list]"

    # For each vol file write out the following:
    # 1. Logical name
    # 2. map name
    # 3. color
    # 4. width
    # 5. list of logical names of surfaces displayed on
    foreach i $vol_list {

	# logical name
	puts $file_hook "[Nvol$i get_logical_name]"
	
	# map name
	puts $file_hook "[Nvol$i get_att map]"

	# color
	puts $file_hook "[Nvol$i get_att color]"

	# width
	puts $file_hook "[Nvol$i get_att width]"

	# logical names of surfaces displayed on
	set draped [list]
	foreach j $surf_list {
	    if {[Nvol$i surf_is_selected Nsurf$j]} then {
		lappend draped $j
	    }
	}
	puts $file_hook "[llength $draped]"
	foreach j $draped {
	    puts $file_hook "[Nlogical_from_literal Nsurf$j]"
	}

	flush $file_hook
    }

    # Done...
}

# Load procedure for loading state of Nviz vol files
proc Nviz_vol_load { file_hook } {
    # Read the number of surfaces saved in this state file
    gets $file_hook num_vols

    # For each vol file, create a new surface with the given logical
    # name and fill in the attributes as appropriate
    for {set i 0} {$i < $num_vols} {incr i} {
	# Read in the logical name for this new vol map
	gets $file_hook logical_name

	# Now create a new vol map with the given logical name
	set new_vol [Nnew_map_obj vol "name=$logical_name"]

	# Set all attributes as appropriate (i.e. as they are read from the state file)
	
	# map
	gets $file_hook att_data
	$new_vol set_att map $att_data

	# color 
	gets $file_hook att_data
	$new_vol set_att color $att_data

	# width
	gets $file_hook att_data
	$new_vol set_att width $att_data

	# Select all the appropriate surfaces to put this map on
	gets $file_hook num_selected_surfs
	for {set j 0} {$j < $num_selected_surfs} {incr j} {
	    gets $file_hook selected_surf

	    # Select this surf by translating from a logical name and selecting
	    $new_vol select_surf [Nliteral_from_logical $selected_surf]
	}

	Nset_current vol [string range $new_vol 5 end]
    }

}


#proc change_color { type me } {
#    set curr [Nget_current $type]
#    switch $type {
#	"vect" { set head Nvect }
#	"site" { set head Nsite }
#    }
#
#    if {0 != $curr} {
#	set clr [$head$curr get_att color]
#	set clr [mkColorPopup .colorpop Color $clr 1]
#	$head$curr set_att color $clr
#    }
#
#    $me configure -bg [get_curr_sv_color $type]
#}
#
#proc get_curr_sv_color { type } {
#    set curr [Nget_current $type]
#    switch $type {
#	"vect" { set head Nvect }
#	"site" { set head Nsite }
#    }
#
#    if {0 == $curr} then {
#	return "gray90"
#    }
#
#    set color [$head$curr get_att color]
#
#    set color [expr int([tcl_to_rgb $color])]
#    set blue  [hexval [expr int($color & 0x0000ff)]]
#    set green [hexval [expr int(($color & 0x00ff00)>>8)]]
#    set red   [hexval [expr int(($color & 0xff0000)>>16)]]
#    return "#$red$green$blue"
#
#}
#
#proc delete_map {type} {
#    set curr [Nget_current $type]
#    switch $type {
#	"vect" { set head Nvect }
#	"site" { set head Nsite }
#    }
#    
#    if {0 != $curr} {
#	$head$curr delete
#	set name 0
#	
#	switch $type {
#	    "vect" { set new_list [Nget_vect_list] }
#	    "site" { set new_list [Nget_site_list] }
#	}
#	
#	if {[llength $new_list] != 0} then {
#	    set name [lindex $new_list 0]
#	} else {
#	    set name 0
#	}
#	
#	set_new_curr $type $name
#    }
#    
#}
#
## Use this routine when adding a vect or site in a script
#proc script_add_map { type map_name } {
#    set temp [Nnew_map_obj $type]
#    $temp set_att map $map_name
#    set_new_curr $type [string range $temp 5 end]
#    
#    return [string range $temp 5 end]
#}
#
#proc add_map {type} {
#    set new [create_map_browser .fbrowse $type 1]
#
#    # Let user know that we are busy
#    appBusy
#
#    if {$new != "-1"} {
#	set temp [Nnew_map_obj $type]
#	$temp set_att map $new
#	set_new_curr $type [string range $temp 5 end]
#    }
#
#    # Let user know that he may proceed
#    appNotBusy
#}
#
#proc set_width {type E} {
#    set i [$E get]
#    set curr [Nget_current $type]
#    switch $type {
#	"vect" { set head Nvect }
#	"site" { set head Nsite }
#    }
#    
#    if {0 != $curr} {
#	$head$curr set_att width $i
#    }
#}
