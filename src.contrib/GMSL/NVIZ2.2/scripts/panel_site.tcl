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


proc mksitePanel { BASE } {
    global Nv_
    
    catch {destroy $BASE}
    
    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} $BASE "Sites" 1 5]
    } else {
	set panel $Nv_($BASE)
    }

    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Site Panel"

    set tmp [frame $BASE.top]
    label $tmp.current -text "Current:" -anchor nw
    mkMapList $tmp.list site 
    button $tmp.new  -text New -anchor ne -command "add_map site"
    button $tmp.delete  -text Delete -anchor ne -command "delete_map site"
    
    set curr [Nget_current site]
    
    if {0 != $curr}  {
	set width [Nsite$curr get_att width]
	set size [Nsite$curr get_att size]
	set Nv_(siteshape) [Nsite$curr get_att marker]
	set maplist [Nget_map_list surf]
	set longdim [expr int([Nget_longdim]) / 50 ]

	# We do this check to make sure that size is ALWAYS set to
	# something sensible the first time the surface is loaded
	if {$size == "0"} then {
	    Nsite$curr set_att size [expr $longdim / 10.0]
	    set size [Nsite$curr get_att size]
	}
	if {"$Nv_(siteshape)" == ""} then {
	    Nsite$curr set_att marker x
	    set Nv_(siteshape) [Nsite$curr get_att marker]
	}
	if {$width == "0"} then {
	    Nsite$curr set_att width 2
	    set width [Nsite$curr get_att width]
	}
    } else {
	set width 1
	set size 10
	set Nv_(siteshape) x
	set Nv_(sitedisplay) 3d
	set maplist {}
	set longdim 100
    }
    
    pack $tmp.current -side left  -expand 1
    pack $tmp.list -side left 
    pack $tmp.delete $tmp.new -side right -expand 1
    
    pack $tmp -side top -fill x -expand 1
    
    frame $BASE.f 
    button $BASE.f.close -text Close \
	-command "Nv_closePanel $BASE" -anchor s
    button $BASE.f.color -text Color -anchor s \
	-command "change_color site $BASE.f.color"
    button $BASE.f.draw_current -text {Draw Current} -anchor s \
	-command {Nsite_draw_one [Nget_current site]}
    bind $BASE.f.color <Expose> \
	"$BASE.f.color configure -bg \[get_curr_sv_color site\]"

    pack $BASE.f.close -side right
    pack $BASE.f.color $BASE.f.draw_current -side left -expand 1
    pack $BASE.f -side bottom -fill x -expand 1
    
    set tmp [frame $BASE.left]    
    Nv_mkFloatScale $tmp.sitesize h "site size" $longdim \
	0 $size change_site_size 2
    Nv_mkArrows $tmp.linewidth "Line Width" [concat set_width site] $width 
    
    frame $tmp.siteshape
    
    radiobutton $tmp.siteshape.x -relief flat -text "use X" -value x \
	-anchor nw -variable Nv_(siteshape) -command change_marker
    radiobutton $tmp.siteshape.sphere -relief flat -text "use sphere" \
	-value sphere -anchor nw -variable Nv_(siteshape) \
	-command change_marker
    radiobutton $tmp.siteshape.diamond -relief flat -text "use diamond" \
	-value diamond -anchor nw -variable Nv_(siteshape) \
	-command change_marker
    
    pack $tmp.siteshape.x $tmp.siteshape.sphere $tmp.siteshape.diamond  \
	-fill x -expand 1
    pack $tmp.sitesize $tmp.linewidth $tmp.siteshape \
	-side top -expand 1 -fill x
    pack $tmp -side left  -fill y -expand 1
    
    set tmp [frame $BASE.right]
    
    frame $tmp.sitedisp
    radiobutton  $tmp.sitedisp.threed -text "3D Sites" \
	-anchor nw -variable Nv_(sitedisplay) -value 3d \
	-command change_site_mode
    radiobutton  $tmp.sitedisp.surfdisp -text "Display on surface(s):" \
	-anchor nw -variable Nv_(sitedisplay) -value surfdisp \
	-command change_site_mode
    
    Nv_mkSurfacelist $tmp.list $maplist Nsite$curr site
    pack $tmp.sitedisp.threed $tmp.sitedisp.surfdisp -fill x 
    pack $tmp.sitedisp $tmp.list  -side top
    pack $tmp -side right -fill y -expand 1
    
    return $panel
}

# Reset procedure for this panel
proc Nviz_site_reset {} {
    set site_list [Nget_site_list]

    foreach i $site_list {
	Nsite$i delete
    }

    set_new_curr site 0
}

# Save procedure for saving state of Nviz site files
proc Nviz_site_save {file_hook} {
    # For each site file we write out all of its attribute information. 
    # Sitess are referenced by logical name so that they are reloadable
    # (otherwise, they may be assigned different id's each time they are loaded
    # and scripts won't work correctly).

    # Get the list of site files
    set site_list [Nget_site_list]

    # Get the list of surfaces for checking draping
    set surf_list [Nget_surf_list]

    # Write out the total number of site files
    puts $file_hook "[llength $site_list]"

    # For each site file write out the following:
    # 1. Logical name
    # 2. map name
    # 3. color
    # 4. width
    # 5. list of logical names of surfaces displayed on
    # 6. marker
    # 7. size
    # 8. useatt
    # 9. display
    foreach i $site_list {

	# logical name
	puts $file_hook "[Nsite$i get_logical_name]"
	
	# map name
	puts $file_hook "[Nsite$i get_att map]"

	# color
	puts $file_hook "[Nsite$i get_att color]"

	# width
	puts $file_hook "[Nsite$i get_att width]"

	# logical names of surfaces displayed on
	set draped [list]
	foreach j $surf_list {
	    if {[Nsite$i surf_is_selected Nsurf$j]} then {
		lappend draped $j
	    }
	}
	puts $file_hook "[llength $draped]"
	foreach j $draped {
	    puts $file_hook "[Nlogical_from_literal Nsurf$j]"
	}

	# marker
	puts $file_hook "[Nsite$i get_att marker]"
	
	# size
	puts $file_hook "[Nsite$i get_att size]"

	# useatt
	puts $file_hook "[Nsite$i get_att useatt]"

	# display
	puts $file_hook "[Nsite$i get_att display]"

	flush $file_hook
    }

    # Done...
}

# Load procedure for loading state of Nviz site files
proc Nviz_site_load { file_hook } {
    # Read the number of  sites saved in this state file
    gets $file_hook num_sites

    # For each site file, create a new site map object with the given
    # logical name and fill in the attributes as appropriate
    for {set i 0} {$i < $num_sites} {incr i} {
	# Read in the logical name for this new site map
	gets $file_hook logical_name

	# Now create a new site map with the given logical name
	set new_site [Nnew_map_obj site "name=$logical_name"]

	# Set all attributes as appropriate (i.e. as they are read from the state file)
	
	# map
	gets $file_hook att_data
	$new_site set_att map $att_data

	# color 
	gets $file_hook att_data
	$new_site set_att color $att_data

	# width
	gets $file_hook att_data
	$new_site set_att width $att_data

	# Select all the appropriate surfaces to put this map on
	gets $file_hook num_selected_surfs
	for {set j 0} {$j < $num_selected_surfs} {incr j} {
	    gets $file_hook selected_surf

	    # Select this surf by translating from a logical name and selecting
	    $new_site select_surf [Nliteral_from_logical $selected_surf]
	}

	# marker
	gets $file_hook att_data
	$new_site set_att marker $att_data

	# size
	gets $file_hook att_data
	$new_site set_att size $att_data

	# useatt
	gets $file_hook att_data
	$new_site set_att useatt $att_data

	# display
	gets $file_hook att_data
	$new_site set_att display $att_data

	Nset_current site [string range $new_site 5 end]
    }

}


proc change_marker {} {
    global Nv_
    
    set curr [Nget_current site]
    if {0 != $curr} {
	Nsite$curr set_att marker $Nv_(siteshape)
    }
}

proc change_site_mode {} {
    global Nv_
    
    set curr [Nget_current site]
    if {0 != $curr} {
	if {![Nsite$curr set_att display $Nv_(sitedisplay)]} then {
	    set Nv_(sitedisplay) surfdisp
	}
    }
}

proc change_site_size {size} {
    set curr [Nget_current site]
    if {0 != $curr} {
	Nsite$curr set_att size $size
    }
}







