# $Id$
#***************************************************************
#*
#* MODULE:       panel_pick.tcl 1.0
#*
#* AUTHOR(S):    ACS - Massimo Cuomo - m.cuomo at acsys.it
#*
#* PURPOSE:		 Picking of site elements for showing associated
#*					DB content. Site map must be enabled with
#*					pick_add_map (and removed with (pick_remove_map)
#*
#* REQUIREMENTS: ACS_utils.tcl
#*
#* COPYRIGHT:    (C) 2005 by the ACS / GRASS Development Team
#*
#*               This program is free software under the
#*               GNU General Public License (>=v2).
#*               Read the file COPYING that comes with GRASS
#*               for details.
#*
#**************************************************************

global ___ACS_utils___
if {![info exists ___ACS_utils___]} {
	source $default_panel_path/ACS_utils.tcl
}

proc mkpickPanel { BASE } {
	global Nv_

    catch {destroy $BASE}

    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} \
		       $BASE "Pick" 1 5]
    } else {
	set panel $Nv_($BASE)
    }


    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Pick"

    frame $BASE.top
    frame $BASE.bottom -bd 2 -relief groove
    frame $BASE.top2

    label $BASE.top.label -text " "
    pack $BASE.top.label -side left -fill y -pady 4


	button $BASE.bottom.addremovemap -text "Add/Remove Map to pick" -command "pick_GUI_addremovemap"
    pack $BASE.bottom.addremovemap -side left

	button $BASE.bottom.close -text Close -command "Nv_closePanel $BASE"
    pack $BASE.bottom.close -side right

    pack $BASE.top $BASE.top2 $BASE.bottom \
    -expand 1 -fill both -side top

	pick_init
	pick_init_GUI $BASE.top

	return $panel
}

###########################################################################################
# pick
###########################################################################################

proc pick_init_GUI {BASE} {
	global Nv_

	frame $BASE.top

		label $BASE.top.l -text "pick" -borderwidth 1 -relief flat
		pack $BASE.top.l  -side left -pady 5 -expand 1

		set Nv_(PICK) 0
		checkbutton $BASE.top.on -variable Nv_(PICK) \
				-command "pick_set Nv_(PICK) $Nv_(TOP).canvas"
		pack $BASE.top.on -side left -expand 1

		frame $BASE.top.maxdist

			label $BASE.top.maxdist.l -text "maxdist" -borderwidth 1 -relief flat
			pack $BASE.top.maxdist.l  -side left -expand 1

			set Nv_(PICK_MAXDIST) \
				[entry $BASE.top.maxdist.e -relief sunken -width 6 -justify right]
			$BASE.top.maxdist.e  insert 0 10000
			pack $BASE.top.maxdist.e -side left -expand 1

		pack $BASE.top.maxdist -side left -expand 1 -pady 3 -padx 5

	pack $BASE.top -side left -expand 1 -pady 3 -padx 2

	frame $BASE.hl

		label $BASE.hl.l -text "show hyperlink" -borderwidth 1 -relief flat
		pack $BASE.hl.l  -side left -expand 1

		rc_load_res "hyperlink.display.maxNumber" maxnumber 20

		set Nv_(PICK_SHOW_HL) 0
		checkbutton $BASE.hl.show \
				-variable Nv_(PICK_SHOW_HL) \
				-command "pick_highlight_hyperlinks Nv_(PICK_SHOW_HL) $Nv_(TOP).canvas [expr $maxnumber - 1]"
		pack $BASE.hl.show -side left -expand 1

	pack $BASE.hl -side left -expand 1 -pady 3 -padx 2
}

proc pick_panel_get_Nv_height {} {global Nv_; return $Nv_(height)}

proc pick_panel_get_pick_maxdist {} {
	global Nv_
	set maxdist [$Nv_(PICK_MAXDIST) get]
	if {[not_a_number $maxdist]} {
		set maxdist 10000
		$Nv_(PICK_MAXDIST) delete 0 end
		$Nv_(PICK_MAXDIST) insert 0 10000
	}
	return $maxdist
}


proc pick_GUI_addremovemap {} {
	global pick

	set l [list]
# isolate Nviz Commands?
	set ll [Nget_site_list]
	foreach ele $ll {lappend l Nsite$ele}

	set ll [Nget_vect_list]
	foreach ele $ll {lappend l Nvect$ele}

	foreach ele $l {puts "[$ele get_att map]"}

	set r $pick(MAP_LIST)

	# list l contains all the active maps at the moment
	# list r contains all the pickable maps
	# we should remove from l all the maps in r
	# if a map is in r, but not in l, it must be removed from r
	# at the end: l U r = original l
	foreach ele $r {
		if {![remove_from_list $ele l]} {
			# $ele is in r but not in l: remove it from pick(MAP_LIST)!
			pick_remove_map $ele
		}
	}
	# update r
	set r $pick(MAP_LIST)

	set lname [list]
	foreach ele $l {lappend lname [$ele get_att map]}
	set rname [list]
	foreach ele $r {lappend rname [$ele get_att map]}

	if {[addremove_list_create2 "Select sites and vects to \"pick\"" \
								"all sites and vects" "pickable sites and vects" lname rname l r]} {
		# now r is modified and we have to set the differences from pick(MAP_LIST)

		# add new pickable elements
		foreach ele $r {
			set index [lsearch -exact $pick(MAP_LIST) $ele]
			# if not present $ele is new
			if {$index < 0} {pick_add_map $ele}
		}

		# remove new non-pickable elements
		foreach ele $pick(MAP_LIST) {
			set index [lsearch -exact $r $ele]
			# if not present $ele has been deleted from list
			if {$index < 0} {pick_remove_map $ele}
		}
	}
}



proc pick_init {} {
	global pick

	rc_load_res "hyperlink.field" pick(MM_NAME) "MULTIMEDIA"
	set pick(MAP_LIST) [list]
}

proc pick_set {_pick _win} {
	global pick
	upvar $_pick pick

	if {$pick} {
		bind $_win <Button-1> {pick %x %y }
	} else {
		bind $_win <Button-1> {}
    }
}

proc pick_map_is_pickable {_map} {
	global pick
	set index [lsearch -exact $_map $pick(MAP_LIST)]
	if {$index < 0} {
		return 0
	} else {
		return 1
	}
}

proc pick_add_map {_map} {
	global pick
	add_to_list $_map pick(MAP_LIST)
}

proc pick_remove_map {_map} {
	global pick
	remove_from_list $_map pick(MAP_LIST)

	catch {unset pick(HL_CAT_LIST.$_map)}
	catch {pick_erase_records $pick(SF.$_map)}
	catch {pick_delete_records $pick(SF.$_map)}
	catch {destroy $pick(WIN.$_map)}
}


proc pick {_x _y} {
	global pick
	set _y [expr [pick_panel_get_Nv_height] - $_y]

	set maxdist [pick_panel_get_pick_maxdist]

	set redraw 0
	foreach map $pick(MAP_LIST) {
		set cat [lindex [Npick_vect $_x $_y [$map get_att map] $maxdist] 0]

		if {$cat != ""} {
			pick_draw_win $map $cat
			set redraw 1
		}
	}

	if {$redraw > 0} {Ndraw_all}
}


proc pick_highlight_hyperlinks {_on _win _max_found} {
	global pick

	upvar $_on on
	appBusy

	if {$on} {
# shapes:
# ST_X 1/ST_BOX 2/ST_SPHERE 3/ST_CUBE 4/ST_DIAMOND 5/ST_DEC_TREE 6/ST_CON_TREE 7/ST_ASTER 8/ST_GYRO 9
		rc_load_res "hyperlink.display.scale" scl 5
		rc_load_res "hyperlink.display.shape" shp 3
		rc_load_res "hyperlink.display.color" clr "#0000ff"

		foreach map $pick(MAP_LIST) {
			set name [$map get_att map]

			set i 0
			foreach {n t} [Nsite_attr_get_fields_name_and_type $name] {
				#SOSTITUIRE $pick(MM_NAME) / "STAZIONE_P"
				if {$n == $pick(MM_NAME)} {
					set pick(HL_CAT_LIST.$map) [lrange [Nsite_attr_get_field_not_emtpy_cats $name $i] 0 $_max_found]
					# catch used in case highlight hasn't been defined
					catch {Nsite_highlight_list color [string range $map 5 end] $pick(HL_CAT_LIST.$map) $clr}
					catch {Nsite_highlight_list size [string range $map 5 end] $pick(HL_CAT_LIST.$map) $scl}
					catch {Nsite_highlight_list marker [string range $map 5 end] $pick(HL_CAT_LIST.$map) $shp}
					Ndraw_all
				}
				incr i
			}
		}
	} else {
		# off
		foreach map $pick(MAP_LIST) {
			if {[info exists pick(HL_CAT_LIST.$map)]} {
				# catch used in case highlight hasn't been defined
				catch {Nsite_unhighlight_list all [string range $map 5 end] $pick(HL_CAT_LIST.$map)}
				unset pick(HL_CAT_LIST.$map)
			}
		}
		Ndraw_all
	}

	appNotBusy
}


proc pick_draw_maxdist {_surf_id _x _y _maxdist} {
	set x1 [expr $_x - $_maxdist]
	set x2 [expr $_x + $_maxdist]
	set y1 [expr $_y - $_maxdist]
	set y2 [expr $_y + $_maxdist]
	Ndraw_line_on_surf $_surf_id $x1 $y1 $x2 $y1
	Ndraw_line_on_surf $_surf_id $x2 $y1 $x2 $y2
	Ndraw_line_on_surf $_surf_id $x2 $y2 $x1 $y2
	Ndraw_line_on_surf $_surf_id $x1 $y2 $x1 $y1
}


proc pick_select_record { _sf _irec} {
	global pick

	set w [scrollframe_interior $_sf]

	set first_row [expr $pick($_sf.ROW) - $pick($_sf.NREC)]
	set irow [expr $_irec + $first_row]
	set nfields [llength $pick($_sf.RECORD.$_irec)]

	for {set i $first_row} {$i < $pick($_sf.ROW)} {incr i} {
		for {set j 0} {$j < $nfields} {incr j} {
			if {$j != $pick($_sf.MM_INDEX)} {
				catch {$w.r$i\c$j configure -background "white"}
			}
		}
	}

	for {set j 0} {$j < $nfields} {incr j} {
		if {$j != $pick($_sf.MM_INDEX)} {
			catch {$w.r$irow\c$j configure -background "yellow"}
		}
	}

	# position scrollframe on selected row
	update
	scrollframe_ymoveto $_sf [expr ($irow.0 - 1.0) / ($pick($_sf.ROW).0)]

	# catch used in case highlight hasn't been defined
	catch {Nsite_unhighlight all [string range $pick($_sf.MAP) 5 end] $pick($_sf.HIGHLIGTHED)}
	set pick($_sf.HIGHLIGTHED) $pick($_sf.UNIQUE.$_irec)
	catch {Nsite_highlight default [string range $pick($_sf.MAP) 5 end] $pick($_sf.UNIQUE.$_irec)}
	Ndraw_all
}

proc pick_draw_win {_map _cat} {
	global pick

	set win ".pick2$_map"
	set sf $win.f

	set hopt "-relief raised -background gray"

	if {[winfo exists $win]} {
		raise $win
		set w [scrollframe_interior $sf]
		pick_add_record $sf $_map $_cat
		return $sf
	}

	set pick($sf.HEADER) [Nsite_attr_get_fields_name [$_map get_att map]]
	if {$pick($sf.HEADER) == ""} {return $win.f}

	set pick($sf.MM_INDEX) [pick_get_multimedia_field $pick($sf.HEADER) $pick(MM_NAME)]

	toplevel $win
	wm resizable $win true true
	wm title $win "Map: [$_map get_att map]"

	wm geometry $win "820x200+10+250"
	wm minsize $win 150 100
	bind $win <Destroy> "pick_unselect_all %W $win $_map"

	scrollframe_create $sf
	pack $sf -expand yes -fill both -side bottom
	set w [scrollframe_interior $sf]

	set pick(SF.$_map) $sf
	set pick(WIN.$_map) $win
	set pick($sf.MAP) $_map

	set pick($sf.ROW) 0

	button $w.b -text "clear" -command "pick_clear_window $win $sf $_map"
	grid $w.b	-row $pick($sf.ROW) -column 0 -columnspan 2 -sticky nsw
	incr pick($sf.ROW)

	pick_draw_row $sf $w $pick($sf.ROW) $pick($sf.HEADER) $hopt -1 ""
	incr pick($sf.ROW)

	if {[info exists pick($sf.NREC)]} {
		for {set i 0} {$i < $pick($sf.NREC)} {incr i} {
					pick_draw_row $sf $w $pick($sf.ROW) $pick($sf.RECORD.$i) \
					"-relief groove -background white" $pick($sf.MM_INDEX) "pick_multimedia"

			incr pick($sf.ROW)
		}
	} else {
		set pick($sf.NREC) 0
	}

	pick_add_record $sf $_map $_cat

	return $sf
}

proc pick_add_record {sf _map _cat} {
	global pick

	set w [scrollframe_interior $sf]

	set index $pick($sf.NREC)
	set pick($sf.RECORD.$index) [Nsite_attr_get_record_values [$_map get_att map] $_cat]
	set pick($sf.UNIQUE.$index) $_cat

	if {$pick($sf.RECORD.$index) != ""} {

		set irec [pick_seek $sf $pick($sf.UNIQUE.$index)]

		if {0 > $irec} {
			# not present
			pick_draw_row $sf $w $pick($sf.ROW) "$pick($sf.RECORD.$index)" \
					"-relief groove -background white" $pick($sf.MM_INDEX) "pick_multimedia"

			pick_select_record $sf $index

			incr pick($sf.NREC)
			incr pick($sf.ROW)
		} else {
			# already present
			pick_select_record $sf $irec
		}
	}
}

proc pick_unselect_all {_w _win _map} {
	if {$_w != $_win} {return}

	# catch used in case highlight hasn't been defined
	catch {Nsite_unhighlight all [string range $pick($_sf.MAP) 5 end] $pick($_sf.HIGHLIGTHED)}
	Ndraw_all
}

proc pick_seek {sf which} {
	global pick

	for {set i 0} {$i < $pick($sf.NREC)} {incr i} {
		if {$which == $pick($sf.UNIQUE.$i)} {return $i}
	}
	return -1
}

proc pick_clear_window {w sf _map} {
	if {![confirm_ask "Clear Window?" "Yes" "No"]} {return}
#	catch {destroy $w}
	pick_erase_records $sf
	pick_delete_records $sf

	# catch used in case highlight hasn't been defined
	catch {Nsite_unhighlight all [string range $pick($_sf.MAP) 5 end] $pick($_sf.HIGHLIGTHED)}
	Ndraw_all
}

proc pick_delete_records {sf} {
	global pick

	for {set i 0} {$i < $pick($sf.NREC)} {incr i} {
		unset pick($sf.RECORD.$i)
	}
	set pick($sf.NREC) 0
}

proc pick_erase_records {_sf} {
	global pick

	set nfields [llength $pick($_sf.HEADER)]

	set w [scrollframe_interior $_sf]
	set first_row [expr $pick($_sf.ROW) - $pick($_sf.NREC)]

	for {set i $first_row} {$i < $pick($_sf.ROW)} {incr i} {
		for {set j 0} {$j < $nfields} {incr j} {catch {destroy $w.r$i\c$j}}
	}
	set pick($_sf.ROW) $first_row
}

proc pick_get_multimedia_field {_header _field} {
	if {$_field == ""} {return -1}
	set i 0
	foreach elt $_header {
		if {$elt == $_field} {return $i}
		incr i
	}
	return -1
}

proc pick_draw_row {sf w row lst opt field cmd} {
	set i -1
	foreach elt $lst {
		incr i
		if {$elt == ""} {set elt "***"}
		if {$i == $field && $cmd != ""} {
			button $w.r$row\c$i -text "[list $elt]" -borderwidth 1  -anchor e -command "$cmd [list $elt]"
			bind $w.r$row\c$i <ButtonPress-1> "pick_mouse_XY %X %Y"
		} else {
			eval label $w.r$row\c$i -text "[list $elt]" -borderwidth 1  -anchor e "$opt"

			bind $w.r$row\c$i <Button-1> "pick_select_record_from_record $sf $row"
		}
		grid $w.r$row\c$i -row $row -column $i -sticky nsew
	}
}

proc pick_select_record_from_record {_sf _row} {
	global pick

	set first_row [expr $pick($_sf.ROW) - $pick($_sf.NREC)]
	set irec [expr $_row - $first_row]

	if {$irec < 0} {return}

	pick_select_record $_sf $irec
}

proc pick_mouse_XY {_x _y} {
	global pick
	set pick(MOUSE_X) $_x
	set pick(MOUSE_Y) $_y
}

proc pick_multimedia {_field} {
	global pick

	set ext [file extension $_field]
	rc_load_res "hyperlink.path" dir

	if {![rc_load_res "hyperlink.extension$ext" app]} {
		if {[file isdirectory $dir/$_field]} {
			#set filelist [split [exec ls $dir/$_field]]
			set filelist [glob $dir/$_field/*]
			set m ".pick_multimedia_hyperlink"
			catch {destroy $m}
			menu $m -tearoff 0
			foreach f $filelist {
				#$m add command -label "$f" -command "pick_multimedia  $_field/$f"
				$m add command -label "[file tail $f]" -command "pick_multimedia  \"$f\""
			}
			tk_popup $m $pick(MOUSE_X) $pick(MOUSE_Y)
		} else {
			puts "*** WARNING (PICK) *** extension \"$ext\" has no associated application"
		}
	} else {
		if {[string range $_field 0 2] == "www"} {
			set filename $_field
		} else {
			set filename [file join $dir $_field]
		}

		puts "pick_multimedia: exec $app $filename"
		if {[catch {exec $app "$filename" &}]} {
			puts "*** WARNING (PICK) *** command \"$app $filename\" returns an error"
		}
	}
}
