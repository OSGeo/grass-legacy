
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
#	Alex Shevlakov sixote@yahoo.com 02/2000
#	functions added to handle postgres queries
########################################################################### 
global WhatsHere Nv
set WhatsHere(first) 1

# Globals to track what's here attributes
set Nv_(what_mapname)            1
set Nv_(what_easting)            1
set Nv_(what_northing)           1
set Nv_(what_elevation)          1
set Nv_(what_colorcat)           1
set Nv_(what_xydiff)             0
set Nv_(what_xyzdiff)            0
set Nv_(what_surfdist)           1
set Nv_(what_exagsurfdist)       1
set Nv_(what_postgres) 	         1
set Nv_(what_pipe)               0
set Nv_(what_pipe_text)          "Pipe to: None"
set Nv_(obj_qry_text)		 "Query map: None"
set Nv_(t_ent) 			 "info_kuruma"
set Nv_(c_ent)			 "rec_id"
set Nv_(xcol_ent)			 "xcoor"
set Nv_(ycol_ent)			 "ycoor"
set Nv_(dist_ent)			 "200"
set Nv_(setcurrtype)			"vect"
set Nv_(obj_qry)			"none@none"

proc mkqueryPanel { BASE } {
    
    global WhatsHere
    global Nv_
    
    catch {destroy $BASE}
    
    # Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} $BASE "What's Here?" 1 5]
    } else {
	set panel $Nv_($BASE)
    }
    
    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "What's Here Panel"
    
    # Create frame, buttons, and attributes menu
    frame $BASE.bf
    checkbutton $BASE.bf.what -text "What's Here?" -relief raised \
	-command whats_here -variable WhatsHere(on)
    button $BASE.bf.seperate -text Reset -command do_seperate
    button $BASE.bf.clear -text Clear -command clear_text
    menubutton $BASE.bf.atts -text Attributes -menu $BASE.bf.atts.m -relief raised
    menu $BASE.bf.atts.m
    pack $BASE.bf.clear $BASE.bf.seperate $BASE.bf.atts -side right -expand 1
    pack $BASE.bf.what -side left
    
    # Add menu entries for menu
    set theMenu $BASE.bf.atts.m
    foreach i {{"Map Name" "mapname" 0} {"Easting" "easting" 0} {"Northing" "northing" 0} \
		   {"Elevation" "elevation" 1} {"Color Category" "colorcat" 0} \
		   {"XY Dist from Prev" "xydiff" 0} \
		   {"XYZ Dist from Prev" "xyzdiff" 2} {"Dist Along Surface" "surfdist" 0} \
		   {"Dist Along Exag Surface" "exagsurfdist" 5} {"Postgres" "postgres" 1}} {

	$theMenu add checkbutton -label [lindex $i 0] -underline [lindex $i 2]\
	    -offvalue 0 -onvalue 1 -variable Nv_(what_[lindex $i 1])
    }

    pack $BASE.bf -side top -fill x 
    
    # frame for close button and saving output to a file
    frame $BASE.cf
    button $BASE.cf.close -text Close -command "Nv_closePanel $BASE" -anchor s
    
    button $BASE.cf.output -textvariable Nv_(what_pipe_text) \
	-command "whats_pipe_bind $BASE"
    
    pack $BASE.cf.close $BASE.cf.output -side right -padx 2
    pack $BASE.cf.output -side left
    pack $BASE.cf -side bottom -fill x 

    
    frame $BASE.st -relief groove -borderwidth 2
    set rbase $BASE.st 
    label $rbase.xcol_label -text "Xcol:"
    entry $rbase.xcol_entry -relief sunken -width 6 \
	-textvariable Nv_(xcol_ent)
    label $rbase.ycol_label -text "Ycol:"
    entry $rbase.ycol_entry -relief sunken -width 6 \
	-textvariable Nv_(ycol_ent)
    label $rbase.dist_label -text "Dist:"
    entry $rbase.dist_entry -relief sunken -width 5 \
	-textvariable Nv_(dist_ent)
    pack $rbase.xcol_label $rbase.xcol_entry \
	$rbase.ycol_label $rbase.ycol_entry $rbase.dist_label $rbase.dist_entry -side left \
	-padx 2 -anchor n -expand no
    pack $rbase -side bottom -fill x
    
    frame $BASE.pg -relief groove -borderwidth 2
    set rbase $BASE.pg
    label $rbase.t_label -text "PG table:"
    entry $rbase.t_entry -relief sunken -width 10 \
	-textvariable Nv_(t_ent)
    label $rbase.c_label -text "Key col:"
    entry $rbase.c_entry -relief sunken -width 8 \
	-textvariable Nv_(c_ent)

    pack $rbase.t_label $rbase.t_entry \
	$rbase.c_label $rbase.c_entry -side left \
	-padx 2 -anchor n -expand no
    pack $rbase -side bottom -fill x
    
    # frame for map button
    frame $BASE.gf -relief groove -borderwidth 2
    button $BASE.gf.vect_qry -textvariable Nv_(obj_qry_text) \
	-command "whats_obj_bind $BASE"

    pack $BASE.gf.vect_qry -side left
    pack $BASE.gf -side bottom -fill x 
    
    frame $BASE.typeobj -relief groove -borderwidth 2
    
    radiobutton $BASE.typeobj.v -relief flat -text "Query vector" -value vect \
	-anchor nw -variable Nv_(setcurrtype)
    radiobutton $BASE.typeobj.s -relief flat -text "sites" \
	-value site -anchor nw -variable Nv_(setcurrtype)
    
    pack $BASE.typeobj.v $BASE.typeobj.s  \
	-side left -fill x -expand 1
    pack $BASE.typeobj -side bottom -fill x

    
    text $BASE.text -wrap word -relief sunken -bd 2 \
	-yscrollcommand "$BASE.yscroll set"  \
	-width 40 -height 10
    
    set WhatsHere(text) $BASE.text
    
    # scrollbar $BASE.xscroll -orient horizontal -relief flat 
    # -activebackground gray80 -command "$BASE.text view"
    
    scrollbar $BASE.yscroll -orient vertical -relief flat \
	-command "$BASE.text yview" -activebackground gray80 
    
    pack $BASE.yscroll -side right -fill y
    # pack $BASE.xscroll -side bottom -fill x
    pack $BASE.text -expand yes -fill both
    
    return $panel
}

proc whats_pipe_bind {BASE} {
    global Nv_

    # Allow the user to set a file for piping the output 
    set new_file [create_file_browser .whats_file 1 0]
    if {$new_file == -1} then return

    set Nv_(what_pipe) $new_file
    set Nv_(what_pipe_text) "Pipe to: $new_file"
}

proc whats_here {} {
    global WhatsHere Nv_
    
    if {$WhatsHere(on)} {
    	if {$Nv_(what_postgres)} {
	bind $Nv_(TOP).canvas <Button> {whats_here_pg_grass %x %y }
    	} else {
	bind $Nv_(TOP).canvas <Button> {whats_here_info %x %y }
	}
	
    } else {
	bind $Nv_(TOP).canvas <Button> {}	
    }
}

proc whats_here_info {x y} {
    global WhatsHere
    global Nv_
    
    set y [expr $Nv_(height) - $y]

#puts "DEBUG $x $y"
    
    set text $WhatsHere(text)
    set tot_out ""

    set list [Nget_point_on_surf $x $y]
    if {[llength $list] < 4} {
	$text insert end "Point not on surface\n"
	append tot_out "Point not on surface\n"
	$text yview -pickplace end
	return
    }
    
    set x [lindex $list 0]
    set y [lindex $list 1]
    set z [lindex $list 2]
    set id [lindex $list 3]
    
    if {$Nv_(what_easting)} then {
	set str [format "\teasting:  %15.4f\n" $x]
	$text insert end "$str"
	append tot_out "$str"
	$text yview -pickplace end
    }

    if {$Nv_(what_northing)} then {
	set str [format "\tnorthing:  %15.4f\n" $y]
	$text insert end "$str"
	append tot_out "$str"
	$text yview -pickplace end
    }

    if {$Nv_(what_elevation)} then {
	set str [format "\televation:  %15.4f\n" $z]
	$text insert end "$str"
	append tot_out "$str"
	$text yview -pickplace end
    }
    
    if {$Nv_(what_mapname)} then {
	set str [Nget_map_name [string range $id 5 end] surf]
	$text insert end "Topo: $str"
	append tot_out "Topo: $str"
	set str [Nget_cat_at_xy $id topo $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	set str [Nget_val_at_xy $id topo $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	$text yview -pickplace end
    }

    if {$Nv_(what_colorcat)} then {
	set map_name [$id get_att color]
	if {[lindex $map_name 0] == "map"} then {
	    set str [lindex $map_name 1]
	} else {
	    set str "constant"
	}
	$text insert end "Color: $str"
	append tot_out "Color: $str"
	set str [Nget_cat_at_xy $id color $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	set str [Nget_val_at_xy $id color $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	$text yview -pickplace end
    }

    if {$WhatsHere(first) == 0} {
	set px $WhatsHere(px)
	set py $WhatsHere(py)
	set pz $WhatsHere(pz)

### NO-NO Change to use lib functions for distance!
	
	if {$Nv_(what_xydiff)} then {
	    set val [expr sqrt(($px-$x)*($px-$x)+($py-$y)*($py-$y))]
	    set str [format "xy distance from previous:   %15.4f\n" $val]
	    $text insert end "$str"
	    append tot_out "$str"
	    $text yview -pickplace end
	}

	if {$Nv_(what_xyzdiff)} then {
	    set val [expr sqrt(($px-$x)*($px-$x)+($py-$y)*($py-$y)+($pz-$z)*($pz-$z))]
	    set str [format "xyz distance from previous:   %15.4f\n" $val]
	    $text insert end "$str"
	    append tot_out "$str"
	    $text yview -pickplace end
	}
	
	if {$WhatsHere(pid) == $id} {
	    if {$Nv_(what_surfdist)} then {
		set dist [Nget_dist_along_surf $id $x $y $px $py 0]
		set str [format "distance along surface:       %15.4f\n" $dist]
		$text insert end "$str"
		append tot_out "$str"
		$text yview -pickplace end
		Nset_draw front
		Ndraw_line_on_surf $id $x $y $px $py
		Nset_draw back
	    }

	    if {$Nv_(what_exagsurfdist)} then {
		set dist [Nget_dist_along_surf $id $x $y $px $py 1]
		set str [format "distance along exag. surface: %15.4f\n" $dist]
		$text insert end "$str"
		append tot_out "$str"
		$text yview -pickplace end
		Nset_draw front
		Ndraw_line_on_surf $id $x $y $px $py
		Nset_draw back
	    }
	    
	}
    }

    $text insert end "\n"

    set WhatsHere(px) $x
    set WhatsHere(py) $y
    set WhatsHere(pz) $z
    set WhatsHere(pid) $id
    set WhatsHere(first) 0
    
    Nset_draw front
    Ndraw_X $id $x $y
    Nset_draw back

    if {$Nv_(what_pipe) != 0} {
	set out_file [open $Nv_(what_pipe) a]
	puts $out_file $tot_out
	close $out_file
    }
}
proc whats_obj_bind {BASE} {
    global Nv_

     set new [create_map_browser .fbrowse $Nv_(setcurrtype) 1]
    if {$new == -1} then return

    set Nv_(obj_qry) $new
    set Nv_(obj_qry_text) "Query map:$new"
}
proc whats_here_pg_grass {x y} {
    global WhatsHere
    global Nv_
    
    set y [expr $Nv_(height) - $y]

#puts "DEBUG $x $y"
    
    set text $WhatsHere(text)
    set tot_out ""
    
    if {$Nv_(setcurrtype) == "vect"} {
    set list [Nget_point_on_pg_grass $x $y $Nv_(obj_qry) $Nv_(t_ent) $Nv_(c_ent)]
    } else {if {$Nv_(setcurrtype) == "site"} {
    set list [Nget_point_on_pg_site $x $y $Nv_(t_ent) $Nv_(xcol_ent) $Nv_(ycol_ent) $Nv_(dist_ent)]
    	} else {
    	$text insert end "Invalid query object type\n"
	append tot_out "Invalid query object type\n"
	$text yview -pickplace end
	return
	}
    }
    
    if {[llength $list] < 5} {
	$text insert end "Point not on surface\n"
	append tot_out "Point not on surface\n"
	$text yview -pickplace end
	return
    }
    
    set x [lindex $list 0]
    set y [lindex $list 1]
    set z [lindex $list 2]
    set id [lindex $list 3]
    set ls [lindex $list 4]
    
    if {$Nv_(what_easting)} then {
	set str [format "\teasting:  %15.4f\n" $x]
	$text insert end "$str"
	append tot_out "$str"
	$text yview -pickplace end
    }

    if {$Nv_(what_northing)} then {
	set str [format "\tnorthing:  %15.4f\n" $y]
	$text insert end "$str"
	append tot_out "$str"
	$text yview -pickplace end
    }

    if {$Nv_(what_elevation)} then {
	set str [format "\televation:  %15.4f\n" $z]
	$text insert end "$str"
	append tot_out "$str"
	$text yview -pickplace end
    }
    
    if {$Nv_(what_postgres)} then {
	set str [format "\tPostgres:  %s\n" $ls]
	$text insert end "$str"
	append tot_out "$str"
	$text yview -pickplace end
    }
    if {$Nv_(what_mapname)} then {
	set str [Nget_map_name [string range $id 5 end] surf]
	$text insert end "Topo: $str"
	append tot_out "Topo: $str"
	set str [Nget_cat_at_xy $id topo $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	set str [Nget_val_at_xy $id topo $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	$text yview -pickplace end
    }

    if {$Nv_(what_colorcat)} then {
	set map_name [$id get_att color]
	if {[lindex $map_name 0] == "map"} then {
	    set str [lindex $map_name 1]
	} else {
	    set str "constant"
	}
	$text insert end "Color: $str"
	append tot_out "Color: $str"
	set str [Nget_cat_at_xy $id color $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	set str [Nget_val_at_xy $id color $x $y]
	$text insert end "\t$str\n"
	append tot_out "\t$str\n"
	$text yview -pickplace end
    }

    if {$WhatsHere(first) == 0} {
	set px $WhatsHere(px)
	set py $WhatsHere(py)
	set pz $WhatsHere(pz)

### NO-NO Change to use lib functions for distance!
	
	if {$Nv_(what_xydiff)} then {
	    set val [expr sqrt(($px-$x)*($px-$x)+($py-$y)*($py-$y))]
	    set str [format "xy distance from previous:   %15.4f\n" $val]
	    $text insert end "$str"
	    append tot_out "$str"
	    $text yview -pickplace end
	}

	if {$Nv_(what_xyzdiff)} then {
	    set val [expr sqrt(($px-$x)*($px-$x)+($py-$y)*($py-$y)+($pz-$z)*($pz-$z))]
	    set str [format "xyz distance from previous:   %15.4f\n" $val]
	    $text insert end "$str"
	    append tot_out "$str"
	    $text yview -pickplace end
	}
	
	if {$WhatsHere(pid) == $id} {
	    if {$Nv_(what_surfdist)} then {
		set dist [Nget_dist_along_surf $id $x $y $px $py 0]
		set str [format "distance along surface:       %15.4f\n" $dist]
		$text insert end "$str"
		append tot_out "$str"
		$text yview -pickplace end
		Nset_draw front
		Ndraw_line_on_surf $id $x $y $px $py
		Nset_draw back
	    }

	    if {$Nv_(what_exagsurfdist)} then {
		set dist [Nget_dist_along_surf $id $x $y $px $py 1]
		set str [format "distance along exag. surface: %15.4f\n" $dist]
		$text insert end "$str"
		append tot_out "$str"
		$text yview -pickplace end
		Nset_draw front
		Ndraw_line_on_surf $id $x $y $px $py
		Nset_draw back
	    }
	    
	}
    }

    $text insert end "\n"

    set WhatsHere(px) $x
    set WhatsHere(py) $y
    set WhatsHere(pz) $z
    set WhatsHere(pid) $id
    set WhatsHere(first) 0
    
    Nset_draw front
    Ndraw_X $id $x $y
    Nset_draw back

    if {$Nv_(what_pipe) != 0} {
	set out_file [open $Nv_(what_pipe) a]
	puts $out_file $tot_out
	close $out_file
    }
}
proc do_seperate {} {
    global WhatsHere Nv_
    
    set text $WhatsHere(text)
    
    $text insert end "___________________________________\n"
    $text yview -pickplace end

    if {$Nv_(what_pipe) != 0} {
	set out_file [open $Nv_(what_pipe) a]
	puts $out_file "___________________________________"
	close $out_file
    }

    set WhatsHere(first) 1
}

proc clear_text {} {
    global WhatsHere
    
    set text $WhatsHere(text)
    
    $text delete 1.0 end
    $text yview -pickplace end
}

