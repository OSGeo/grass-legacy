##########################################################################
# 
# Panel to provide d.3d type interface for manually entering position / 
# view.
#
##########################################################################

proc mkposPanel { BASE } {
    global Nv_ bearing_calc
    
    catch {destroy $BASE}
    
    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} $BASE "Position" 1 5]
    } else {
	set panel $Nv_($BASE)
    }
    
    frame $BASE -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Position Panel"

#################################
# From menu    
    set tmp1 [frame $BASE.top1 -border 1 -relief sunken]
    set tmp1a [frame $BASE.top1.top1a]
    set tmp1b [frame $BASE.top1.top1b]
    
    label $tmp1a.l1 -text "From (Eye)"
    pack $tmp1a.l1 -side top
    
    label $tmp1b.l1 -text "East:"
    entry $tmp1b.e1 -width 10 -textvariable Nv_(east1) -background white
    label $tmp1b.l2 -text "North:"
    entry $tmp1b.e2 -width 10 -textvariable Nv_(north1) -background white
    label $tmp1b.l3 -text "Ht:"
    entry $tmp1b.e3 -width 8 -textvariable Nv_(ht1) -background white
    
    pack $tmp1b.l1 $tmp1b.e1 $tmp1b.l2 $tmp1b.e2 $tmp1b.l3 $tmp1b.e3 -side left 
    
    pack $tmp1a $tmp1b -side top -fill x -expand 1
    pack $tmp1 -side top -fill x -expand 1

# Seperator
Nv_makeSeparator $BASE.sep1   
#pack $BASE.sep1 -side top -fill x

#################################
# To menu       
    set tmp2 [frame $BASE.top2 -border 1 -relief sunken]
    set tmp2a [frame $BASE.top2.top2a]
    set tmp2b [frame $BASE.top2.top2b]
    
    label $tmp2a.l1 -text "To (Surface)"
    pack $tmp2a.l1 -side top
    
    label $tmp2b.l1 -text "East:"
    entry $tmp2b.e1 -width 10 -textvariable Nv_(east2) -background white
    label $tmp2b.l2 -text "North:"
    entry $tmp2b.e2 -width 10 -textvariable Nv_(north2) -background white
    label $tmp2b.l3 -text "Ht:"
    entry $tmp2b.e3 -width 8 -textvariable Nv_(ht2) -background white
    
    pack $tmp2b.l1 $tmp2b.e1 $tmp2b.l2 $tmp2b.e2 $tmp2b.l3 $tmp2b.e3 -side left 
    
    pack $tmp2a $tmp2b -side top -fill x -expand 1
    pack $tmp2 -side top -fill x -expand 1

# Seperator
Nv_makeSeparator $BASE.sep2   
#pack $BASE.sep2 -side top -fill x
    
#################################
# Range Bearing menu       
    set tmp4 [frame $BASE.top4 -border 1 -relief sunken]
    set tmp4a [frame $BASE.top4.top4a]
    set tmp4b [frame $BASE.top4.top4b]
    set tmp4c [frame $BASE.top4.top4c]
    
    label $tmp4a.l1 -text "Range / Bearing"
    pack $tmp4a.l1 -side top
    
    label $tmp4b.l1 -text "Range:"
    entry $tmp4b.e1 -width 10 -textvariable Nv_(range) -background white
    label $tmp4b.l2 -text "Bearing\n(deg.):"
    entry $tmp4b.e2 -width 10 -textvariable Nv_(bearing) -background white
    label $tmp4b.l3 -text "Elev.\n(deg.):"
    entry $tmp4b.e3 -width 8 -textvariable Nv_(elev) -background white
    pack $tmp4b.l1 $tmp4b.e1 $tmp4b.l2 $tmp4b.e2 $tmp4b.l3 $tmp4b.e3 -side left 
    
    radiobutton $tmp4c.r1 -text "Eye to Surface" -variable "bearing_calc" -value "1" -command "show_bearing"
    radiobutton $tmp4c.r2 -text "Surface to Eye" -variable "bearing_calc" -value "2" -command "show_bearing"
    button $tmp4c.b1 -text "Calculate" -command {calc_position $bearing_calc}
    pack $tmp4c.r1 $tmp4c.r2 $tmp4c.b1 -side left
    
    
    pack $tmp4a $tmp4b $tmp4c -side top -fill x -expand 1
    pack $tmp4 -side top -fill x -expand 1

    
#################################
# Buttons menu       
    set tmp3 [frame $BASE.top3 -border 1 -relief sunken]
    
    button $tmp3.b1 -text "Refresh" \
    	-command {set from_loc [Nget_real_position 1]
    	set to_loc [Nget_real_position 2]
    	set Nv_(east1) [format_number [lindex $from_loc 0]]
    	set Nv_(north1) [format_number [lindex $from_loc 1]]
    	set Nv_(ht1) [format_number [lindex $from_loc 2]]
    	
    	set Nv_(east2) [format_number [lindex $to_loc 0]]
    	set Nv_(north2) [format_number [lindex $to_loc 1]]
    	set Nv_(ht2) [format_number [lindex $to_loc 2]]

	show_bearing
    	}
    
    button $tmp3.b2 -text "Apply" \
    	-command {
	#Set To coords
	Nset_focus_real $Nv_(east2) $Nv_(north2) $Nv_(ht2)
	#reset XY canvas
	change_display 2

	#Set From coords
    	Nmove_to_real $Nv_(east1) $Nv_(north1) $Nv_(ht1)
	#reset height
	Nv_setEntry $Nv_(main_BASE).midf.height.f.entry $Nv_(ht1)
	Nv_floatscaleCallback $Nv_(main_BASE).midf.height e 2 null $Nv_(ht1)
	#reset XY canvas
        change_display 1

	Nquick_draw
    	}
    
    button $tmp3.b3 -text "Close" -command "Nv_closePanel $BASE"
    	
    pack $tmp3.b1 $tmp3.b2 -side left 
    pack $tmp3.b3 -side right

    pack $tmp3 -side top -fill x -expand 1
    
    #set radiobutton
    set bearing_calc 1
    	
return $panel
   
}

########################################
# Proc format_number to format float to reasonable
# number of decimals -- max = 3
proc format_number {n} {

set num_tmp $n

if {$n == [expr int($num_tmp)] } {
	set val [format %.0f $n]
} elseif { [expr $n*10.] == [expr int($num_tmp*10.)] } {
	set val [format %.1f $n]
} elseif { [expr $n*100.] == [expr int($num_tmp*100.)] } {
	set val [format %.2f $n]
} else {
	set val [format %.3f $n]
}

return $val

}

########################################
# Proc calc_position to coordinate from
# rangle bearing and elev.
proc calc_position {flag} {
global Nv_

set RAD 0.0174532925199432958

#convert range to 2D range
set range_xy  [expr (cos($Nv_(elev)*$RAD) * $Nv_(range))]
set zz [expr (sin($Nv_(elev)*$RAD) * $Nv_(range))]
set xx [expr (sin($Nv_(bearing)*$RAD) * $range_xy)]
set yy [expr (cos($Nv_(bearing)*$RAD) * $range_xy)]

 if {$flag == 1} {
 #Calculate new surface center from eye position
 set Nv_(east2) [format_number [expr $Nv_(east1) + $xx]]
 set Nv_(north2) [format_number [expr $Nv_(north1) + $yy]]
 #always look down
 set Nv_(ht2) [format_number [expr $Nv_(ht1) - $zz]]
  } else {
 #Calculate new eye position from surface center coord
 set Nv_(east1) [format_number [expr $Nv_(east2) + $xx]]
 set Nv_(north1) [format_number [expr $Nv_(north2) + $yy]]
 #always look up
 set Nv_(ht1) [format_number [expr $Nv_(ht2) + $zz]]
 }
}

########################################
# Proc show_bearing to calculate and show
# current range and bearing
proc show_bearing {} {
global Nv_ bearing_calc

set RAD 0.0174532925199432958

if {$bearing_calc == 1} {
set xx [expr $Nv_(east2) - $Nv_(east1)]
set yy [expr $Nv_(north2) - $Nv_(north1)]
set zz [expr $Nv_(ht2) - $Nv_(ht1)]
} else {
set xx [expr $Nv_(east1) - $Nv_(east2)]
set yy [expr $Nv_(north1) - $Nv_(north2)]
set zz [expr $Nv_(ht1) - $Nv_(ht2)]
}

set Nv_(range) [format_number [expr sqrt( ($xx*$xx) + ($yy*$yy) + ($zz*$zz) )]]
set Nv_(elev) [format_number [expr sinh(abs($zz)/$Nv_(range))/$RAD ]]

if {$yy == 0. && $xx == 0.} {
	set bear_tmp 0.
} elseif {$yy == 0.} {
	set bear_tmp 90.
} elseif {$xx == 0.} {
        set bear_tmp 0.
} else {
set bear_tmp [expr atan(abs($xx)/abs($yy)) / $RAD]
}
if {$xx >= 0. && $yy > 0.} {
	set Nv_(bearing) [format_number $bear_tmp]
} elseif {$xx > 0. && $yy <= 0.} {
	set Nv_(bearing) [format_number [expr 180. - $bear_tmp]]
} elseif {$xx <= 0. && $yy < 0.} {
	set Nv_(bearing) [format_number [expr $bear_tmp + 180.]]
} elseif {$xx < 0. && $yy >= 0.} {
	set Nv_(bearing) [format_number [expr 360. - $bear_tmp]]
} else {
	set Nv_(bearing) 999
}
	
}
