###############################################################
# rastarrows.tcl - raster slope arrows layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmArrows {
    variable array opt # hist options
    variable count 1
    variable array tree # mon
}

source $gmpath/mapcanvas.tcl

proc GmArrows::create { tree parent } {
    variable opt
    variable count
    #variable tree
    
    global gmpath
    global mon

    set node "arrows:$count"

    set frm [ frame .arrowicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
		-variable GmArrows::opt($count,_check) -height 1 -padx 0 -width 0]

    image create photo aico -file "$gmpath/rastarrows.gif"
    set ico [label $frm.ico -image aico -bd 1 -relief raised]
    
    pack $check $ico -side left
        
    $tree insert end $parent $node -text  "arrows $count"\
		-window $frm -drawcross auto  
    
    set opt($count,_check) 1 
 	set opt($count,map) "" 
    set opt($count,type) "grass" 
    set opt($count,arrow_color) "green" 
    set opt($count,grid_color) "grey" 
    set opt($count,x_color) "black" 
	set opt($count,unknown_color) "red" 
    set opt($count,skip) 1
    set opt($count,magnitude_map) "" 
    set opt($count,scale) 1.0 

    incr count
    return $node
}

proc GmArrows::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmArrows::select_map { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmArrows::opt($id,map) $m
        GmTree::autonamel "arrows for $m"
    }
}

proc GmArrows::select_magmap { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmArrows::opt($id,magnitude_map) $m
        GmTree::autonamel "arrows for $m"
    }
}
# display histogram options
proc GmArrows::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Display arrows whose orientations are based on raster aspect map" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    set row [ frame $frm.heading2 ]
    Label $row.a -text "  (optionally, arrow lengths are based on slope or intensity map)" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # raster map for arrow direction
    set row [ frame $frm.map ]
    Label $row.a -text "Aspect map: "
    Button $row.b -image [image create photo -file "$gmpath/rastarrowsdir.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmArrows::select_map $id"
    Entry $row.c -width 35 -text " $opt($id,map)" \
          -textvariable GmArrows::opt($id,map) \
          -background white
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
		-image [image create photo -file "$gmpath/grass.gif"] \
		-command "run g.manual d.rast.arrow" \
		-background $bgcolor -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # map type
    set row [ frame $frm.type ]
    Label $row.a -text "    aspect value type"
    ComboBox $row.b -padx 2 -width 8 -textvariable GmArrows::opt($id,type) \
		-values {"grass" "compass" "agnps" "answers"} -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
		
    # skip factor
    set row [ frame $frm.skip ]
    Label $row.a -text "    draw arrows every Nth grid cell" 
    SpinBox $row.b -range {1 200 1} -textvariable GmArrows::opt($id,skip) \
		-width 4 -entrybg white 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # arrow and grid color
    set row [ frame $frm.color1 ]
    Label $row.a -text "    arrow color     "
    ComboBox $row.b -padx 2 -width 10 -textvariable GmArrows::opt($id,arrow_color) \
		-values {"white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    Label $row.c -text "      cell grid color"
    ComboBox $row.d -padx 2 -width 10 -textvariable GmArrows::opt($id,grid_color) \
		-values {"none" "white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
    
    # x and unknown color
    set row [ frame $frm.color2 ]
    Label $row.a -text "    null value color"
    ComboBox $row.b -padx 2 -width 10 -textvariable GmArrows::opt($id,x_color) \
		-values {"white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    Label $row.c -text { 'unknowns' color}
    ComboBox $row.d -padx 2 -width 10 -textvariable GmArrows::opt($id,unknown_color) \
		-values {"none" "white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # raster map for arrow magnitude
    set row [ frame $frm.mag ]
    Label $row.a -text "Slope/intensity map: "
    Button $row.b -image [image create photo -file "$gmpath/rastarrowsint.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmArrows::select_magmap $id"
    Entry $row.c -width 35 -text " $opt($id,magnitude_map)" \
          -textvariable GmArrows::opt($id,magnitude_map) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
		
    # scale arrow length
    set row [ frame $frm.scale ]
    LabelEntry $row.a -label [G_msg "    scale factor for computing arrow length"] \
		-textvariable GmArrows::opt($id,scale) -width 5 -entrybg white
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
}

proc GmArrows::save { tree depth node } {
    variable opt
    global mon
    
    set id [GmTree::node_id $node]

    foreach key { 
    	_check map type arrow_color grid_color x_color unknown_color \
    	skip magnitude_map scale } {
         GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc GmArrows::display { node } {
    variable opt
    variable tree
    set currmon ""
    set input ""
    global gmpath
    global mon

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    set cmd "d.rast.arrow map=$opt($id,map) type=$opt($id,type) \
    	arrow_color=$opt($id,arrow_color) grid_color=$opt($id,grid_color) \
    	x_color=$opt($id,x_color) unknown_color=$opt($id,unknown_color) \
    	skip=$opt($id,skip) scale=$opt($id,scale)"

    # include nulls
    if { $opt($id,magnitude_map) != "" } { 
        append cmd " magnitude_map=$opt($id,magnitude_map)"
    }
	
	run_panel $cmd 
}
    
proc GmArrows::mapname { node } {
    variable opt
    variable tree
    global mon
    global mapname
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 
    
    set mapname $opt($id,map)
    return $mapname
}

proc GmArrows::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath
    global mon
    
    set node "arrows:$count"

    set frm [ frame .arrowicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
		-variable GmArrows::opt($count,_check) \
		-height 1 -padx 0 -width 0]

    image create photo aico -file "$gmpath/rastarrows.gif"
    set ico [label $frm.ico -image aico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,map) == ""} {
    	$tree insert end $parent $node -text "arrows $count" -window $frm \
			-drawcross auto
	} else {
	    $tree insert end $parent $node -text "arrows for $opt($id,map)" \
		-window $frm -drawcross auto
	}

    set opt($count,_check) $opt($id,_check)
    set opt($count,map) "$opt($id,map)" 
    set opt($count,type) "$opt($id,type)"
    set opt($count,arrow_color) "$opt($id,arrow_color)"
    set opt($count,grid_color) "$opt($id,grid_color)"
    set opt($count,x_color) "$opt($id,x_color)" 
	set opt($count,unknown_color) "$opt($id,unknown_color)"
    set opt($count,skip) "$opt($id,skip)"
    set opt($count,magnitude_map) "$opt($id,magnitude_map)"
    set opt($count,scale)  "$opt($id,scale)"

    incr count
    return $node
}
