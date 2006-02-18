###############################################################
# rastnums.tcl - cell value display layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmRnums {
    variable array opt # hist options
    variable count 1
    variable array tree # mon
}

source $gmpath/mapcanvas.tcl

proc GmRnums::create { tree parent } {
    variable opt
    variable count
    #variable tree
    
    global gmpath
    global mon

    set node "rnums:$count"

    set frm [ frame .rnumsicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
		-variable GmRnums::opt($count,_check) \
		-height 1 -padx 0 -width 0]

    image create photo nico -file "$gmpath/rastnums.gif"
    set ico [label $frm.ico -image nico -bd 1 -relief raised]
    
    pack $check $ico -side left
        
    $tree insert end $parent $node \
	-text  "cell values $count"\
	-window    $frm \
	-drawcross auto  
    
    set opt($count,_check) 1 
    set opt($count,map) "" 
    set opt($count,grid_color) "grey" 
    set opt($count,text_color) "black" 
    set opt($count,cellcolor) 0 
    
    incr count
    return $node
}

proc GmRnums::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmRnums::select_map { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRnums::opt($id,map) $m
        GmTree::autonamel "cell values for $m"
    }
}

# display histogram options
proc GmRnums::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Display cell values from raster map or image" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    set row [ frame $frm.heading2 ]
    Label $row.a -text "  (resolution must be 100x100 or less)"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # raster name for histogram
    set row [ frame $frm.name ]
    Label $row.a -text "Raster to display: "
    Button $row.b -image [image create photo -file "$gmpath/raster.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmRnums::select_map $id"
    Entry $row.c -width 35 -text " $opt($id,map)" \
          -textvariable GmRnums::opt($id,map) \
          -background white
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
		-image [image create photo -file "$gmpath/grass.gif"] \
		-command "run g.manual d.rast.num" \
		-background $bgcolor -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # grid color
    set row [ frame $frm.grid ]
    Label $row.a -text "Color for cell grid:       "
    ComboBox $row.b -padx 2 -width 10 -textvariable GmRnums::opt($id,grid_color) \
		-values {"white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # text color
    set row [ frame $frm.text ]
    Label $row.a -text "Color for cell values:   "
    ComboBox $row.b -padx 2 -width 10 -textvariable GmRnums::opt($id,text_color) \
		-values {"white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    checkbutton $row.c -text [G_msg "use raster colors for cell values"] \
        -variable GmRnums::opt($id,cellcolor) 
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
}

proc GmRnums::save { tree depth node } {
    variable opt
    global mon
    
    set id [GmTree::node_id $node]

    foreach key { 
    	_check map grid_color text_color cellcolor } {
         GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc GmRnums::display { node } {
    variable opt
    variable tree
    set currmon ""
    global gmpath
    global mon

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    set cmd "d.rast.num map=$opt($id,map) grid_color=$opt($id,grid_color) \
    	text_color=$opt($id,text_color)"

    # include nulls
    if { $opt($id,cellcolor) } { 
        append cmd " -f"
    }

    # only run if less than 100x100 cells
	set string ""
	set cells 0
	set rest ""
	set rc [open "|g.region -g" r]
	set rowscolumns [read $rc]
	close $rc
	regexp {rows=(\d*)} $rowscolumns string rows
	regexp {cols=(\d*)} $rowscolumns string cols
	set cells [expr $rows * $cols]
	if { $cells <= 10000} {
		runcmd $cmd 
	} else {
		set msgtxt "Cell values can only be displayed\nfor regions of < 10,000 cells" 
		set answer [tk_messageBox -message $msgtxt -type ok -parent .mapcan($mon)]
		if { $answer == "ok" } {return}
	}
}
    
proc GmRnums::mapname { node } {
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

proc GmRnums::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath
    global mon

    set node "hist:$count"

    set frm [ frame .rnumsicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
		-variable GmRnums::opt($count,_check) \
		-height 1 -padx 0 -width 0]

    image create photo nico -file "$gmpath/histogram.gif"
    set ico [label $frm.ico -image nico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,map) == ""} {
    	$tree insert end $parent $node \
		-text      "histogram $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "histogram for $opt($id,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)
    set opt($count,map) $opt($id,map)
    set opt($count,grid_color) $opt($id,grid_color)
    set opt($count,text_color) $opt($id,text_color)
    set opt($count,cellcolor) $opt($id,cellcolor)

    incr count
    return $node
}
