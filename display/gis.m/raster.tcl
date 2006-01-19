# 18 March 2005

namespace eval GmRaster {
    variable array opt # raster options
    variable count 1
    variable array tree # mon
}

source $gmpath/mapcanvas.tcl

proc GmRaster::create { tree parent } {
    variable opt
    variable count
    #variable tree
    
    global gmpath
    global mon

    set node "raster:$count"

    set frm [ frame .rastericon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmRaster::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rico -file "$gmpath/raster.gif"
    set ico [label $frm.ico -image rico -bd 1 -relief raised]
    
    pack $check $ico -side left
        
    $tree insert end $parent $node \
	-text  "raster $count"\
	-window    $frm \
	-drawcross auto  
    
    set opt($count,_check) 1 
    set opt($count,map) "" 
    set opt($count,drapemap) "" 
    set opt($count,querytype) "cat" 
    set opt($count,rastquery) "" 
    set opt($count,rasttype) "" 
    set opt($count,bkcolor) "" 
    set opt($count,overlay) 1 
    
    incr count
    return $node
}

proc GmRaster::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmRaster::select_map1 { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRaster::opt($id,map) $m
        GmTree::autonamel $m
    }
}

proc GmRaster::select_map2 { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRaster::opt($id,drapemap) $m
        GmTree::autonamel $m
    }
}
# display raster options
proc GmRaster::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # raster name
    set row [ frame $frm.name ]
    Button $row.a -text [G_msg "Raster name:"] \
           -command "GmRaster::select_map1 $id"
    Entry $row.b -width 49 -text " $opt($id,map)" \
          -textvariable GmRaster::opt($id,map) \
          -background white
    Button $row.c -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.rast" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # raster query
    set row [ frame $frm.rquery ]
    Label $row.a -text "Raster values to display"
    LabelEntry $row.b -textvariable GmRaster::opt($id,rastquery) -width 42 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # drape name
    set row [ frame $frm.drape ]
    Button $row.a -text [G_msg "Raster to drape over 1st map:"] \
           -command "GmRaster::select_map2 $id"
    Entry $row.b -width 35 -text " $opt($id,drapemap)" \
          -textvariable GmRaster::opt($id,drapemap) \
          -background white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    set row [ frame $frm.drapeinfo ]
    Label $row.a -text "     (color over relief map or data fusion--1st map for shading, 2nd for color)"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
        
    # overlay
    set row [ frame $frm.over ]
    checkbutton $row.a -text [G_msg "overlay maps from other layers (transparent null value cells)"] \
        -variable GmRaster::opt($id,overlay) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # background color
    set row [ frame $frm.bg ]
    Label $row.a -text " Set background color (colored null value cells)"
    ComboBox $row.b -padx 2 -width 10 -textvariable GmRaster::opt($id,bkcolor) \
                    -values {"white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
                    -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmRaster::save { tree depth node } {
    variable opt
    global mon
    
    set id [GmTree::node_id $node]

    foreach key { 
    	_check map drapemap querytype rastquery rasttype bkcolor overlay } {
         GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc GmRaster::display { node } {
    variable opt
    variable rasttype
    variable tree
    set rasttype ""
    set currmon ""
    set line ""
    set input ""
    global gmpath
    global mon

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    set cmd "d.rast map=$opt($id,map)"

    # overlay
    if { $opt($id,overlay) } { 
        append cmd " -o"
    }

    # set raster type
     set rt [open "|r.info map=$opt($id,map) -t" r]
     set rasttype [read $rt]
     close $rt
        if {[regexp -nocase ".=CELL" $rasttype]} {
            set querytype "cat"
        } else {
            set querytype "vallist"
        }


    # raster query
    if { $opt($id,rastquery) != "" } { 
        append cmd " {$querytype=$opt($id,rastquery)}"
    }
    
    # background color
    if { $opt($id,bkcolor) != "" } { 
        append cmd " bg=$opt($id,bkcolor)"
    }
    
    set cmd2 "d.his h_map=$opt($id,drapemap) i_map=$opt($id,map)"
    
    if { $opt($id,drapemap) == "" } { 
        run_panel $cmd 
    } else {
        run_panel $cmd2
    }



	#set current monitor for raster display
	if ![catch {open "|d.mon -L" r} input] {
		while {[gets $input line] >= 0} {
			if {[regexp -nocase {.*(selected).*} $line]} {
				regexp -nocase {..} $line currmon
			}              
		}
	}

}

# get selected raster map (used for query)
proc GmRaster::mapname { node } {
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

proc GmRaster::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath
    global mon

    set node "raster:$count"

    set frm [ frame .rastericon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmRaster::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rico -file "$gmpath/raster.gif"
    set ico [label $frm.ico -image rico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,map) == ""} {
    	$tree insert end $parent $node \
		-text      "raster $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "$opt($id,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)

    set opt($count,map) "$opt($id,map)" 
    set opt($count,drapemap) "$opt($id,drapemap)" 
    set opt($count,querytype) "$opt($id,querytype)" 
    set opt($count,rastquery) "$opt($id,rastquery)" 
    set opt($count,rasttype) "$opt($id,rasttype)" 
    set opt($count,bkcolor) "$opt($id,bkcolor)" 
    set opt($count,overlay) "$opt($id,overlay)"

    incr count
    return $node
}
