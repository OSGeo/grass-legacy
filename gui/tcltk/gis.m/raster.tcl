##########################################################################
# raster.tcl - raster layer options file for GRASS GIS Manager
# March 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmRaster {
    variable array opt # raster current options
    variable count 1
    variable array tree # mon
    variable array lfile # raster
    variable array lfilemask # raster
    variable optlist
}

source $gmpath/mapcanvas.tcl

proc GmRaster::create { tree parent } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist
    
    global gmpath
    global mon

    set node "raster:$count"

	#create form for layer tree entry
    set frm [ frame .rastericon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmRaster::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rico -file "$gmpath/raster.gif"
    set ico [label $frm.ico -image rico -bd 1 -relief raised]
    pack $check $ico -side left

	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "raster $count"\
	-window    $frm \
	-drawcross auto  
    
    #set default option values
    set opt($count,1,_check) 1 
	set opt($count,1,opacity) 1.0
    set opt($count,1,map) "" 
    set opt($count,1,drapemap) "" 
    set opt($count,1,querytype) "cat" 
    set opt($count,1,rastquery) "" 
    set opt($count,1,rasttype) "" 
    set opt($count,1,bkcolor) "" 
    set opt($count,1,overlay) 1 
    set opt($count,1,mod) 1
    
	set optlist {_check map drapemap querytype rastquery rasttype bkcolor \
		overlay opacity}

    foreach key $optlist {
		set opt($count,0,$key) $opt($count,1,$key)
    } 
    
	# create files in tmp diretory for layer output
	set mappid [pid]
	set lfile($count) [eval exec "g.tempfile pid=$mappid"]
	set lfilemask($count) $lfile($count)
	append lfile($count) ".ppm"
	append lfilemask($count) ".pgm"

    incr count
    return $node

}

proc GmRaster::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

proc GmRaster::select_map { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRaster::opt($id,1,map) $m
        GmTree::autonamel $m
    }
}

proc GmRaster::select_drapemap { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRaster::opt($id,1,drapemap) $m
        GmTree::autonamel $m
    }
}
# display raster options
proc GmRaster::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading ]
    Label $row.a -text "Display raster maps" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1 \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmRaster::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # raster name
    set row [ frame $frm.name ]
    Label $row.a -text "Base map:        "
    Button $row.b -image [image create photo -file "$gmpath/raster.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "base raster map to display"]\
		-command "GmRaster::select_map $id"
    Entry $row.c -width 35 -text " $opt($id,1,map)" \
          -textvariable GmRaster::opt($id,1,map) \
          -background white -text
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.rast" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # raster query
    set row [ frame $frm.rquery ]
    Label $row.a -text "     values to display"
    LabelEntry $row.b -textvariable GmRaster::opt($id,1,rastquery) -width 35 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # drape name
    set row [ frame $frm.drapeinfo1 ]
    Label $row.a -text "     Optional color draping. Use base map for shading,"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    set row [ frame $frm.drapeinfo2 ]
    Label $row.a -text "     drape map for color in color relief map or data fusion"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    set row [ frame $frm.drape ]
    Label $row.a -text "     drape map:  "
    Button $row.b -image [image create photo -file "$gmpath/raster.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "raster map to drape over base map"]\
		-command "GmRaster::select_drapemap $id"
    Entry $row.c -width 35 -text " $opt($id,1,drapemap)" \
          -textvariable GmRaster::opt($id,1,drapemap) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
        
    # overlay
    set row [ frame $frm.over ]
    checkbutton $row.a -text [G_msg "overlay maps from other layers (transparent null value cells)"] \
        -variable GmRaster::opt($id,1,overlay) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # background color
    set row [ frame $frm.bg ]
    Label $row.a -text " Set background color (colored null value cells)"
    ComboBox $row.b -padx 2 -width 10 -textvariable GmRaster::opt($id,1,bkcolor) \
                    -values {"white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
                    -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmRaster::save { tree depth node } {
    variable opt
    variable optlist
    global mon
    
    set id [GmTree::node_id $node]

    foreach key $optlist {
         GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}


# append elevation maps display lists for NVIZ
proc GmRaster::addelev {node nvelev} {
    variable opt
    variable tree
    global mon
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]
    
    if { ! ( $opt($id,1,_check) ) } { return } 

	set nvelev "$opt($id,1,map)"
	
	return $nvelev
}

# append drape colors to display lists for NVIZ
proc GmRaster::addcolor {node nvcolor} {
    variable opt
    variable tree
    global mon
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,1,_check) ) } { return } 

	if { $opt($id,1,drapemap) != "" } {
		set nvcolor $opt($id,1,drapemap)
	} else { 
		set nvcolor $opt($id,1,map)
    } 
    return $nvcolor
}

proc GmRaster::display { node mod } {
    global mapfile
    global maskfile
    global complist
    global opclist
    global masklist
    global gmpath
    global mon
    variable optlist
    variable lfile 
    variable lfilemask
    variable opt
    variable rasttype
    variable tree

    set rasttype ""
    set currmon ""
    set line ""
    set input ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]
    

    set opt($id,1,mod) $mod    

    if { $opt($id,1,map) == "" } { return } 

    set cmd "d.rast map=$opt($id,1,map)"

    # overlay
    if { $opt($id,1,overlay) } { 
        append cmd " -o"
    }

    # set raster type
     set rt [open "|r.info map=$opt($id,1,map) -t" r]
     set rasttype [read $rt]
     close $rt
        if {[regexp -nocase ".=CELL" $rasttype]} {
            set querytype "cat"
        } else {
            set querytype "vallist"
        }


    # raster query
    if { $opt($id,1,rastquery) != "" } { 
        append cmd " {$querytype=$opt($id,1,rastquery)}"
    }
    
    # background color
    if { $opt($id,1,bkcolor) != "" } { 
        append cmd " bg=$opt($id,1,bkcolor)"
    }
    
    set cmd2 "d.his -n h_map=$opt($id,1,drapemap) i_map=$opt($id,1,map)"
            
    # check to see if options have changed
    foreach key $optlist {
        if {$opt($id,0,$key) != $opt($id,1,$key)} {
        	set opt($id,1,mod) 1
        	set opt($id,0,$key) $opt($id,1,$key)
        }
    } 
    
    # if options have change (or mod flag set by other procedures) re-render map
	if {$opt($id,1,mod) == 1} {
		if { $opt($id,1,drapemap) == "" } { 
			# redraw raster
			run_panel $cmd
		} else {
			# redraw raster as his
			run_panel $cmd2
		}
		file copy -force $mapfile($mon) $lfile($id)
		file copy -force $maskfile($mon) $lfilemask($id)
    }

    if { ! ( $opt($id,1,_check) ) } { return } 


    #add lfile to compositing list
	if {$complist($mon) != "" } {
	    append complist($mon) ","
	    append complist($mon) [file tail $lfile($id)]
	} else {
	    append complist($mon) [file tail $lfile($id)]
	}	

	if {$masklist($mon) != "" } {
	    append masklist($mon) ","
	    append masklist($mon) [file tail $lfilemask($id)]
	} else {
	    append masklist($mon) [file tail $lfilemask($id)]
	}	

	if {$opclist($mon) != "" } {
	    append opclist($mon) ","
	    append opclist($mon) $opt($id,1,opacity)
	} else {
	    append opclist($mon) $opt($id,1,opacity)
	}	
	
	# reset options changed flag
	set opt($id,1,mod) 0
}

# get selected raster map (used for query)
proc GmRaster::mapname { node } {
    variable opt
    variable tree
    global mon
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,1,_check) ) } { return } 

    if { $opt($id,1,map) == "" } { return } 
    
    set mapname $opt($id,1,map)
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
                           -variable GmRaster::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rico -file "$gmpath/raster.gif"
    set ico [label $frm.ico -image rico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,1,map) == ""} {
    	$tree insert end $parent $node \
		-text      "raster $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "$opt($id,1,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,1,_check) $opt($id,1,_check)

    set opt($count,1,map) "$opt($id,1,map)" 
	set opt($count,1,opacity) "opt($id,1,opacity)"
    set opt($count,1,drapemap) "$opt($id,1,drapemap)" 
    set opt($count,1,querytype) "$opt($id,1,querytype)" 
    set opt($count,1,rastquery) "$opt($id,1,rastquery)" 
    set opt($count,1,rasttype) "$opt($id,1,rasttype)" 
    set opt($count,1,bkcolor) "$opt($id,1,bkcolor)" 
    set opt($count,1,overlay) "$opt($id,1,overlay)"

    incr count
    return $node
}
