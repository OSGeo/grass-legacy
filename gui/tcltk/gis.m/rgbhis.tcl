##########################################################################
# rgbhis.tcl - RGB and HIS display layer options file for GRASS GIS Manager
# March 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmRgbhis {
    variable array opt # rgbhis current options
    variable count 1
    variable array lfile # rgbhis
    variable array lfilemask # rgbhis
    variable optlist
}


proc GmRgbhis::create { tree parent } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist
    global gmpath

    set node "rgbhis:$count"

    set frm [ frame .rgbicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
		-variable GmRgbhis::opt($count,1,_check) \
		-height 1 -padx 0 -width 0]

    image create photo rgbico -file "$gmpath/rgbhis.gif"
    set ico [label $frm.ico -image rgbico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "RGB-HIS $count"\
	-window    $frm \
	-drawcross auto  
    
    set opt($count,1,_check) 1 
    set opt($count,1,map1) "" 
    set opt($count,1,map2) "" 
    set opt($count,1,map3) "" 
	set opt($count,1,opacity) 1.0
    set opt($count,1,overlay) 1 
    set opt($count,1,rgb) 1 
    set opt($count,1,his) 0 
    set opt($count,1,mod) 1


	set optlist { _check map1 map2 map3 rgb his overlay opacity}

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

proc GmRgbhis::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

proc GmRgbhis::select_map1 { id } {
    variable tree
    variable node
    set m1 [GSelect cell]
    if { $m1 != "" } { 
        set GmRgbhis::opt($id,1,map1) $m1
        GmTree::autonamel "RGB-HIS $m1"
    }
}

proc GmRgbhis::select_map2 { id } {
    variable tree
    variable node
    set m2 [GSelect cell]
    if { $m2 != "" } { 
        set GmRgbhis::opt($id,1,map2) $m2
    }
}
proc GmRgbhis::select_map3 { id } {
    variable tree
    variable node
    set m3 [GSelect cell]
    if { $m3 != "" } { 
        set GmRgbhis::opt($id,1,map3) $m3
    }
}
# display RGB and HIS options
proc GmRgbhis::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading ]
    Label $row.a -text "Display 3 raster maps as red/green/blue or hue/intensity/saturation channels" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmRgbhis::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # raster1 name
    set row [ frame $frm.name1 ]
    Label $row.a -text "     red (RGB) or hue (HIS):           "
    Button $row.b -image [image create photo -file "$gmpath/rgbhis1.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "raster map for red or hue channel"]\
		-command "GmRgbhis::select_map1 $id" -height 26
    Entry $row.c -width 30 -text " $opt($id,1,map1)" \
          -textvariable GmRgbhis::opt($id,1,map1) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # raster2 name
    set row [ frame $frm.name2 ]
    Label $row.a -text "     green (RGB) or intensity (HIS):"
    Button $row.b -image [image create photo -file "$gmpath/rgbhis2.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "raster map for green or intensity channel"]\
		-command "GmRgbhis::select_map2 $id" -height 26
    Entry $row.c -width 30 -text " $opt($id,1,map2)" \
          -textvariable GmRgbhis::opt($id,1,map2) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # raster3 name
    set row [ frame $frm.name3 ]
    Label $row.a -text "     blue (RGB) or saturation (HIS):"
    Button $row.b -image [image create photo -file "$gmpath/rgbhis3.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "raster map for blue or saturation channel"]\
		-command "GmRgbhis::select_map3 $id" -height 26
    Entry $row.c -width 30 -text " $opt($id,1,map3)" \
          -textvariable GmRgbhis::opt($id,1,map3) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
    
    # display type
    set row [ frame $frm.type ]
    checkbutton $row.a -text [G_msg "display maps as RGB"] -variable \
        GmRgbhis::opt($id,1,rgb) 
    Button $row.b -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.rgb" \
            -background $bgcolor \
            -helptext [G_msg "Help for RGB"]
    checkbutton $row.c -text [G_msg "display maps as HIS"] -variable \
        GmRgbhis::opt($id,1,his) 
    Button $row.d -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.his" \
            -background $bgcolor \
            -helptext [G_msg "Help for HIS"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # overlay
    set row [ frame $frm.over ]
    checkbutton $row.a -text [G_msg "overlay maps from other layers (transparent null value cells)"] -variable \
        GmRgbhis::opt($id,1,overlay) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	set opt($id,1,mod) "1"

}

proc GmRgbhis::save { tree depth node } {
    variable opt
    variable optlist
    global mon
    
    set id [GmTree::node_id $node]

    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}


proc GmRgbhis::display { node mod} {
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
    
    set line ""
    set input ""
    global gmpath
    set cmd1 ""
    set cmd2 ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set opt($id,1,mod) $mod

    if { $opt($id,1,map1) == "" } { return } 
    if { $opt($id,1,map2) == "" &&  $opt($id,1,map3) == "" && $opt($id,1,rgb) == 1 } { 
        return 
     } 
  
    if { $opt($id,1,rgb) == 1 } { 
        set cmd1 "d.rgb red=$opt($id,1,map1) green=$opt($id,1,map2) blue=$opt($id,1,map3)" 
     }

    if { $opt($id,1,his) == 1 } { 
        set cmd2 "d.his h_map=$opt($id,1,map1)" 
        if { $opt($id,1,map2) != "" } {        
            append cmd2 " i_map=$opt($id,1,map2)"
         }
        if { $opt($id,1,map3) != "" } {        
            append cmd2 " s_map=$opt($id,1,map2)"
         }
     }

    # check to see if options have changed
    foreach key $optlist {
        if {$opt($id,0,$key) != $opt($id,1,$key)} {
        	set opt($id,1,mod) 1
        	set opt($id,0,$key) $opt($id,1,$key)
        }
    } 

	# overlay
	if { $opt($id,1,overlay) && $opt($id,1,rgb) == 1 } { 
		append cmd1 " -o"
	}

	if { $opt($id,1,overlay) && $opt($id,1,his) == 1 } { 
		append cmd2 " -n"
	}

    # if options have change (or mod flag set by other procedures) re-render map
	if {$opt($id,1,mod) == 1} {
		# display rgb map    
		if { $cmd1 != "" } { 
			# redraw rgb
			run_panel $cmd1
		}
		# display his map    
		if { $cmd2 != "" } { 
			# redraw his
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



proc GmRgbhis::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "rgbhis:$count"

    set frm [ frame .rgbhisicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmRgbhis::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rgbico -file "$gmpath/rgbhis.gif"
    set ico [label $frm.ico -image rgbico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,1,map1) == ""} {
    	$tree insert end $parent $node \
		-text      "RGB-HIS $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "RGB-HIS $opt($id,1,map1)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,1,_check) $opt($id,1,_check)

    set opt($count,1,map1) "$opt($id,1,map1)" 
    set opt($count,1,map2) "$opt($id,1,map2)"  
    set opt($count,1,map3) "$opt($id,1,map3)"  
	set opt($count,1,opacity) opt($id,1,opacity)
    set opt($count,1,overlay) $opt($id,1,overlay)
    set opt($count,1,rgb) $opt($id,1,rgb)
    set opt($count,1,his) $opt($id,1,his) 

    incr count
    return $node
}

proc GmRgbhis::mapname { node } {
    variable opt
    variable tree
    global mon
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,1,_check) ) } { return } 

    if { $opt($id,1,map1) == "" && $opt($id,1,map2) == "" } { return }

    set mapname "$opt($id,1,map1),$opt($id,1,map2)"

    if {$opt($id,1,map3) != ""} {
    	append mapname ",$opt($id,1,map3)"
    }
    
    return $mapname
}
