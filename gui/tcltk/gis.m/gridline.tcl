##########################################################################
# grid.tcl - grid and line overlay layer options file for GRASS GIS Manager
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmGridline {
    variable array opt # grid current options
    variable count 1
    variable array tree # mon
    variable array lfile # raster
    variable array lfilemask # raster
    variable optlist
    variable first
    variable array dup # vector
}

proc GmGridline::create { tree parent } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist
    variable first
	variable dup
    global mon
    global gmpath
    global iconpath
    global guioptfont

    set node "gridline:$count"

    set frm [ frame .gridicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmGridline::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo gico -file "$iconpath/module-d.grid.gif"
    set gdico [label $frm.gdico -image gico -bd 1 -relief raised]
    
    pack $check $gdico -side left
    
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "gridline $count"\
	-window    $frm \
	-drawcross auto  

    set opt($count,1,_check) 1 
    set dup($count) 0
    
	set opt($count,1,opacity) 1.0
    set opt($count,1,gridline) "gridline" 
    set opt($count,1,gridcolor) \#AAAAAA
    set opt($count,1,gridborder) \#000000 
    set opt($count,1,gridsize) 100 
    set opt($count,1,gridorigin) "0,0" 
    set opt($count,1,griddraw) 1 
    set opt($count,1,gridgeod) 0
    set opt($count,1,borderdraw) 1 
    set opt($count,1,textdraw) 1 
    
    set opt($count,1,rhumbdraw) 0 
    set opt($count,1,rhumbcoor) "" 
    set opt($count,1,rhumbcolor) "black" 
    
    set opt($count,1,geoddraw) 0 
    set opt($count,1,geodcoor) "" 
    set opt($count,1,geodcolor) "black" 
    set opt($count,1,geodtxtcolor) "none" 

    set first 1
    set opt($count,1,mod) 1
    
	set optlist { _check gridcolor gridborder gridsize gridorigin griddraw gridgeod \
    			borderdraw textdraw rhumbdraw rhumbcoor geoddraw geodcoor geodcolor \
    			geodtxtcolor} 

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

proc GmGridline::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

# display gridline options
proc GmGridline::options { id frm } {
    variable opt
    global gmpath
    global bgcolor
    global iconpath

    # Panel heading
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Display grid lines, and geodesic lines or rhumblines" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmGridline::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # grid options 1
    set row [ frame $frm.grid1 ]
    Label $row.a -text "Grid options: "
    checkbutton $row.b -text [G_msg "draw grid"] -variable GmGridline::opt($id,1,griddraw) 
    checkbutton $row.c -text [G_msg "geodetic grid"] -variable GmGridline::opt($id,1,gridgeod) 
    SelectColor $row.d -type menubutton -variable GmGridline::opt($id,1,gridcolor)    
    Label $row.e -text [G_msg " grid color   "] 
    Button $row.f -text [G_msg "Help"] \
            -image [image create photo -file "$iconpath/gui-help.gif"] \
            -command "run g.manual d.grid" \
            -background $bgcolor \
            -helptext [G_msg "Help for grids"]
    Label $row. -text [G_msg " grid color"] 
    pack $row.a $row.b $row.c $row.d $row.e $row.f  -side left
    pack $row -side top -fill both -expand yes

    # grid options 2
    set row [ frame $frm.grid2 ]
    Label $row.a -text [G_msg "     "]
    checkbutton $row.b -text [G_msg "draw grid border"] -variable GmGridline::opt($id,1,borderdraw) 
    checkbutton $row.c -text [G_msg "draw border text"] -variable GmGridline::opt($id,1,textdraw) 
    Label $row.d -text [G_msg " border & text color"] 
    SelectColor $row.e -type menubutton -variable GmGridline::opt($id,1,gridborder)
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # grid options 3
    set row [ frame $frm.grid3 ]
    Label $row.a -text "    grid size (map units)"
    LabelEntry $row.b -textvariable GmGridline::opt($id,1,gridsize) -width 7 \
            -entrybg white
    Label $row.c -text " grid origin (east, north)"
    LabelEntry $row.d -textvariable GmGridline::opt($id,1,gridorigin) -width 15 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
    
    set row [ frame $frm.line ]
    Label $row.a -text "Geodesic and rhumblines for latlong locations only"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # geodesic line options 1
    set row [ frame $frm.geod1 ]
    Label $row.a -text "     "
    checkbutton $row.b -text [G_msg "draw geodesic line"] -variable GmGridline::opt($id,1,geoddraw)
    Button $row.c -text [G_msg "Help"] \
            -image [image create photo -file "$iconpath/gui-help.gif"] \
            -command "run g.manual d.geodesic" \
            -background $bgcolor \
            -helptext [G_msg "Help for geodesic lines"]
    Label $row.d -text " line color"
    ComboBox $row.e -padx 2 -width 7 -textvariable GmGridline::opt($id,1,geodcolor) \
                    -values {"white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
                    -entrybg white
    Label $row.f -text " text color"
    ComboBox $row.g -padx 2 -width 7 -textvariable GmGridline::opt($id,1,geodtxtcolor) \
                    -values {"white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
                    -entrybg white
    pack $row.a $row.b $row.c $row.d $row.e $row.f $row.g -side left
    pack $row -side top -fill both -expand yes
    
    # geodesic line options 2
    set row [ frame $frm.geod2 ]
    Label $row.a -text "     line endpoints (x1,y1,x2,y2)"
    LabelEntry $row.b -textvariable GmGridline::opt($id,1,geodcoor) -width 35 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # rhumbline options 1
    set row [ frame $frm.rhumb1 ]
    Label $row.a -text "     "
    checkbutton $row.b -text [G_msg "draw rhumbline"] -variable GmGridline::opt($id,1,rhumbdraw)
    Button $row.c -text [G_msg "Help"] \
            -image [image create photo -file "$iconpath/gui-help.gif"] \
            -command "run g.manual d.rhumbline" \
            -background $bgcolor \
            -helptext [G_msg "Help for rhumblines"]
    Label $row.d -text " line color"
    ComboBox $row.e -padx 2 -width 7 -textvariable GmGridline::opt($id,1,rhumbcolor) \
                    -values {"white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
                    -entrybg white
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes
    
    # rhumbline options 2
    set row [ frame $frm.rhumb2 ]
    Label $row.a -text "     line endpoints (x1,y1,x2,y2)"
    LabelEntry $row.b -textvariable GmGridline::opt($id,1,rhumbcoor) -width 35 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmGridline::save { tree depth node } {
    variable opt
    variable optlist
    
    set id [GmTree::node_id $node]


    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"     

    }                     
}

proc GmGridline::display { node mod } {
    global mon
    global mapfile
    global maskfile
    global complist
    global opclist
    global masklist
    variable optlist
    variable lfile 
    variable lfilemask
    variable opt
    variable tree
    variable dup
    variable count
    variable first

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set opt($id,1,mod) $mod    

    set cmd ""
    set cmd2 ""
    set cmd3 ""

    if { ! ( $opt($id,1,_check) ) } { return } 
    
    # set hex colors to rgb         
    set gridcolor [Gm::color $opt($id,1,gridcolor)]
    set gridborder [Gm::color $opt($id,1,gridborder)]

    
    # d.grid command
    if { $opt($id,1,griddraw) || $opt($id,1,borderdraw) } {
            set cmd "d.grid size=$opt($id,1,gridsize) origin=$opt($id,1,gridorigin) \
                color=$gridcolor bordercolor=$gridborder" 
        } 
        
    if { $opt($id,1,gridgeod) && $cmd != "" } {append cmd " -g"} 
    if { !$opt($id,1,griddraw) && $cmd != "" } {append cmd " -n"} 
    if { !$opt($id,1,borderdraw) && $cmd != "" } {append cmd " -b"}
    if { !$opt($id,1,textdraw) && $cmd != "" } {append cmd " -t"}
        
    # d.geodesic command
    if { $opt($id,1,geoddraw) } {
        set cmd2 "d.geodesic coor=$opt($id,1,geodcoor) \
                lcolor=$opt($id,1,geodcolor) \
                tcolor=$opt($id,1,geodtxtcolor)"  
    }

    # d.rhumbline command
    if { $opt($id,1,rhumbdraw) } {
        set cmd3 "d.rhumbline coor=$opt($id,1,rhumbcoor) \
	       lcolor=$opt($id,1,rhumbcolor) " 
    }

    # check to see if options have changed
    foreach key $optlist {
        if {$opt($id,0,$key) != $opt($id,1,$key)} {
        	set opt($id,1,mod) 1
        	set opt($id,0,$key) $opt($id,1,$key)
        }
    } 
    
    # if options have change (or mod flag set by other procedures) re-render map
	if {$opt($id,1,mod) == 1 || $dup($id) == 1 || $first == 1} {
		runcmd "d.frame -e"
		if { $cmd != "" } { run_panel $cmd } 
		if { $cmd2 != "" } { run_panel $cmd2 } 
		if { $cmd3 != "" } { run_panel $cmd3 }     
	   	file rename -force $mapfile($mon) $lfile($id)
    	file rename -force $maskfile($mon) $lfilemask($id)
		# reset options changed flag
		set opt($id,1,mod) 0
		set dup($id) 0
		set first 0
	}

    #add lfile, maskfile, and opacity to compositing lists
    if { $opt($id,1,_check) } {

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
	}
}


proc GmGridline::duplicate { tree parent node id } {
    variable optlist
    variable lfile
    variable lfilemask
    variable opt
    variable count
	variable dup
	global guioptfont
	global iconpath
	global first

    set node "gridline:$count"
	set dup($count) 1

    set frm [ frame .gridlineicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmGridline::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo gico -file "$iconpath/module-d.grid.gif"
    set gdico [label $frm.gdico -image gico -bd 1 -relief raised]
    
    pack $check $gdico -side left


	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

     $tree insert $sellayer $parent $node \
	-text  "gridline $count"\
	-window    $frm \
	-drawcross auto  

	set opt($count,1,opacity) $opt($id,1,opacity)

	set optlist { _check gridcolor gridborder gridsize gridorigin griddraw gridgeod \
    			borderdraw textdraw rhumbdraw rhumbcoor geoddraw geodcoor geodcolor \
    			geodtxtcolor} 

    foreach key $optlist {
    	set opt($count,1,$key) $opt($id,1,$key)
		set opt($count,0,$key) $opt($count,1,$key)
    } 

	# create files in tmp directory for layer output
	set mappid [pid]
	set lfile($count) [eval exec "g.tempfile pid=$mappid"]
	set lfilemask($count) $lfile($count)
	append lfile($count) ".ppm"
	append lfilemask($count) ".pgm"

    incr count
    return $node
}