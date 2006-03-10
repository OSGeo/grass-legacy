##########################################################################
# barscale.tcl - barscale layer options file for GRASS GIS Manager
# March 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmBarscale {
    variable array opt # barscale current options
    variable count 1
    variable array lfile # rgbhis
    variable array lfilemask # rgbhis
    variable optlist
    variable first
}


proc GmBarscale::create { tree parent } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist
    variable first
    global mon
    global gmpath

    set node "barscale:$count"

    set frm [ frame .barscaleicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmBarscale::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo scaleico -file "$gmpath/barscale.gif"
    set ico [label $frm.ico -image scaleico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "scale $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,1,_check) 1 

	set opt($count,1,opacity) 1.0
    set opt($count,1,tcolor) \#000000 
    set opt($count,1,bcolor) \#FFFFFF 
    set opt($count,1,bcolor_none) 0
    set opt($count,1,line) 0 
    set opt($count,1,at) "2,2" 
    set opt($count,1,feet) 0 
    set opt($count,1,top) 0 
    set opt($count,1,arrow) 0 
    set opt($count,1,scale) 0 
    set opt($count,1,mod) 1
    set first 1
    
    set optlist { _check bcolor bcolor_none tcolor at feet line top arrow \
    	scale opacity}
    
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

proc GmBarscale::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}


# barscale options
proc GmBarscale::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Display scale and north arrow" \
    	-fg MediumBlue    
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmBarscale::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # color
    set row [ frame $frm.color ]
    Label $row.a -text [G_msg "Scale appearance:  text color"] 
    SelectColor $row.b -type menubutton -variable GmBarscale::opt($id,1,tcolor)
    Label $row.c -text [G_msg "   "] 
    Button $row.d -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.barscale" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
    
    # background
    set row [ frame $frm.background ]
    Label $row.a -text [G_msg "    background color "] 
    SelectColor $row.b -type menubutton -variable GmBarscale::opt($id,1,bcolor)
    Label $row.c -text [G_msg "   "] 
    checkbutton $row.d -text [G_msg "transparent background"] \
    	-variable GmBarscale::opt($id,1,bcolor_none) 
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # arrow or scale only
    set row [ frame $frm.arrow ]
    Label $row.a -text [G_msg "    "] 
    checkbutton $row.b -text [G_msg "display N. arrow only"] \
    	-variable GmBarscale::opt($id,1,arrow) 
    checkbutton $row.c -text [G_msg "display scale only"] \
    	-variable GmBarscale::opt($id,1,scale) 
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # text on top
    set row [ frame $frm.textontop ]
    Label $row.a -text [G_msg "    "] 
    checkbutton $row.b -text [G_msg "text on top of scale, instead of to right"] \
    	-variable GmBarscale::opt($id,1,top) 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # scale options
    set row [ frame $frm.opts ]
    Label $row.a -text [G_msg "    "] 
    checkbutton $row.b -text [G_msg "line scale instead of bar"] \
    	-variable GmBarscale::opt($id,1,line) 
    checkbutton $row.c -text [G_msg "use feet/miles instead of meters"] \
    	-variable GmBarscale::opt($id,1,feet) 
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # at
    set row [ frame $frm.at1 ]
    Label $row.a -text "Scale placement: 0-100% from top left of display"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
        
    # at
    set row [ frame $frm.at2 ]
    Label $row.a -text "    enter x,y for scale lower left corner"
    LabelEntry $row.b -textvariable GmBarscale::opt($id,1,at) -width 8 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}



proc GmBarscale::save { tree depth node } {
    variable opt
    variable optlist
    global mon
    
    set id [GmTree::node_id $node]

    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}


proc GmBarscale::display { node mod } {
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
    variable first

 	set line ""
    set input ""
    global gmpath
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set opt($id,1,mod) $mod

    # set hex colors to rgb         
    set tcolor [Gm::color $opt($id,1,tcolor)]
    set bcolor [Gm::color $opt($id,1,bcolor)]

    # no background color
    if { $opt($id,1,bcolor_none) == 1 } { 
        set bcolor "none"
    }

    set cmd "d.barscale tcolor=$tcolor bcolor=$bcolor at=$opt($id,1,at)"

    # line scale
    if { $opt($id,1,line) != 0 } { 
        append cmd " -l"
    }

    # text on top
    if { $opt($id,1,top) != 0 } { 
        append cmd " -t"
    }

    # english units
    if { $opt($id,1,feet) != 0} { 
        append cmd " -f"
    }

	# arrow only
	if { $opt($id,1,arrow) != 0 } {
		append cmd " -n"
	}
	
	# scale only
	if { $opt($id,1,scale) != 0 } {
		append cmd " -s"
	}

    # check to see if options have changed
    foreach key $optlist {
        if {$opt($id,0,$key) != $opt($id,1,$key)} {
        	set opt($id,1,mod) 1
        	set opt($id,0,$key) $opt($id,1,$key)
        }
    } 

    # if options have change (or mod flag set by other procedures) re-render map
	if {$opt($id,1,mod) == 1 || $first == 1 } {
		run_panel $cmd
		set first 0
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


proc GmBarscale::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath
    global mon

    set node "barscale:$count"

    set frm [ frame .barscaleicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmBarscale::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo scaleico -file "$gmpath/barscale.gif"
    set ico [label $frm.ico -image scaleico -bd 1 -relief raised]
    
    pack $check $ico -side left

    $tree insert end $parent $node \
		-text      "scale $count" \
		-window    $frm \
		-drawcross auto

    set opt($count,1,_check) $opt($id,1,_check)

	set opt($count,1,opacity) opt($id,1,opacity)
    set opt($count,1,tcolor) "$opt($id,1,tcolor)" 
    set opt($count,1,bcolor) "$opt($id,1,bcolor)" 
    set opt($count,1,line) "$opt($id,1,line)" 
    set opt($count,1,at) "$opt($id,1,at)"
    set opt($count,1,feet) "$opt($id,1,feet)"
    set opt($count,1,top) "$opt($id,1,top)"

    incr count
    return $node
}
