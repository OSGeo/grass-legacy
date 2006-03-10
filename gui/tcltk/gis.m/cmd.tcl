##########################################################################
# cmd.tcl - command layer options file for GRASS GIS Manager
# March 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmCmd {
    variable array opt # cmd current options
    variable count 1
    variable array tree # mon
    variable array lfile # raster
    variable array lfilemask # raster
    variable optlist
}


proc GmCmd::create { tree parent } {
    variable opt
    variable count 
    variable lfile
    variable lfilemask
    variable optlist
    global gmpath

    set node "cmd:$count"

    set frm [ frame .cmdicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmCmd::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo cico -file "$gmpath/cmd.gif"
    set ico [label $frm.ico -image cico -bd 1 -relief raised]
    
    pack $check $ico -side left

	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text      "cmd $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,1,_check) 1 
	set opt($count,1,opacity) 1.0
    set opt($count,1,cmd) ""
    set opt($count,1,mod) 1

	set optlist { _check cmd opacity}

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

proc GmCmd::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

# display cmd options
proc GmCmd::options { id frm } {
    variable opt

    # Panel heading
    set row [ frame $frm.heading ]
    Label $row.a -text "Enter any GRASS command" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmCmd::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # cmd name
    set row [ frame $frm.name ]
    Label $row.a -text [G_msg "Command:"] 
    Entry $row.b -width 50 -text "$opt($id,1,cmd)" \
          -textvariable GmCmd::opt($id,1,cmd) \
          -background white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmCmd::save { tree depth node } {
    variable opt
    variable optlist
    
    set id [GmTree::node_id $node]

    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}

proc GmCmd::display { node mod} {
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
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set opt($id,1,mod) $mod    

    if { $opt($id,1,cmd) == "" } { return } 

    set cmd $opt($id,1,cmd)

    # check to see if options have changed
    foreach key $optlist {
        if {$opt($id,0,$key) != $opt($id,1,$key)} {
        	set opt($id,1,mod) 1
        	set opt($id,0,$key) $opt($id,1,$key)
        }
    } 
    
    # if options have change (or mod flag set by other procedures) re-render map
	if {$opt($id,1,mod) == 1} {
		run_panel $cmd
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

proc GmCmd::duplicate { tree parent node id} {
    variable opt
    variable count 
    global gmpath

    set node "cmd:$count"

    set frm [ frame .cmdicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmCmd::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo cico -file "$gmpath/cmd.gif"
    set ico [label $frm.ico -image cico -bd 1 -relief raised]
    
    pack $check $ico -side left

    $tree insert end $parent $node \
	-text      "cmd $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,1,_check) 1 

    set opt($count,1,cmd) "$opt($id,1,cmd)" 
	set opt($count,1,opacity) opt($id,1,opacity)

    incr count
    return $node
}