##########################################################################
# labels.tcl - vector labels layer options file for GRASS GIS Manager
# March 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmLabels {
    variable array opt # labels current options
    variable count 1
    variable array tree # mon    
    variable array lfile # raster
    variable array lfilemask # raster
    variable optlist
}


proc GmLabels::create { tree parent } {
    variable opt
    variable count 
    variable lfile
    variable lfilemask
    variable optlist
    global gmpath

    set node "labels:$count"

    set frm [ frame .labelsicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmLabels::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo labels_ico -file "$gmpath/labels.gif"
    set ico [label $frm.ico -image labels_ico -bd 1 -relief raised]
    
    pack $check $ico -side left

	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text      "labels $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,1,_check) 1 

    set opt($count,1,map) "" 
	set opt($count,1,opacity) 1.0
    set opt($count,1,minreg) "" 
    set opt($count,1,maxreg) "" 
    set opt($count,1,ignore_rot) 0 
    set opt($count,1,mod) 1

	set optlist { _check map minreg maxreg opacity}

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

proc GmLabels::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

proc GmLabels::select_labels { id } {
    set m [GSelect paint/labels]
    if { $m != "" } { 
        set GmLabels::opt($id,1,map) $m
        GmTree::autonamel $m
    }
}

# display labels options
proc GmLabels::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading ]
    Label $row.a -text "Display labels for vector objects (created with v.label)" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmLabels::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # labels name
    set row [ frame $frm.name ]
    Label $row.a -text [G_msg "Labels file:"]
    Button $row.b -image [image create photo -file "$gmpath/labels.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "labels file to display"] \
		-command "GmLabels::select_labels $id"
    Entry $row.c -width 40 -text "$opt($id,1,map)" \
		-textvariable GmLabels::opt($id,1,map) \
		-background white
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
		-image [image create photo -file "$gmpath/grass.gif"] \
		-command "run g.manual d.labels" \
		-background $bgcolor \
		-helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # display only in limited region size range
    set row [ frame $frm.region ]
    Label $row.a -text [G_msg "Display constraints:"]
    LabelEntry $row.b -label "min" -textvariable GmLabels::opt($id,1,minreg) \
            -width 8 -entrybg white
    LabelEntry $row.c -label "max" -textvariable GmLabels::opt($id,1,maxreg) \
            -width 8 -entrybg white
    Label $row.d -text [G_msg "region size"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # ignore rotation
    set row [ frame $frm.ignore_rot ]
    checkbutton $row.a -text [G_msg " ignore rotation setting and draw horizontally"] -variable \
        GmLabels::opt($id,1,ignore_rot) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # launch v.label
    set row [ frame $frm.vlabel ]
    Label $row.a -text "Launch v.label to create labels file" 
    Button $row.b -text [G_msg "v.label"] \
	    -command "execute v.label"
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

}

proc GmLabels::save { tree depth node } {
    variable opt
    variable optlist
    
    set id [GmTree::node_id $node]


    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}

proc GmLabels::display { node mod } {
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
    
    if { ! ( $opt($id,1,_check) ) } { return } 
    if { $opt($id,1,map) == "" } { return } 

    set cmd "d.labels labels=$opt($id,1,map)"

    if { $opt($id,1,minreg) != "" } { 
        append cmd " minreg=$opt($id,1,minreg)"
    }
    if { $opt($id,1,maxreg) != "" } { 
        append cmd " maxreg=$opt($id,1,maxreg)"
    }

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


proc GmLabels::query { node } {
    puts "Query not supported for Paint labels layer"
}

proc GmLabels::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "labels:$count"

    set frm [ frame .labelsicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmLabels::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo labels_ico -file "$gmpath/labels.gif"
    set ico [label $frm.ico -image labels_ico -bd 1 -relief raised]
    
    pack $check $ico -side left
	
	if { $opt($id,1,map) == ""} {
    	$tree insert end $parent $node \
		-text      "labels $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "$opt($id,1,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,1,_check)  $opt($id,1,_check)

    set opt($count,1,map) "$opt($id,1,map)" 
	set opt($count,1,opacity) opt($id,1,opacity)
    set opt($count,1,minreg) "$opt($id,1,minreg)" 
    set opt($count,1,maxreg) "$opt($id,1,maxreg)" 

    incr count
    return $node
}
