##########################################################################
# histogram.tcl - raster histogram display layer options file for GRASS GIS Manager
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmHist {
    variable array opt # hist current options
    variable count 1
    variable array tree # mon
    variable array tree # mon
    variable array lfile # raster
    variable array lfilemask # raster
    variable optlist
}

source $gmpath/mapcanvas.tcl

proc GmHist::create { tree parent } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist    
    global gmpath
    global mon

    set node "histogram:$count"

    set frm [ frame .histicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmHist::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo hico -file "$gmpath/histogram.gif"
    set ico [label $frm.ico -image hico -bd 1 -relief raised]
    
    pack $check $ico -side left
        
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "histogram $count"\
	-window    $frm \
	-drawcross auto  
    
    set opt($count,1,_check) 1 
    set opt($count,1,map) "" 
	set opt($count,1,opacity) 1.0
    set opt($count,1,color) "black" 
    set opt($count,1,style) "bar" 
    set opt($count,1,nsteps) 255 
    set opt($count,1,nulls) 0 
    
    set opt($count,1,mod) 1

	set optlist {_check map color style nsteps nulls opacity}
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

proc GmHist::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

proc GmHist::select_map { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmHist::opt($id,1,map) $m
        GmTree::autonamel "histogram of $m"
    }
}

# display histogram options
proc GmHist::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading ]
    Label $row.a -text "Draw histogram of values from raster map or image" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmHist::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # raster name for histogram
    set row [ frame $frm.name ]
    Label $row.a -text "Raster to histogram: "
    Button $row.b -image [image create photo -file "$gmpath/raster.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmHist::select_map $id"
    Entry $row.c -width 35 -text " $opt($id,1,map)" \
          -textvariable GmHist::opt($id,1,map) \
          -background white
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
		-image [image create photo -file "$gmpath/grass.gif"] \
		-command "run g.manual d.histogram" \
		-background $bgcolor -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # graph style and color
    set row [ frame $frm.style ]
    Label $row.a -text "Graph style"
    ComboBox $row.b -padx 2 -width 4 -textvariable GmHist::opt($id,1,style) \
		-values {"bar" "pie"} -entrybg white
    # histogram color
    Label $row.c -text "Histogram frame and text color"
    ComboBox $row.d -padx 2 -width 10 -textvariable GmHist::opt($id,1,color) \
		-values {"white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
    
    # steps for fp maps and nulls
    set row [ frame $frm.steps ]
    Label $row.a -text "Steps/bins for values (fp maps only)" 
    SpinBox $row.b -range {2 255 1} -textvariable GmHist::opt($id,1,nsteps) \
		-width 4 -helptext "steps/bins" -entrybg white 
    Label $row.c -text "   "
    checkbutton $row.d -text [G_msg "include null values"] \
        -variable GmHist::opt($id,1,nulls) 
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
}

proc GmHist::save { tree depth node } {
    variable opt
    variable optlist
    global mon
    
    set id [GmTree::node_id $node]

    foreach key $optlist {
         GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}

proc GmHist::display { node mod } {
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
    global gmpath
    global mon

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set opt($id,1,mod) $mod    


    if { $opt($id,1,map) == "" } { return } 

    set cmd "d.histogram -q map=$opt($id,1,map) style=$opt($id,1,style) \
    	color=$opt($id,1,color)"

    # include nulls
    if { $opt($id,1,nulls) } { 
        append cmd " -n"
    }

    # set steps
    if { $opt($id,1,nsteps) != "" } { 
		set rt [open "|r.info map=$opt($id,1,map) -t" r]
		set rasttype [read $rt]
		close $rt
		if {[regexp -nocase ".=FCELL" $rasttype]} {
            append cmd " nsteps=$opt($id,1,nsteps)"
        }
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
    
proc GmHist::mapname { node } {
    variable opt
    variable tree
    global mon
    global mapname
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,1,_check) ) } { return } 

    if { $opt($id,1,map) == "" } { return } 
    
    set mapname $opt($id,1,map)
    return $mapname
}

proc GmHist::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath
    global mon

    set node "hist:$count"

    set frm [ frame .histicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
		-variable GmHist::opt($count,1,_check) \
		-height 1 -padx 0 -width 0]

    image create photo hico -file "$gmpath/histogram.gif"
    set ico [label $frm.ico -image hico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,1,map) == ""} {
    	$tree insert end $parent $node \
		-text      "histogram $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "histogram for $opt($id,1,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,1,_check) $opt($id,1,_check)
    set opt($count,1,map) "$opt($id,1,map)" 
	set opt($count,1,opacity) opt($id,1,opacity)
    set opt($count,1,color) "$opt($id,1,color)"
    set opt($count,1,style) "$opt($id,1,style)" 
    set opt($count,1,nsteps) "$opt($id,1,nsteps)"
    set opt($count,1,nulls) "$opt($id,1,nulls)"

    incr count
    return $node
}
