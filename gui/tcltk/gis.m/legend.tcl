##########################################################################
# legend.tcl - raster legend layer options file for GRASS GIS Manager
# March 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmLegend {
    variable array opt # legend current options
    variable count 1
    variable array lfile # raster
    variable array lfilemask # raster
    variable optlist
}


proc GmLegend::create { tree parent } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist
    global gmpath

    set node "legend:$count"

    set frm [ frame .legendicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmLegend::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo legico -file "$gmpath/legend.gif"
    set ico [label $frm.ico -image legico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "legend $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,1,_check) 1 
    set opt($count,1,map) "" 
	set opt($count,1,opacity) 1.0
    set opt($count,1,color) "black" 
    set opt($count,1,lines) 0 
    set opt($count,1,thin) 1 
    set opt($count,1,labelnum) 5 
    set opt($count,1,at) "5,95,5,10" 
    set opt($count,1,use) "" 
    set opt($count,1,range) "" 
    set opt($count,1,nolbl) 0 
    set opt($count,1,noval) 0 
    set opt($count,1,skip) 0 
    set opt($count,1,smooth) 0 
    set opt($count,1,flip) 0 
    set opt($count,1,mod) 1
    
	set optlist { _check map color lines thin labelnum at use range \
             nolbl noval skip smooth flip opacity}
             
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

proc GmLegend::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

proc GmLegend::select_map { id } {
    variable tree
    variable node
    set m [GSelect cell]
    if { $m != "" } { 
        set GmLegend::opt($id,1,map) $m
        GmTree::autonamel "legend for $m"
    }
}

# legend options
proc GmLegend::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Display legend for raster map using cat values and labels" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmLegend::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # raster name
    set row [ frame $frm.map ]
    Label $row.a -text "Raster map: "
    Button $row.b -image [image create photo -file "$gmpath/raster.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmLegend::select_map $id"
    Entry $row.c -width 35 -text " $opt($id,1,map)" \
          -textvariable GmLegend::opt($id,1,map) \
          -background white
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.legend" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # placement
    set row [ frame $frm.at1 ]
    Label $row.a -text "    legend placement as 0-100% of display height/width from bottom left"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    set row [ frame $frm.at2 ]
    Label $row.a -text "    set legend corners (bottom,top,left,right)"
    LabelEntry $row.b -textvariable GmLegend::opt($id,1,at) -width 15 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # text color
    set row [ frame $frm.color ]
    Label $row.a -text [G_msg "Legend appearance: text color"] 
    ComboBox $row.b -padx 0 -width 10 -textvariable GmLegend::opt($id,1,color) \
		-values {"white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
		-entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # no category labels or numbers
    set row [ frame $frm.cats ]
    Label $row.a -text "    " 
    checkbutton $row.b -text [G_msg "do not display labels"] -variable \
        GmLegend::opt($id,1,nolbl) 
    checkbutton $row.c -text [G_msg "do not display values"] -variable \
        GmLegend::opt($id,1,noval) 
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # display lines
    set row [ frame $frm.lines ]
    Label $row.a -text "    number of lines (0=display all):" 
    SpinBox $row.b -range {0 1000 1} -textvariable GmLegend::opt($id,1,lines) \
		-entrybg white -width 5 -helptext "Lines to display" 
    Label $row.c -text "  " 
    checkbutton $row.d -text [G_msg "invert legend"] -variable \
        GmLegend::opt($id,1,flip) 
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
    
    # thin
    set row [ frame $frm.thin ]
    Label $row.a -text "    interval between categories (integer maps)" 
    SpinBox $row.b -range {1 1000 1} -textvariable GmLegend::opt($id,1,thin) \
		-entrybg white -width 5 -helptext "Thinning interval" 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # labelnum
    set row [ frame $frm.labelnum ]
    Label $row.a -text "  " 
    checkbutton $row.b -text [G_msg "draw smooth gradient (fp maps)"] -variable \
        GmLegend::opt($id,1,smooth) 
    Label $row.c -text "with maximum of" 
    SpinBox $row.d -range {2 100 1} -textvariable GmLegend::opt($id,1,labelnum) \
                   -entrybg white -width 4 -helptext "Maximum lines to display for gradient" 
    Label $row.e -text "lines" 
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes
            
	# display subset of values
    set row [ frame $frm.subset ]
    Label $row.a -text "Display legend for subset of raster values"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # skip
    set row [ frame $frm.opts ]
    Label $row.a -text "  " 
    checkbutton $row.b -text [G_msg "skip categories with no labels"] -variable \
        GmLegend::opt($id,1,skip) 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # use cats
    set row [ frame $frm.use ]
    Label $row.a -text "    legend for only these categories     "
    LabelEntry $row.b -textvariable GmLegend::opt($id,1,use) -width 28 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # range
    set row [ frame $frm.range ]
    Label $row.a -text "    legend for only this range of values"
    LabelEntry $row.b -textvariable GmLegend::opt($id,1,range) -width 28 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

	set opt($id,1,mod) "1"
}

proc GmLegend::save { tree depth node } {
    variable opt
    variable optlist
    
    set id [GmTree::node_id $node]

    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}


proc GmLegend::display { node mod } {
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
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set opt($id,1,mod) $mod    

    if { $opt($id,1,map) == "" } { return } 
    set cmd "d.legend map=$opt($id,1,map) color=$opt($id,1,color) \
            lines=$opt($id,1,lines) thin=$opt($id,1,thin) \
            labelnum=$opt($id,1,labelnum) at=$opt($id,1,at)"

    # use cats
    if { $opt($id,1,use) != "" } { 
        append cmd " use=$opt($id,1,use)"
    }

    # range
    if { $opt($id,1,range) != "" } { 
        append cmd " range=$opt($id,1,range)"
    }

    # nolbl
    if { $opt($id,1,nolbl) != 0 } { 
        append cmd " -v"
    }

    # noval
    if { $opt($id,1,noval) != 0 } { 
        append cmd " -c"
    }

    # skip
    if { $opt($id,1,skip) != 0} { 
        append cmd " -n"
    }

    # smooth
    if { $opt($id,1,smooth) != 0 } { 
        append cmd " -s"
    }
        
    # flip
    if { $opt($id,1,flip) != 0 } { 
        append cmd " -f"
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


proc GmLegend::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "legend:$count"

    set frm [ frame .legendicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmLegend::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo legico -file "$gmpath/legend.gif"
    set ico [label $frm.ico -image legico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,1,map) == ""} {
    	$tree insert end $parent $node \
		-text      "legend $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "legend for $opt($id,1,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,1,_check) $opt($id,1,_check)

    set opt($count,1,map) "$opt($id,1,map)" 
	set opt($count,1,opacity) opt($id,1,opacity)
    set opt($count,1,color) "$opt($id,1,color)" 
    set opt($count,1,lines) "$opt($id,1,lines)" 
    set opt($count,1,thin) "$opt($id,1,thin)" 
    set opt($count,1,labelnum) "$opt($id,1,labelnum)"
    set opt($count,1,at) "$opt($id,1,at)"
    set opt($count,1,use) "$opt($id,1,use)"
    set opt($count,1,range) "$opt($id,1,range)"
    set opt($count,1,nolbl) "$opt($id,1,nolbl)" 
    set opt($count,1,noval) "$opt($id,1,noval)" 
    set opt($count,1,skip) "$opt($id,1,skip)" 
    set opt($count,1,smooth) "$opt($id,1,smooth)"
    set opt($count,1,flip) "$opt($id,1,flip)" 

    incr count
    return $node
}
