###############################################################
# histogram.tcl - raster histogram display layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmHist {
    variable array opt # hist options
    variable count 1
    variable array tree # mon
}

source $gmpath/mapcanvas.tcl

proc GmHist::create { tree parent } {
    variable opt
    variable count
    #variable tree
    
    global gmpath
    global mon

    set node "histogram:$count"

    set frm [ frame .histicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmHist::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo hico -file "$gmpath/histogram.gif"
    set ico [label $frm.ico -image hico -bd 1 -relief raised]
    
    pack $check $ico -side left
        
    $tree insert end $parent $node \
	-text  "histogram $count"\
	-window    $frm \
	-drawcross auto  
    
    set opt($count,_check) 1 
    set opt($count,map) "" 
    set opt($count,color) "black" 
    set opt($count,style) "bar" 
    set opt($count,nsteps) 255 
    set opt($count,nulls) 0 
    
    incr count
    return $node
}

proc GmHist::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmHist::select_map { id } {
    variable tree
    variable node
    global mon
    
    set m [GSelect cell]
    if { $m != "" } { 
        set GmHist::opt($id,map) $m
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

    # raster name for histogram
    set row [ frame $frm.name ]
    Label $row.a -text "Raster to histogram: "
    Button $row.b -image [image create photo -file "$gmpath/raster.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmHist::select_map $id"
    Entry $row.c -width 35 -text " $opt($id,map)" \
          -textvariable GmHist::opt($id,map) \
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
    ComboBox $row.b -padx 2 -width 4 -textvariable GmHist::opt($id,style) \
		-values {"bar" "pie"} -entrybg white
    # histogram color
    Label $row.c -text "Histogram frame and text color"
    ComboBox $row.d -padx 2 -width 10 -textvariable GmHist::opt($id,color) \
		-values {"white" "grey" "gray" "black" "brown" "red" "orange" \
		"yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" \
		"magenta"} -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
    
    # steps for fp maps and nulls
    set row [ frame $frm.steps ]
    Label $row.a -text "Steps/bins for values (fp maps only)" 
    SpinBox $row.b -range {2 255 1} -textvariable GmHist::opt($id,nsteps) \
		-width 4 -helptext "steps/bins" -entrybg white 
    Label $row.c -text "   "
    checkbutton $row.d -text [G_msg "include null values"] \
        -variable GmHist::opt($id,nulls) 
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
}

proc GmHist::save { tree depth node } {
    variable opt
    global mon
    
    set id [GmTree::node_id $node]

    foreach key { 
    	_check map color style nsteps nulls } {
         GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc GmHist::display { node } {
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

    set cmd "d.histogram -q map=$opt($id,map) style=$opt($id,style) \
    	color=$opt($id,color)"

    # include nulls
    if { $opt($id,nulls) } { 
        append cmd " -n"
    }

    # set steps
    if { $opt($id,nsteps) != "" } { 
		set rt [open "|r.info map=$opt($id,map) -t" r]
		set rasttype [read $rt]
		close $rt
		if {[regexp -nocase ".=FCELL" $rasttype]} {
            append cmd " nsteps=$opt($id,nsteps)"
        }
	}
	
	run_panel $cmd 
}
    
proc GmHist::mapname { node } {
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

proc GmHist::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath
    global mon

    set node "hist:$count"

    set frm [ frame .histicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
		-variable GmHist::opt($count,_check) \
		-height 1 -padx 0 -width 0]

    image create photo hico -file "$gmpath/histogram.gif"
    set ico [label $frm.ico -image hico -bd 1 -relief raised]
    
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
    set opt($count,map) "$opt($id,map)" 
    set opt($count,color) "$opt($id,color)"
    set opt($count,style) "$opt($id,style)" 
    set opt($count,nsteps) "$opt($id,nsteps)"
    set opt($count,nulls) "$opt($id,nulls)"

    incr count
    return $node
}
