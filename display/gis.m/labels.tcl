###############################################################
# labels.tcl - vector labels layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmLabels {
    variable array opt # labels options
    variable count 1
    variable array tree # mon    
}


proc GmLabels::create { tree parent } {
    variable opt
    variable count 
    global gmpath

    set node "labels:$count"

    set frm [ frame .labelsicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmLabels::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo labels_ico -file "$gmpath/labels.gif"
    set ico [label $frm.ico -image labels_ico -bd 1 -relief raised]
    
    pack $check $ico -side left

    $tree insert end $parent $node \
	-text      "labels $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,_check) 1 

    set opt($count,map) "" 
    set opt($count,minreg) "" 
    set opt($count,maxreg) "" 
    set opt($count,ignore_rot) 0 

    incr count
    return $node
}

proc GmLabels::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmLabels::select_map { id } {
    set m [GSelect paint/labels]
    if { $m != "" } { 
        set GmLabels::opt($id,map) $m
        GmTree::autonamel $m
    }
}

# display labels options
proc GmLabels::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # labels name
    set row [ frame $frm.name ]
    Button $row.a -text [G_msg "Labels name:"] \
           -command "GmLabels::select_map $id"
    Entry $row.b -width 40 -text "$opt($id,map)" \
          -textvariable GmLabels::opt($id,map) \
          -background white
    Button $row.c -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.labels" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # display only in limited region size range
    set row [ frame $frm.region ]
    Label $row.a -text [G_msg "Display constraints:"]
    LabelEntry $row.b -label "min" -textvariable GmLabels::opt($id,minreg) \
            -width 8 -entrybg white
    LabelEntry $row.c -label "max" -textvariable GmLabels::opt($id,maxreg) \
            -width 8 -entrybg white
    Label $row.d -text [G_msg "region size"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # ignore rotation
    set row [ frame $frm.ignore_rot ]
    checkbutton $row.a -text [G_msg " ignore rotation setting and draw horizontally"] -variable \
        GmLabels::opt($id,ignore_rot) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
}

proc GmLabels::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]


    foreach key { _check map minreg maxreg } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc GmLabels::display { node } {
    variable opt
    variable tree
    global mon

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]
    
    if { ! ( $opt($id,_check) ) } { return } 
    if { $opt($id,map) == "" } { return } 

    set cmd "d.labels labels=$opt($id,map)"

    if { $opt($id,minreg) != "" } { 
        append cmd " minreg=$opt($id,minreg)"
    }
    if { $opt($id,maxreg) != "" } { 
        append cmd " maxreg=$opt($id,maxreg)"
    }

    run_panel $cmd
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
                           -variable GmLabels::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo labels_ico -file "$gmpath/labels.gif"
    set ico [label $frm.ico -image labels_ico -bd 1 -relief raised]
    
    pack $check $ico -side left
	
	if { $opt($id,map) == ""} {
    	$tree insert end $parent $node \
		-text      "labels $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "$opt($id,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check)  $opt($id,_check)

    set opt($count,map) "$opt($id,map)" 
    set opt($count,minreg) "$opt($id,minreg)" 
    set opt($count,maxreg) "$opt($id,maxreg)" 

    incr count
    return $node
}
