###############################################################
# dtext.tcl - standard text layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmDtext {
    variable array opt # dtext options
    variable count 1
}


proc GmDtext::create { tree parent } {
    variable opt
    variable count
    global gmpath

    set node "dtext:$count"

    set frm [ frame .dtexticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmDtext::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo dtxtico -file "$gmpath/dtext.gif"
    set ico [label $frm.ico -image dtxtico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "text $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,_check) 1 

    set opt($count,text) "" 
    set opt($count,at) "" 
    set opt($count,line) "" 
    set opt($count,color) \#000000 
    set opt($count,size) 5 
    set opt($count,bold) 0 
    
    incr count
    return $node
}

proc GmDtext::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmDtext::select_file { id } {
    variable tree
    variable node
    set m [GSelect file]
    if { $m != "" } { 
        set GmDtext::opt($id,path) $m
    }
}


# dtext options
proc GmDtext::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # text
    set row [ frame $frm.text ]
    Label $row.a -text "Text to display:"
    LabelEntry $row.b -textvariable GmDtext::opt($id,text) -width 51 \
            -entrybg white
    Button $row.c -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.text" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
    
    # placement1
    set row [ frame $frm.at ]
    Label $row.a -text "Text placement: as % of display from lower left (x,y)"
    LabelEntry $row.b -textvariable GmDtext::opt($id,at) -width 10 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
        
    # placement2
    set row [ frame $frm.line ]
    Label $row.a -text "     by line number from top (1-1000)"
    LabelEntry $row.b -textvariable GmDtext::opt($id,line) -width 10 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # font options
    set row [ frame $frm.fontopt ]
    Label $row.a -text [G_msg "Text options: color"] 
    SelectColor $row.b -type menubutton -variable GmDtext::opt($id,color)
    Label $row.c -text " text height (% of display)" 
    SpinBox $row.d -range {1 100 1} -textvariable GmDtext::opt($id,size) \
                   -entrybg white -width 3 
    checkbutton $row.e -padx 10 -text [G_msg "bold text"] -variable \
        GmDtext::opt($id,bold) 
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

}

proc GmDtext::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check text at line color size bold } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}




proc GmDtext::display { node } {
    variable opt
    variable tree
    global mon
    set line ""
    set input ""
    global gmpath
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    # set hex colors to rgb         
    set color [Gm::color $opt($id,color)]
    

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,text) == "" } { return } 
    
    set cmd "echo $opt($id,text) | d.text color=$color size=$opt($id,size) "

    # at
    if { $opt($id,at) != "" } { 
        append cmd " {at=$opt($id,at)}"
    }

    # line
    if { $opt($id,line) != "" } { 
        append cmd " line=$opt($id,line)"
    }


    # bold text
    if { $opt($id,bold) != 0 } { 
        append cmd " -b"
    }
    
#    eval "exec echo $opt($id,text) | $cmd"

	run_panel $cmd
    
}



proc GmDtext::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "dtext:$count"

    set frm [ frame .dtexticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmDtext::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo dtxtico -file "$gmpath/dtext.gif"
    set ico [label $frm.ico -image dtxtico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,text) == ""} {
    	$tree insert end $parent $node \
		-text      "text $count" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)

    set opt($count,text) $opt($id,text) 
    set opt($count,at) $opt($id,at) 
    set opt($count,line) $opt($id,line) 
    set opt($count,color) $opt($id,color) 
    set opt($count,size) $opt($id,size) 
    set opt($count,bold) $opt($id,bold)

    incr count
    return $node
}
