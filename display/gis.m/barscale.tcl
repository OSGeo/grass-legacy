# 4 Sept 2005
# panel for d.barscale
# Michael Barton, Arizona State University

namespace eval GmBarscale {
    variable array opt # barscale options
    variable count 1
}


proc GmBarscale::create { tree parent } {
    variable opt
    variable count
    global mon
    global gmpath

    set node "barscale:$count"

    set frm [ frame .barscaleicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmBarscale::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo scaleico -file "$gmpath/barscale.gif"
    set ico [label $frm.ico -image scaleico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "scale $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,_check) 1 

    set opt($count,tcolor) \#000000 
    set opt($count,bcolor) \#FFFFFF 
    set opt($count,bcolor_none) 0
    set opt($count,line) 0 
    set opt($count,at) "2,2" 
    set opt($count,feet) 0 
    set opt($count,top) 0 
    set opt($count,mouse) 0 
    
    incr count
    return $node
}

proc GmBarscale::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}


# barscale options
proc GmBarscale::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # color
    set row [ frame $frm.color ]
    Label $row.a -text [G_msg "Text color: "] 
    SelectColor $row.b -type menubutton -variable GmBarscale::opt($id,tcolor)
    Label $row.c -text [G_msg " Background color: "] 
    SelectColor $row.d -type menubutton -variable GmBarscale::opt($id,bcolor)
    checkbutton $row.e -text [G_msg "no background color"] -variable \
        GmBarscale::opt($id,bcolor_none) 
    Label $row.f -text [G_msg " "] 
    Button $row.g -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.barscale" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e $row.f $row.g -side left
    pack $row -side top -fill both -expand yes
    
    # at
    set row [ frame $frm.at ]
    Label $row.a -text "Place left corner of scale at 0-100% from top left of monitor (x,y)"
    LabelEntry $row.b -textvariable GmBarscale::opt($id,at) -width 8 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
        
    # scale options
    set row [ frame $frm.opts ]
    checkbutton $row.a -text [G_msg "line scale instead of bar scale"] -variable \
        GmBarscale::opt($id,line) 
    checkbutton $row.b -text [G_msg "text on top of scale, instead of to right"] -variable \
        GmBarscale::opt($id,top) 
    pack $row.a $row.b  -side left
    pack $row -side top -fill both -expand yes

    # english units
    set row [ frame $frm.units ]
    checkbutton $row.a -text [G_msg "use feet/miles instead of meters"] -variable \
        GmBarscale::opt($id,feet) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # mouse
    set row [ frame $frm.mouse ]
    checkbutton $row.a -text \
        [G_msg "place with mouse (cannot save placement with group)"] \
        -variable GmBarscale::opt($id,mouse) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
}



proc GmBarscale::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check bcolor bcolor_none tcolor at feet line top mouse } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}


proc GmBarscale::display { node } {
    variable opt
    variable tree
    set line ""
    set input ""
    global gmpath
    set cmd ""
    global mon

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]


    if { ! ( $opt($id,_check) ) } { return } 

    # set hex colors to rgb         
    set tcolor [Gm::color $opt($id,tcolor)]
    set bcolor [Gm::color $opt($id,bcolor)]

    # no background color
    if { $opt($id,bcolor_none) == 1 } { 
        set bcolor "none"
    }

    set cmd "d.barscale tcolor=$tcolor bcolor=$bcolor "

    # line scale
    if { $opt($id,line) != 0 } { 
        append cmd " -l"
    }

    # text on top
    if { $opt($id,top) != 0 } { 
        append cmd " -t"
    }

    # english units
    if { $opt($id,feet) != 0} { 
        append cmd " -f"
    }

    # place with coordinates
    if { $opt($id,at) != "" && $opt($id,mouse) == 0 } { 
        append cmd " at=$opt($id,at)"
        run_panel $cmd
    }

    # place with mouse
    if { $opt($id,mouse) != 0 } { 
        append cmd " -m"
        term_panel $cmd
    }
    
    
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
                           -variable GmBarscale::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo scaleico -file "$gmpath/barscale.gif"
    set ico [label $frm.ico -image scaleico -bd 1 -relief raised]
    
    pack $check $ico -side left

    $tree insert end $parent $node \
		-text      "scale $count" \
		-window    $frm \
		-drawcross auto

    set opt($count,_check) $opt($id,_check)

    set opt($count,tcolor) "$opt($id,tcolor)" 
    set opt($count,bcolor) "$opt($id,bcolor)" 
    set opt($count,line) "$opt($id,line)" 
    set opt($count,at) "$opt($id,at)"
    set opt($count,feet) "$opt($id,feet)"
    set opt($count,top) "$opt($id,top)"
    set opt($count,mouse) "$opt($id,mouse)" 

    incr count
    return $node
}
