# 31 August 2005
# panel for d.rgb and d.his
# Michael Barton, Arizona State University

namespace eval GmRgbhis {
    variable array opt # rgbhis options
    variable count 1
}


proc GmRgbhis::create { tree parent } {
    variable opt
    variable count
    global gmpath

    set node "rgbhis:$count"

    set frm [ frame .rgbicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmRgbhis::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rgbico -file "$gmpath/rgbhis.gif"
    set ico [label $frm.ico -image rgbico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "RGB-HIS $count"\
	-window    $frm \
	-drawcross auto  
    
    set opt($count,_check) 1 
    set opt($count,map1) "" 
    set opt($count,map2) "" 
    set opt($count,map3) "" 
    set opt($count,overlay) 1 
    set opt($count,rgb) 1 
    set opt($count,his) 0 
    
    incr count
    return $node
}

proc GmRgbhis::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmRgbhis::select_map1 { id } {
    variable tree
    variable node
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRgbhis::opt($id,map1) $m
        GmTree::autonamel "RGB-HIS $m"
    }
}

proc GmRgbhis::select_map2 { id } {
    variable tree
    variable node
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRgbhis::opt($id,map2) $m
        GmTree::autonamel "RGB-HIS $m"
    }
}
proc GmRgbhis::select_map3 { id } {
    variable tree
    variable node
    set m [GSelect cell]
    if { $m != "" } { 
        set GmRgbhis::opt($id,map3) $m
        GmTree::autonamel "RGB-HIS $m"
    }
}
# display RGB and HIS options
proc GmRgbhis::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.rquery ]
    Label $row.a -text "Display 3 raster maps as Red Green Blue channels or Hue Intensity Saturation"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # raster1 name
    set row [ frame $frm.name1 ]
    Label $row.a -text "     "
    Button $row.b -text [G_msg "red (RGB) or hue (HIS):"] \
           -command "GmRgbhis::select_map1 $id"
    Entry $row.c -width 30 -text " $opt($id,map1)" \
          -textvariable GmRgbhis::opt($id,map1) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # raster2 name
    set row [ frame $frm.name2 ]
    Label $row.a -text "     "
    Button $row.b -text [G_msg "green (RGB) or intensity (HIS):"] \
           -command "GmRgbhis::select_map2 $id"
    Entry $row.c -width 30 -text " $opt($id,map2)" \
          -textvariable GmRgbhis::opt($id,map2) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # raster3 name
    set row [ frame $frm.name3 ]
    Label $row.a -text "     "
    Button $row.b -text [G_msg "blue (RGB) or saturation (HIS):"] \
           -command "GmRgbhis::select_map3 $id"
    Entry $row.c -width 30 -text " $opt($id,map3)" \
          -textvariable GmRgbhis::opt($id,map3) \
          -background white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
    
    # display type
    set row [ frame $frm.type ]
    checkbutton $row.a -text [G_msg "display maps as RGB"] -variable \
        GmRgbhis::opt($id,rgb) 
    Button $row.b -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.rgb" \
            -background $bgcolor \
            -helptext [G_msg "Help for RGB"]
    checkbutton $row.c -text [G_msg "display maps as HIS"] -variable \
        GmRgbhis::opt($id,his) 
    Button $row.d -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.his" \
            -background $bgcolor \
            -helptext [G_msg "Help for HIS"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # overlay
    set row [ frame $frm.over ]
    checkbutton $row.a -text [G_msg "overlay maps from other layers (transparent null value cells)"] -variable \
        GmRgbhis::opt($id,overlay) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

}

proc GmRgbhis::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check map1 map2 map3 rgb his overlay } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}


proc GmRgbhis::display { node } {
    variable opt
    variable tree
    global mon
    
    set line ""
    set input ""
    global gmpath
    set cmd1 ""
    set cmd2 ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map1) == "" } { return } 
    if { $opt($id,map2) == "" &&  $opt($id,map3) == "" && $opt($id,rgb) == 1 } { 
        return 
     } 
  
    if { $opt($id,rgb) == 1 } { 
        set cmd1 "d.rgb red=$opt($id,map1) green=$opt($id,map2) blue=$opt($id,map3)" 
     }

    if { $opt($id,his) == 1 } { 
        set cmd2 "d.his h_map=$opt($id,map1)" 
        if { $opt($id,map2) != "" } {        
            append cmd2 " i_map=$opt($id,map2)"
         }
        if { $opt($id,map3) != "" } {        
            append cmd2 " s_map=$opt($id,map2)"
         }
     }

    # overlay
    if { $opt($id,overlay) && $opt($id,rgb) == 1 } { 
        append cmd1 " -o"
    }
    if { $opt($id,overlay) && $opt($id,his) == 1 } { 
        append cmd2 " -n"
    }

    # display maps    
    if { $cmd1 != "" } { 
		run_panel $cmd1
    }
    
    if { $cmd2 != "" } { 
        run_panel $cmd2 
    } 
}



proc GmRgbhis::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "rgbhis:$count"

    set frm [ frame .rgbhisicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmRgbhis::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rgbico -file "$gmpath/rgbhis.gif"
    set ico [label $frm.ico -image rgbico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,map1) == ""} {
    	$tree insert end $parent $node \
		-text      "RGB-HIS $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "RGB-HIS $opt($id,map1)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)

    set opt($count,map1) "$opt($id,map1)" 
    set opt($count,map2) "$opt($id,map2)"  
    set opt($count,map3) "$opt($id,map3)"  
    set opt($count,overlay) $opt($id,overlay)
    set opt($count,rgb) $opt($id,rgb)
    set opt($count,his) $opt($id,his) 

    incr count
    return $node
}

proc GmRgbhis::query { node } {
    variable opt
    variable tree
    global mon
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map1) == "" } { return }
    if { $opt($id,map2) == "" } { return }
    if { $opt($id,map3) == "" } { return }

    set cmd "d.what.rast map=$opt($id,map1),$opt($id,map2),$opt($id,map3)"
    
    term $cmd
}
