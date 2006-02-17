###############################################################
# maptext.tcl - TclTk canvas text layer options file for GRASS GIS Manager
# February 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmCtext {
    variable array opt # ctext options
    variable count 1
}

proc GmCtext::create { tree parent } {
    variable opt
    variable count
    global gmpath
    global frm

    set node "ctext:$count"

    set frm [ frame .ctexticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmCtext::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo ctico -file "$gmpath/maptext.gif"
    set ico [label $frm.ico -image ctico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "text layer $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,_check) 1 

    set opt($count,text) "" 
    set opt($count,xcoord) 100
    set opt($count,ycoord) 100
    set opt($count,font) "times 12" 
    set opt($count,fill) \#000000 
    set opt($count,width)  100
    set opt($count,anchor) "center_left" 
    set opt($count,justify) "left" 
    set opt($count,coordinates) "pixels" 
    
    incr count
    return $node
}

proc GmCtext::select_font { id } {
	global mon
	global frm
	variable opt
    
    set fon [SelectFont $frm.font -type dialog -sampletext 1 -title "Select font"]
	if { $fon != "" } {set opt($id,font) $fon}
}

proc GmCtext::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}


# ctext options
proc GmCtext::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading ]
    Label $row.a -text "Display text" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # text
    set row [ frame $frm.text ]
    Label $row.a -text "Text to display:"
    LabelEntry $row.b -textvariable GmCtext::opt($id,text) -width 50 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # coordinates1
    set row [ frame $frm.east_north ]
    Label $row.a -text "Text placement:   x & y coordinates (from upper left) "
    LabelEntry $row.b -textvariable GmCtext::opt($id,xcoord) -width 8 \
            -entrybg white
    LabelEntry $row.c -textvariable GmCtext::opt($id,ycoord) -width 8 \
            -entrybg white
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
        
    # coordinates2
    set row [ frame $frm.textcoord2 ]
    Label $row.a -text [G_msg "     coordinate type for text placement"] 
    ComboBox $row.b -padx 2 -width 10 -textvariable GmCtext::opt($id,coordinates) \
                    -values {"pixels" "percent" "geographic" } -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
        
    # text options1
    set row [ frame $frm.textopt1 ]
    Label $row.a -text [G_msg "     align text with coordinate point  "] 
    ComboBox $row.b -padx 2 -width 12 -textvariable GmCtext::opt($id,anchor) \
                    -values {"lower_left" "bottom_center" "lower_right" "center_left" "center" 
                    "center_right" "upper_left" "top_center" "upper_right" } \
                    -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # text options2
    set row [ frame $frm.textopt2 ]
    Label $row.a -text [G_msg "     justification"] 
    ComboBox $row.b -padx 2 -width 7 -textvariable GmCtext::opt($id,justify) \
                    -values {"left" "center" "right"} -entrybg white
    Label $row.c -text "  line width"
    LabelEntry $row.d -textvariable GmCtext::opt($id,width) -width 5 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
        
    # select font
    set row [ frame $frm.font ]
    Label $row.a -text [G_msg "Font:"] 
    Button $row.b -image [image create photo -file "$gmpath/font.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "select font for text"] \
	    -command "GmCtext::select_font $id"
    Entry $row.c -width 15 -text "$opt($id,font)" \
	    -textvariable GmCtext::opt($id,font) \
	    -background white 
    Label $row.d -text [G_msg "  color"] 
    SelectColor $row.e -type menubutton -variable GmCtext::opt($id,fill)
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes


}

proc GmCtext::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check text east_north font path charset color \
            size align rotation linespacing bold textcoord radians htpixel } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc GmCtext::display { node } {
    variable opt
    variable tree
    variable can
    global mon
    global gmpath
    global canvas_w
    global canvas_h

    set line ""
    set input ""
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set can($mon) $MapCanvas::can($mon)
    
    
    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,text) == "" } { return } 
        
    switch $opt($id,anchor) {
    	"lower_left" 	{ set anchor "sw"}
    	"bottom_center" { set anchor "s" }
    	"lower_right" 	{ set anchor "se"}
    	"center_left" 	{ set anchor "w" }
    	"center" 		{ set anchor "center" }
    	"center_right" 	{ set anchor "e" }
    	"upper_left" 	{ set anchor "nw"}
    	"top_center" 	{ set anchor "n" }
    	"upper_right" 	{ set anchor "ne"}
    }
        
    if {$opt($id,coordinates) == "geographic"} {
		set xcoord [MapCanvas::mape2scrx $opt($id,xcoord)]
		set ycoord [MapCanvas::mapn2scry $opt($id,ycoord)]
		$can($mon) create text $xcoord $ycoord \
			-anchor $anchor \
			-justify $opt($id,justify) \
			-width $opt($id,width) \
			-fill $opt($id,fill) \
			-font $opt($id,font) \
			-text $opt($id,text)
    } elseif {$opt($id,coordinates) == "percent"} {
		set xpct [expr ($opt($id,xcoord) / 100.0) * $canvas_w($mon)]
		set ypct [expr ($opt($id,ycoord) / 100.0) * $canvas_h($mon)]
		$can($mon) create text $xpct $ypct \
			-anchor $anchor \
			-justify $opt($id,justify) \
			-width $opt($id,width) \
			-fill $opt($id,fill) \
			-font $opt($id,font) \
			-text $opt($id,text)
    } else {
		$can($mon) create text $opt($id,xcoord) $opt($id,ycoord) \
			-anchor $anchor \
			-justify $opt($id,justify) \
			-width $opt($id,width) \
			-fill $opt($id,fill) \
			-font $opt($id,font) \
			-text $opt($id,text)
	}
}


proc GmCtext::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "ctext:$count"

    set frm [ frame .ctexticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmCtext::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo ctico -file "$gmpath/maptext.gif"
    set ico [label $frm.ico -image ctico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,text) == ""} {
    	$tree insert end $parent $node \
		-text      "text layer $count" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)
    set opt($count,text) $opt($id,text) 
    set opt($count,xcoord) $opt($id,xcoord)
    set opt($count,ycoord) $opt($id,ycoord)
    set opt($count,font) $opt($id,font)
    set opt($count,fill) $opt($id,fill)
    set opt($count,width) $opt($id,width)
    set opt($count,anchor) $opt($id,anchor)
    set opt($count,justify) $opt($id,justify)
    set opt($count,coordinates) $opt($id,coordinates)

    incr count
    return $node
}
