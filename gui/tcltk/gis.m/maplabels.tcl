##########################################################################
# maplabels.tcl - options panel for TclTk postscript labels layer for GRASS GIS Manager
# April 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmCLabels {
    variable array opt # labels current options
    variable count 1
    variable array tree # mon    
    variable optlist
    variable array dup # layer
}


proc GmCLabels::create { tree parent } {
    variable opt
    variable count
    variable optlist
	variable dup
    global mon
    global iconpath

    set node "clabels:$count"

    set frm [ frame .clabelsicon$count]
    set check [checkbutton $frm.check \
		-variable GmCLabels::opt($count,1,_check) \
		-height 1 -padx 0 -width 0]

    image create photo clabels_ico -file "$iconpath/gui-maplabels.gif"
    set ico [label $frm.ico -image clabels_ico -bd 1 -relief raised]

    bind $ico <ButtonPress-1> "GmTree::selectn $tree $node"

    pack $check $ico -side left

	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text      "PS labels $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,1,_check) 1 
    set dup($count) 0

    set opt($count,1,xcoord) 1.0 
    set opt($count,1,ycoord) 1.0 
    set opt($count,1,xoffset) 1.0 
    set opt($count,1,yoffset) 1.0 
    set opt($count,1,labels) "" 
    set opt($count,1,lfont) default 
    set opt($count,1,lfill) \#000000 
    set opt($count,1,lwidth)  100
    set opt($count,1,lanchor) "center_left" 
    set opt($count,1,ljust) "left" 
    set opt($count,1,ltxt) ""
    set opt($count,1,override) 0

	set optlist { _check xcoord ycoord xoffset yoffset labels lfont lfill lwidth \
		lanchor ljust ltxt override }

    foreach key $optlist {
		set opt($count,0,$key) $opt($count,1,$key)
    } 
        
    incr count
    return $node
}

proc GmCLabels::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

proc GmCLabels::select_labels { id } {
    set m [GSelect paint/labels]
    if { $m != "" } { 
        set GmCLabels::opt($id,1,labels) $m
        GmTree::autonamel $m
    }
}

proc GmCLabels::select_font { id frm } {
	global mon
	variable opt
    
    set fon [SelectFont $frm.lfont -type dialog -sampletext 1 -title "Select label font"]
	if { $fon != "" } {set opt($id,1,lfont) $fon}
}

# display labels options
proc GmCLabels::options { id frm } {
    variable opt
    global iconpath

    # Panel heading1
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Create postscript labels for vector objects from v.labels file" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # Panel heading2
    set row [ frame $frm.heading2 ]
    Label $row.a -text "  (for postscript eps, pdf, and print output only)" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # labels name
    set row [ frame $frm.name ]
    Label $row.a -text [G_msg "Labels file:"]
    Button $row.b -image [image create photo -file "$iconpath/gui-maplabels.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "labels file to display"] \
		-command "GmCLabels::select_labels $id"
    Entry $row.c -width 40 -text "$opt($id,1,labels)" \
		-textvariable GmCLabels::opt($id,1,labels)
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # override
    set row [ frame $frm.disp ]
    Label $row.a -text [G_msg "Override offset, alignment, and color settings in v.labels file: "]
    checkbutton $row.b -variable GmCLabels::opt($id,1,override)
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # labels options1
    set row [ frame $frm.lblopt1 ]
    Label $row.a -text [G_msg "Align label with vector object: "] 
    ComboBox $row.b -padx 2 -width 12 -textvariable GmCLabels::opt($id,1,lanchor) \
                    -values {"lower left" "lower center" "lower right" "center left" "center" 
                    "center right" "upper left" "upper center" "upper right" }
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

	# label offset
    set row [ frame $frm.offset ]
    Label $row.a -text [G_msg "Offset label from vector object: "] 
    LabelEntry $row.b -label [G_msg "x offset"] \
    	-textvariable GmCLabels::opt($id,1,xoffset) \
		-width 10
    LabelEntry $row.c -label [G_msg "y offset"] \
    	-textvariable GmCLabels::opt($id,1,yoffset) \
		-width 10
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # labels options2
    set row [ frame $frm.lbltopt2 ]
    Label $row.a -text [G_msg "Justification: "] 
    ComboBox $row.b -padx 2 -width 7 -textvariable GmCLabels::opt($id,1,ljust) \
                    -values {"left" "center" "right"}
    Label $row.c -text " Label line length: "
    LabelEntry $row.d -textvariable GmCLabels::opt($id,1,lwidth) -width 5 
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
        
    # select font
    set row [ frame $frm.font ]
    Label $row.a -text [G_msg "Font:"] 
    Button $row.b -image [image create photo -file "$iconpath/gui-font.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "select font for label"] \
	    -command "GmCLabels::select_font $id $frm"
    Entry $row.c -width 15 -text "$opt($id,1,lfont)" \
	    -textvariable GmCLabels::opt($id,1,lfont)  
    Label $row.d -text [G_msg "  color"] 
    SelectColor $row.e -type menubutton -variable GmCLabels::opt($id,1,lfill)
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # launch v.label
    set row [ frame $frm.vlabel ]
    Label $row.a -text "Launch v.label to create labels file" 
    Button $row.b -text [G_msg "v.label"] \
	    -command "execute v.label"
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

}

proc GmCLabels::save { tree depth node } {
    variable opt
    variable optlist
    
    set id [GmTree::node_id $node]


    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}

proc GmCLabels::display { node } {
    global mon
    global env
    variable optlist
    variable opt
    variable tree
    variable dup
    variable count
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]
    
    set can($mon) $MapCanvas::can($mon)
    
    set labelpath "$env(GISDBASE)/$env(LOCATION_NAME)/$env(MAPSET)/paint/labels/$opt($id,1,labels)"    
    
    if { ! ( $opt($id,1,_check) ) } { return } 
    
    # open the v.label file for reading
	catch {set labelfile [open $labelpath r]}
	
	#loop through coordinates and options for each label
    while { [gets $labelfile in] > -1 } {
		set key ""
		set val ""
        set in [string trim $in " "] 
		if { $in == "" } { continue }
		if { ![regexp -- {([^ ]+) (.+)$} $in r key val] } {set key $in}
        
		# Label options	
		switch $key {
			"east:" {
				set east $val
				set opt($id,1,xcoord) [MapCanvas::mape2scrx $mon $east]
			}
			"north:" {
				set north $val
				set opt($id,1,ycoord) [MapCanvas::mapn2scry $mon $north]
			}
			"xoffset:" {
				if { $opt($id,1,override) == 0 } {
					set opt($id,1,xoffset) $val
				}
				if { $opt($id,1,xoffset) != "" } {
					set opt($id,1,xcoord) [expr $opt($id,1,xcoord) + $opt($id,1,xoffset)]
				}
			}
			"yoffset:" {
				if { $opt($id,1,override) == 0 } {
					set opt($id,1,yoffset) $val
				}
				if { $opt($id,1,yoffset) != "" } {
					set opt($id,1,ycoord) [expr $opt($id,1,ycoord) + $opt($id,1,yoffset)]
				}
			}
			"ref:" {
				if { $opt($id,1,override) == 0 } {
					set opt($id,1,lanchor) $val
				}
				switch $opt($id,1,lanchor) {
					"lower left" 	{ set opt($id,1,anchor) "ne"}
					"left lower" 	{ set opt($id,1,anchor) "ne"}
					"lower" 		{ set opt($id,1,anchor) "n" }
					"lower center" 	{ set opt($id,1,anchor) "n" }
					"center lower" 	{ set opt($id,1,anchor) "n" }
					"lower right" 	{ set opt($id,1,anchor) "nw"}
					"right lower" 	{ set opt($id,1,anchor) "nw"}
					"left" 			{ set opt($id,1,anchor) "e" }
					"center left" 	{ set opt($id,1,anchor) "e" }
					"left center" 	{ set opt($id,1,anchor) "e" }
					"center" 		{ set opt($id,1,anchor) "center" }
					"right" 		{ set opt($id,1,anchor) "w" }
					"center right" 	{ set opt($id,1,anchor) "w" }
					"right center" 	{ set opt($id,1,anchor) "w" }
					"upper left" 	{ set opt($id,1,anchor) "se"}
					"left upper" 	{ set opt($id,1,anchor) "se"}
					"center" 		{ set opt($id,1,anchor) "s" }
					"upper center" 	{ set opt($id,1,anchor) "s" }
					"center upper" 	{ set opt($id,1,anchor) "s" }
					"upper right" 	{ set opt($id,1,anchor) "sw"}
					"right upper" 	{ set opt($id,1,anchor) "sw"}
					default 		{ set opt($id,1,anchor) "w" }
				}
			}
			"font:" {
				set x ""
			}
			"color:" {
				if { $opt($id,1,override) == 0 } {
					set opt($id,1,lfill)  [color_grass_to_tcltk $val]
				}
				if { $opt($id,1,lfill) == "" } {
					set opt($id,1,lfill) "#000000"
				}
			}
			"fontsize:" {
				set x ""
			}
			"width:" {
				set x ""
			}
			"hcolor:" {
				set x ""
			}
			"hwidth:" {
				set x ""
			}
			"background:" {
				set x ""
			}
			"border:" {
				set x ""
			}
			"opaque:" {
				set x ""
			}
			"rotate:" {
				set x ""
			}
			"text:" {
				set opt($id,1,ltxt) $val
				# create each label when loop gets to a text line in the labels file
				$can($mon) create text $opt($id,1,xcoord) $opt($id,1,ycoord) \
					-anchor $opt($id,1,anchor) \
					-justify $opt($id,1,ljust) \
					-width $opt($id,1,lwidth) \
					-fill $opt($id,1,lfill) \
					-font $opt($id,1,lfont) \
					-text $opt($id,1,ltxt)
			}
			default {
				#for anything else, just move on
				set x ""
			}
		}
	}
	# close labels file
	if {[catch {close $labelfile} error]} {
		puts $error
	}

}


proc GmCLabels::query { node } {
    puts "Query not supported for Paint labels layer"
}

proc GmCLabels::duplicate { tree parent node id } {
    variable optlist
    variable opt
    variable count
	variable dup
	global iconpath

    set node "PS labels:$count"
	set dup($count) 1

    set frm [ frame .clabelsicon$count]
    set check [checkbutton $frm.check \
		-variable GmCLabels::opt($count,1,_check) \
		-height 1 -padx 0 -width 0]

    image create photo clabels_ico -file "$iconpath/gui-maplabels.gif"
    set ico [label $frm.ico -image clabels_ico -bd 1 -relief raised]

    bind $ico <ButtonPress-1> "GmTree::selectn $tree $node"

    pack $check $ico -side left
	
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

	if { $opt($id,1,labels) == ""} {
	    $tree insert $sellayer $parent $node \
		-text      "clabels $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert $sellayer $parent $node \
		-text      "$opt($id,1,clabels)" \
		-window    $frm \
		-drawcross auto
	}

    foreach key $optlist {
    	set opt($count,1,$key) $opt($id,1,$key)
		set opt($count,0,$key) $opt($count,1,$key)
    } 
	
	set id $count
	
    incr count
    return $node
}
