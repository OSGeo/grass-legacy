##########################################################################
# dtext.tcl new version - standard text layer options file for GRASS GIS Manager
# November 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmDtext {
    variable array opt # d.text.new options
    variable count 1
    variable array lfile 
    variable array lfilemask
    variable optlist
    variable first
    variable array dup 
    variable placement 
    variable currfont
    variable optlist

	#store current GRASS_FONT value to reset at end
	if {[info exists env(GRASS_FONT)]} {
		set currfont env(GRASS_FONT)
	} else {
		set currfont ""
	}
}

proc GmDtext::create { tree parent } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist
    variable first
	variable dup
	variable optlist
    global iconpath
    
    set node "dtext:$count"

    set frm [ frame .texticon$count]
    set check [checkbutton $frm.check \
    	-variable GmDtext::opt($count,1,_check) \
		-height 1 -padx 0 -width 0]

    image create photo textico -file "$iconpath/module-d.text.gif"
    set ico [label $frm.ico -image textico -bd 1 -relief raised]
    
    bind $ico <ButtonPress-1> "GmTree::selectn $tree $node"

    pack $check $ico -side left
    
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "text layer $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,1,_check) 1 
    set dup($count) 0

    set opt($count,_check) 1 
	set opt($count,1,opacity) 1.0
    set opt($count,1,text) "" 
    set opt($count,1,at) "10,10" 
    set opt($count,1,coordinates) "percent" 
    set opt($count,1,mouseset) 0
    set opt($count,1,align) "lower_left" 
    set opt($count,1,line)  10
    set opt($count,1,rotate) 0
    set opt($count,1,font) "romans" 
    set opt($count,1,fonttype) "grassfont" 
    set opt($count,1,bold) 0 
	set opt($count,1,size) 10
    set opt($count,1,color) \#000000 
    set opt($count,1,linespace) 1.25
    set first 1
        
    set optlist { _check text at coordinates mouseset\
    	align line rotate font bold size color linespace}
    
    foreach key $optlist {
		set opt($count,0,$key) $opt($count,1,$key)
    } 

	# create files in tmp diretory for layer output
	set mappid [pid]
	set lfile($count) [exec g.tempfile pid=$mappid]
	set lfilemask($count) $lfile($count)
	append lfile($count) ".ppm"
	append lfilemask($count) ".pgm"
		
    incr count
    return $node
}

###############################################################################
proc GmDtext::select_font { id frm } {
	global mon env xfontdir
	variable opt
    
    # tcltk font selection widget - fonts done work for GRASS?
    #set fon [SelectFont $frm.fontset -type dialog -sampletext 1 -title "Select font"]
	#if { $fon != "" } {set opt($id,1,font) $fon}
	
	# initialize fontdir
	set fontdir ""
	
	if { $opt($id,1,fonttype) == "grassfont" } {
		set fontdir "$env(GISBASE)/fonts"
	} elseif { $opt($id,1,fonttype) == "x11font" } {
		set fontdir $xfontdir
	} else {
		if { [info exists env(HOME)] } {
			set fontdir $env(HOME)
		} else {
			set fontdir ""
		}
	}

	if { $fontdir != "" } {
		set fontpath [tk_getOpenFile -initialdir $fontdir \
			-title [G_msg "Select font file"]]
	} else {
		set fontpath [tk_getOpenFile  \
			 -title [G_msg "Select font file"]]
	}
	
	set GmDtext::opt($id,1,font) $fontpath

	# reinitialize fontdir
	set fontdir ""

}

###############################################################################
proc GmDtext::set_option { node key value } {
    variable opt

    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

###############################################################################

proc GmDtext::mouseset { id } {
	# use mouse to set text placement coordinates
	global mon pctentry pixelentry geogentry
	variable placement
	if { $GmDtext::opt($id,1,mouseset) == 1} {
		if {$GmDtext::opt($id,1,coordinates) == "pixels"} {
			set pixelentry $GmDtext::placement
		} else {
			set pixelentry ""
		}
		if {$GmDtext::opt($id,1,coordinates) == "percent"} {
			set pctentry $GmDtext::placement
		} else {
			set pctentry ""
		}
		if {$GmDtext::opt($id,1,coordinates) == "geographic"} {
			set geogentry $GmDtext::placement
		} else {
			set geogentry ""
		}
	}
}

###############################################################################
# dtext options
proc GmDtext::options { id frm } {
    variable opt
	variable placement
	variable first
    global iconpath bgcolor

    # Panel heading
    set row [ frame $frm.heading ]
    Label $row.a -text [G_msg "Display text"] \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmDtext::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # text
    set row [ frame $frm.text ]
    LabelEntry $row.a -label [G_msg "Text to display: "] -textvariable GmDtext::opt($id,1,text) -width 51
    Button $row.b -text [G_msg "Help"] \
            -image [image create photo -file "$iconpath/gui-help.gif"] \
            -command "spawn g.manual --q d.text.new" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # coordinates1
    set row [ frame $frm.east_north ]
    set placement [LabelEntry $row.a -textvariable GmDtext::opt($id,1,at) -width 25 \
    	-label [G_msg "Text placement: x,y coordinates (from upper left) "]]
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
        
    # coordinates2
    set row [ frame $frm.textcoord2 ]
    Label $row.a -text [G_msg "     coordinate type for text placement "] 
    ComboBox $row.b -padx 2 -width 10 -textvariable GmDtext::opt($id,1,coordinates) \
    	-values {"pixels" "percent" "geographic" } -modifycmd "GmDtext::mouseset $id"
    checkbutton $row.c -text [G_msg "place with mouse"] \
    	-variable GmDtext::opt($id,1,mouseset) \
    	-command "GmDtext::mouseset $id"
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
    
    # alignment
    set row [ frame $frm.textalign ]
    Label $row.a -text [G_msg "     align text with coordinate point  "] 
    ComboBox $row.b -padx 2 -width 12 -textvariable GmDtext::opt($id,1,align) \
		-values {"lower_left" "bottom_center" "lower_right" "center_left" "center" 
		"center_right" "upper_left" "top_center" "upper_right" } 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

	# rotation
    set row [ frame $frm.textrotate ]
    Label $row.a -text [G_msg "     text rotation (degrees)"] 
    set rotation [SpinBox $row.b -range {-360 360 1} -textvariable GmDtext::opt($id,1,rotate) \
		-entrybg white -width 6]
	if {$first==1} {$rotation setvalue @360}
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes	

    # font options1
    set row [ frame $frm.fontopt1 ]
    Label $row.a -text [G_msg "Text options: font type"] 
    set gfont [radiobutton $row.b -variable GmDtext::opt($id,1,fonttype) -value "grassfont" \
        -text [G_msg "GRASS"] -highlightthickness 0]
        DynamicHelp::register $gfont balloon [G_msg "GRASS stroke fonts"]
        $gfont select
    set xfont [radiobutton $row.c -variable GmDtext::opt($id,1,fonttype) -value "x11font" \
        -text [G_msg "X11 TrueType"] -highlightthickness 0]
        DynamicHelp::register $xfont balloon [G_msg "TrueType fonts from x11 directory"]
    set ofont [radiobutton $row.d -variable GmDtext::opt($id,1,fonttype) -value "other" \
        -text [G_msg "Other"] -highlightthickness 0]
        DynamicHelp::register $ofont balloon [G_msg "Custom font path"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

	# font options2
	set row [ frame $frm.fontopt2 ]
    Label $row.a -text [G_msg "     font "] 
    Button $row.b -image [image create photo -file "$iconpath/gui-font.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "select font for text (default is GRASS romans)"] \
	    -command "GmDtext::select_font $id $frm"
    Entry $row.c -width 30 -text "$opt($id,1,font)" \
	    -textvariable GmDtext::opt($id,1,font) 
    Label $row.d -text [G_msg "  color"] 
    SelectColor $row.e -type menubutton -variable GmDtext::opt($id,1,color)
    checkbutton $row.f -padx 10 -text [G_msg "bold text"] -variable \
        GmDtext::opt($id,1,bold) 
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes
	

    # font options3
    set row [ frame $frm.fontopt3 ]
    LabelEntry $row.a -label [G_msg "     text height in pixels "]\
    	-textvariable GmDtext::opt($id,1,size) -width 6
    LabelEntry $row.b -label [G_msg "  line spacing"]\
    	-textvariable GmDtext::opt($id,1,linespace) -width 6
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

}

###############################################################################
proc GmDtext::save { tree depth node } {
    variable opt
    variable optlist
    global mon
    
    puts "optlist is $optlist"

    set id [GmTree::node_id $node]

    foreach key $optlist {
		GmTree::rc_write $depth "$key $opt($id,1,$key)"
    }
}

###############################################################################
proc GmDtext::display { node mod } {
    global mon env
    variable optlist
    variable lfile 
    variable lfilemask
    variable opt
    variable tree
    variable dup
    variable count
    variable first
    variable currfont
    
 	set line ""
    set input ""
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    # If we are told dirty (for zoom) force dirty
    # Don't remove a dirty from a previous unrendered zoom
    if {$mod} {set opt($id,1,mod) 1}

    # set hex colors to rgb         
    set color [Gm::color $opt($id,1,color)]
    

    if { ! ( $opt($id,1,_check) ) } { return } 

    if { $opt($id,1,text) == "" } { return } 

    switch $opt($id,1,align) {
    	"lower_left" 	{ set align "ll"}
    	"bottom_center" { set align "lc" }
    	"lower_right" 	{ set align "lr"}
    	"center_left" 	{ set align "cl" }
    	"center" 		{ set align "cc" }
    	"center_right" 	{ set align "cr" }
    	"upper_left" 	{ set align "ul"}
    	"top_center" 	{ set align "uc" }
    	"upper_right" 	{ set align "ur"}
    }
    
    if {$opt($id,1,coordinates) == "percent"} {
		set atlist [split $opt($id,1,at) ","]
		set xcoord [lindex $atlist 0]
		set ycoord [expr 100 - [lindex $atlist 1]]
		set at "$xcoord,$ycoord"
    } else {
    	set at $opt($id,1,at)
    }
  
    set cmd "d.text.new -s size=$opt($id,1,size) color=$color \
    	at=$at align=$align  rotation=$opt($id,1,rotate) \
    	linespacing=$opt($id,1,linespace) --q {text=$opt($id,1,text)}"
  
    # bold text
    if { $opt($id,1,bold) != 0 } { 
        append cmd " -b"
    }
    
    # coordinates in pixels or geographic coordinates
    if {$opt($id,1,coordinates) == "pixels"} {
    	append cmd " -p"    
    } elseif {$opt($id,1,coordinates) == "geographic"} {
    	append cmd " -g"
    }
    
    
    # set grass font environmental variable to user selection"
	if { $opt($id,1,font) != ""} { set env(GRASS_FONT) $opt($id,1,font) }

	# Decide whether to run, run command, and copy files to temp
	GmCommonLayer::display_command [namespace current] $id $cmd
	
	# set grass font environmental variable to whatever it was when we started
	# this lets different text layers have different fonts
	
	if {$currfont == ""} {
		unset env(GRASS_FONT)
	} else {
		set env(GRASS_FONT) $currfont
	}

}

###############################################################################
# text duplicate layer

proc GmDtext::duplicate { tree parent node id } {
    variable opt
    variable count
    variable lfile
    variable lfilemask
    variable optlist
    variable first
	variable dup
    global iconpath

    set node "dtext:$count"
    set dup($count) 1

    set frm [ frame .texticon$count]
    set check [checkbutton $frm.check \
    	-variable GmDtext::opt($count,1,_check) \
		-height 1 -padx 0 -width 0]

    image create photo textico -file "$iconpath/module-d.text.gif"
    set ico [label $frm.ico -image textico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
		-text  "text layer $count"\
		-window    $frm \
		-drawcross auto  
        
	set opt($count,1,opacity) $opt($id,1,opacity)
    set first 1
        
    foreach key $optlist {
    	set opt($count,1,$key) $opt($id,1,$key)
		set opt($count,0,$key) $opt($count,1,$key)
    } 
	
	set id $count
	
	# create files in tmp directory for layer output
	set mappid [pid]
	set lfile($count) [exec g.tempfile pid=$mappid]
	set lfilemask($count) $lfile($count)
	append lfile($count) ".ppm"
	append lfilemask($count) ".pgm"

    incr count
    return $node
}
