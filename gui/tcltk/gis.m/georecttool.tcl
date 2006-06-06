###############################################################
# georecttool.tcl - toolbar file for georectify canvas, GRASS GIS Manager
# June 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
###############################################################


namespace eval GRToolBar {
    variable toolbar
}


###############################################################################

proc GRToolBar::create { tb } {
    global gmpath
    global bgcolor
    global maptools
    global iconpath
    variable toolbar
    
    set selcolor #88aa88
    set maptools "selector"
    set toolbar $tb

    # DISPLAY AND MONITOR SELECTION
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 ]
    
    # display
    $bbox1 add -image [image create photo -file "$iconpath/gui-display.gif"] \
        -command "GRMap::request_redraw 0" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "Display active layers"]

    # erase
    $bbox1 add -image [image create photo -file "$iconpath/gui-erase.gif"] \
        -command "GRMap::erase" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "Erase to white"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical -background $bgcolor ]
    pack $sep1 -side left -fill y -padx 5 -anchor w
    
    # DISPLAY TOOLS

    # selector
    set selector [radiobutton $tb.selector \
    	-image [image create photo -file "$iconpath/gui-pointer.gif"] \
        -command "GRMap::stoptool; GRMap::selector" \
		-variable maptools -value selector  -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor  ]
    DynamicHelp::register $selector balloon [G_msg "Set ground control points"]

    # zoom in
    set zoomin [radiobutton $tb.zoomin \
    	-image [image create photo -file "$iconpath/gui-zoom_in.gif"] \
        -command "GRMap::stoptool; GRMap::zoombind 1" \
		-variable maptools -value zoomin -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]   
    DynamicHelp::register $zoomin balloon [G_msg "Zoom In"]
    
    #zoom out
    set zoomout [radiobutton $tb.zoomout \
		-image [image create photo -file "$iconpath/gui-zoom_out.gif"] \
        -command "GRMap::stoptool; GRMap::zoombind -1" \
		-variable maptools -value zoomout  -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]    
    DynamicHelp::register $zoomout balloon [G_msg "Zoom Out"]

    # pan
    set pan [radiobutton $tb.pan \
		-image [image create photo -file "$iconpath/gui-pan.gif"] \
        -command "GRMap::stoptool; GRMap::panbind" \
		-variable maptools -value pan  -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]    
    DynamicHelp::register $pan balloon [G_msg "Pan"]

    pack $selector $zoomin $zoomout $pan -side left -anchor w

    set sep2 [Separator $toolbar.sep2 -orient vertical -background $bgcolor ]
    pack $sep2 -side left -fill y -padx 5 -anchor w

    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 0  ]

    # zoom.back
    $bbox2 add -image [image create photo -file "$iconpath/gui-zoom_back.gif"] \
        -command "GRMap::zoom_back" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1\
        -highlightbackground $bgcolor -activebackground $bgcolor \
        -helptext [G_msg "Return to previous zoom"]

    # zoom to map
    $bbox2 add -image [image create photo -file "$iconpath/gui-zoom_map.gif"] \
        -command {GRMap::zoom_map} \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1\
        -highlightbackground $bgcolor -activebackground $bgcolor \
        -helptext [G_msg "Zoom to map"]

    pack $bbox2 -side left -anchor w
}

###############################################################################
# changes button on keypress
proc GRToolBar::changebutton { rbname } {
	global maptools
	
	set maptools $rbname
}
