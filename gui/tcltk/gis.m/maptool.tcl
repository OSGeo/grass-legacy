###############################################################
# maptool.tcl - toolbar file GRASS GIS Manager map display canvas
# January 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
###############################################################


namespace eval MapToolBar {
    variable toolbar
}


###############################################################################

proc MapToolBar::create { tb } {
    global gmpath
    global bgcolor
    global mon
    global env
    global maptools
    global selclr
    global mapfile
    global iconpath
    global bgcolor
    variable toolbar
    
    set selcolor #88aa88
    set maptools "pointer"
    set toolbar $tb

    # DISPLAY AND MONITOR SELECTION
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 ]
    
    # display
    $bbox1 add -image [image create photo -file "$iconpath/gui-display.gif"] \
        -command "MapCanvas::request_redraw $mon 0" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "Display active layers"]

    # zoom to current region  
    $bbox1 add -image [image create photo -file "$iconpath/gui-redraw.gif"] \
        -command "MapCanvas::request_redraw $mon 1" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor -activebackground $bgcolor \
        -helptext [G_msg "Redraw all layers"]


    $bbox1 add -image [image create photo -file "$iconpath/module-nviz.gif"] \
        -command {GmGroup::nvdisplay "root"} \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "Start NVIZ using active layers in current region"]

    # erase
    $bbox1 add -image [image create photo -file "$iconpath/gui-erase.gif"] \
        -command "MapCanvas::erase $mon" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "Erase to white"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical -background $bgcolor ]
    pack $sep1 -side left -fill y -padx 5 -anchor w
    
    # DISPLAY TOOLS

    # pointer
    set pointer [radiobutton $tb.pointer \
    	-image [image create photo -file "$iconpath/gui-pointer.gif"] \
        -command "MapCanvas::stoptool $mon" \
		-variable maptools -value pointer  -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor  ]
    DynamicHelp::register $pointer balloon [G_msg "Pointer"]

    # zoom in
    set zoomin [radiobutton $tb.zoomin \
    	-image [image create photo -file "$iconpath/gui-zoom_in.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::zoombind $mon 1" \
		-variable maptools -value zoomin -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]   
    DynamicHelp::register $zoomin balloon [G_msg "Zoom In"]
    
    #zoom out
    set zoomout [radiobutton $tb.zoomout \
		-image [image create photo -file "$iconpath/gui-zoom_out.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::zoombind $mon -1" \
		-variable maptools -value zoomout  -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]    
    DynamicHelp::register $zoomout balloon [G_msg "Zoom Out"]

    # pan
    set pan [radiobutton $tb.pan \
		-image [image create photo -file "$iconpath/gui-pan.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::panbind $mon" \
		-variable maptools -value pan  -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]    
    DynamicHelp::register $pan balloon [G_msg "Pan"]

    pack $pointer $zoomin $zoomout $pan -side left -anchor w

    set sep2 [Separator $toolbar.sep2 -orient vertical -background $bgcolor ]
    pack $sep2 -side left -fill y -padx 5 -anchor w

    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 0  ]

    # zoom.back
    $bbox2 add -image [image create photo -file "$iconpath/gui-zoom_back.gif"] \
        -command "MapCanvas::zoom_back $mon" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1\
        -highlightbackground $bgcolor -activebackground $bgcolor \
        -helptext [G_msg "Return to previous zoom"]

	set mapzoom [menubutton $tb.mapzoom  \
		-image [image create photo -file "$iconpath/gui-mapzoom.gif"] \
        -highlightthickness 0 -takefocus 0 -relief flat -borderwidth 1  \
        -highlightbackground $bgcolor -activebackground honeydew \
        -bg $bgcolor -width 32 -indicatoron 0 -direction below]
    DynamicHelp::register $mapzoom balloon [G_msg "Zoom to..."]

	# menu zooming display
	set zoommenu [menu $mapzoom.zm -type normal]
	
	set zmimg [image create photo -file "$iconpath/gui-zoom_map.gif"]
	set zrimg [image create photo -file "$iconpath/gui-zoom_region.gif"]
	set zcimg [image create photo -file "$iconpath/gui-zoom_current.gif"]
	set zdimg [image create photo -file "$iconpath/gui-zoom_default.gif"]

	$zoommenu add command \
		-compound top \
		-label {Zoom to selected map} \
		-command {MapCanvas::zoom_map $mon}
	$zoommenu add command \
		-compound right \
		-label {Zoom to saved region} \
		-command {MapCanvas::zoom_region $mon}
	$zoommenu add command \
		-compound center \
		-label {Zoom to current region (set with g.region)} \
		-command {MapCanvas::zoom_current $mon}
	$zoommenu add command \
		-compound center \
		-label {Zoom to default region} \
		-command {MapCanvas::zoom_default $mon}
	$zoommenu add command \
		-compound center \
		-label {Set current region to match display} \
		-command {MapCanvas::gregion_zoom $mon}

	$mapzoom configure -menu $zoommenu

    pack $bbox2 -side left -anchor w

	pack $mapzoom -side left -anchor w -expand no -fill y 

    set sep3 [Separator $toolbar.sep3 -orient vertical -background $bgcolor ]
    pack $sep3 -side left -fill y -padx 5 -anchor w


    # query
    set query [radiobutton $tb.query \
		-image [image create photo -file "$iconpath/gui-query.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::querybind $mon" \
		-variable maptools -value query  -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]    
    DynamicHelp::register $query balloon [G_msg "Query"]

    # measure
    set measure [radiobutton $tb.measure \
		-image [image create photo -file "$iconpath/gui-measure.gif"]  \
    	-command "MapCanvas::stoptool $mon; MapCanvas::measurebind $mon"\
		-variable maptools -value measure -relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]    
    DynamicHelp::register $measure balloon [G_msg "Measure"]

    set bbox3 [ButtonBox $toolbar.bbox3 -spacing 0 ]
    $bbox3 add -image [image create photo -file "$iconpath/gui-profile.gif"] \
    	-command "MapCanvas::stoptool $mon; MapCanvas::startprofile $mon" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor -activebackground $bgcolor \
        -helptext [G_msg "Create profile of raster map"]


    pack $query $measure -side left -anchor w
    pack $bbox3 -side left -anchor w


    set sep4 [Separator $toolbar.sep4 -orient vertical -background $bgcolor ]
    pack $sep4 -side left -fill y -padx 5 -anchor w

    # FILE & PRINT
    set bbox4 [ButtonBox $toolbar.bbox4 -spacing 0 ]
    
    $bbox4 add -image [image create photo -file "$iconpath/file-print.gif"]  \
    	-command "MapCanvas::printcanvas $mon" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor -activebackground $bgcolor \
        -helptext [G_msg "Print raster & vector maps to eps file"]

	set mapsave [menubutton $tb.mapsave  \
		-image [image create photo -file "$iconpath/gui-filesave.gif"] \
        -highlightthickness 0 -takefocus 0 -relief flat -borderwidth 1  \
        -highlightbackground $bgcolor -activebackground honeydew \
        -bg $bgcolor -width 32 -indicatoron 0 -direction below]
    DynamicHelp::register $mapsave balloon [G_msg "Export display to graphics file"]

	pack $mapsave -side left -anchor w -expand no -fill y 
    pack $bbox4 -side left -anchor w

	# menu for saving display
	set savefile [menu $mapsave.sf -type normal]
	set pngfile [menu $savefile.png -type normal]
	set ppmfile [menu $savefile.ppm -type normal]
	set jpgfile [menu $savefile.jpg -type normal]

	$savefile add command -label "PPM/PNM" -command {MapToolBar::savefile ppm 0}
	$savefile add command -label "TIF*" -command {MapToolBar::savefile tif 0}
	$savefile add cascade -label "JPG*" -menu $jpgfile
		$jpgfile add command -label "low quality (50)"  \
			-command {MapToolBar::savefile jpg 50}
		$jpgfile add command -label "mid quality (75)" \
			-command {MapToolBar::savefile jpg 75}
		$jpgfile add command -label "high quality (95)" \
			-command {MapToolBar::savefile jpg 95}
	$savefile add command -label "BMP*" -command {MapToolBar::savefile bmp 0}
	$savefile add command -label "(* requires gdal)" -state disabled

	$mapsave configure -menu $savefile

    set sep5 [Separator $toolbar.sep5 -orient vertical ]
    pack $sep5 -side left -fill y -padx 5 -anchor w

    # Render modes

    # Strict render mode
    # Uses previous resolution and exact boundaries
    set strictdraw [radiobutton $tb.strictdraw \
		-command "MapCanvas::exploremode $mon 0" \
		-variable MapToolBar::explore($mon) -value strict \
		-relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]
    DynamicHelp::register $strictdraw balloon [G_msg "Strict Draw Mode"]
    icon_configure $strictdraw drawmode strict

    # Explore render mode
    # Uses resolution to match display and expanded boundaries to fill display
    set exploredraw [radiobutton $tb.strictzoom \
		-command "MapCanvas::exploremode $mon 1" \
		-variable MapToolBar::explore($mon) -value explore \
		-relief flat -offrelief flat -overrelief raised \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selcolor \
		-activebackground $bgcolor -highlightbackground $bgcolor ]
    DynamicHelp::register $exploredraw balloon [G_msg "Explore Draw Mode"]
    icon_configure $exploredraw drawmode explore

    # This does not actually set the mode
    # it just starts visually in sync with the default
    set MapToolBar::explore($mon) strict

    pack $strictdraw $exploredraw -side left -anchor w
}

###############################################################################
# changes button on keypress
proc MapToolBar::changebutton { rbname } {
	global maptools
	
	set maptools $rbname
}

###############################################################################
# procedures for saving files

# save png file
proc MapToolBar::savefile { type quality } {
	global env
	global mon
	global outfile
	global tmpdir
	

	if { [info exists HOME] } {
		set dir $env(HOME)
		set path [tk_getSaveFile -initialdir $dir \
			-title "Save file: do not add extension to file name"]
	} else {
		set path [tk_getSaveFile  \
			 -title "Save file: do not add extension to file name"]
	}
	set currdir [pwd]
	cd $tmpdir

	file copy -force $outfile($mon) $path.ppm

	cd $currdir
	
	if { $path != "" } {				
		switch $type {
			"ppm" {
				return
			}
			"tif" { 
				eval exec {gdal_translate $path.ppm $path.tif -of GTIFF}
				eval exec "rm $path.ppm"
			}
			"bmp" { 
				eval exec {gdal_translate $path.ppm $path.bmp -of BMP}
				eval exec "rm $path.ppm"
			}
			"jpg" { 
				eval exec {gdal_translate $path.ppm $path.jpg -of JPEG -co \
					QUALITY=$quality}
				eval exec "rm $path.ppm"
			}
		}
	}
	return
}

