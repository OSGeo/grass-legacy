# Toolbar for map display canvas, GRASS GIS Manager
# January 2006
# Michael Barton (Arizona State University)
#


namespace eval MapToolBar {
    variable toolbar
}


###############################################################################

proc MapToolBar::create { tb } {
    global gmpath
    global bgcolor
    global mon
    global env
    variable toolbar
    
    set toolbar $tb

    # DISPLAY AND MONITOR SELECTION
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 -background $bgcolor ]
    
    # display
    $bbox1 add -image [image create photo -file "$gmpath/display.gif"] \
        -command "mapcan::mapsettings $mon; mapcan::drawmap $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Display active layers in current region"]

    # erase
    $bbox1 add -image [image create photo -file "$gmpath/erase.gif"] \
        -command "mapcan::erase $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Erase to white"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical -background aquamarine2 ]
    pack $sep1 -side left -fill y -padx 5 -anchor w
    
    # DISPLAY TOOLS
    set bbox3 [ButtonBox $toolbar.bbox3 -background $bgcolor -spacing 0  ]

    # zoom in
    $bbox3 add -image [image create photo -file "$gmpath/zoom.gif"] \
        -command "mapcan::zoombind $mon 1" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Zoom"]
    
    #zoom out
    $bbox3 add -image [image create photo -file "$gmpath/zoomout.gif"] \
        -command "mapcan::zoombind $mon -1" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Zoom out"]
    
    # zoom.back
    $bbox3 add -image [image create photo -file "$gmpath/zoom.back.gif"] \
        -command "mapcan::zoom_back $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1\
        -helptext [G_msg "Return to previous zoom"]

    # zoom to default region  
    $bbox3 add -image [image create photo -file "$gmpath/zoom_default.gif"] \
        -command "mapcan::zoom_default $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Zoom to default region"]

    # zoom to saved region
    $bbox3 add -image [image create photo -file "$gmpath/zoom_region.gif"] \
        -command "mapcan::zoom_region $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Zoom to saved region"]

    # pan
    $bbox3 add -image [image create photo -file "$gmpath/pan.gif"] \
        -command "mapcan::panbind $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1\
        -helptext [G_msg "Pan and recenter"]

    # query
    $bbox3 add -image [image create photo -file "$gmpath/query.gif"] \
        -command "mapcan::querybind $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Query map (select map first)"]

    # measure
    $bbox3 add -image [image create photo -file "$gmpath/measure.gif"]  \
    	-command "mapcan::measurebind $mon"\
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Measure lengths and areas"]

    pack $bbox3 -side left -anchor w

    set sep3 [Separator $toolbar.sep3 -orient vertical -background aquamarine2 ]
    pack $sep3 -side left -fill y -padx 5 -anchor w

    # FILE & PRINT
    set bbox4 [ButtonBox $toolbar.bbox4 -spacing 0 -background $bgcolor ]

    $bbox4 add -image [image create photo -file "$gmpath/print.gif"]  \
    	-command "mapcan::printcanvas $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Print raster & vector maps to eps file"]

	set mapsave [menubutton $tb.mapsave  \
		-image [image create photo -file "$gmpath/save.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -bg $bgcolor -width 28 -indicatoron 1 -direction below]


	pack $mapsave -side left -anchor w -expand no -fill y 
    pack $bbox4 -side left -anchor w

	# menu for saving display
	set savefile [menu $mapsave.sf -type normal]
	set pngfile [menu $savefile.png -type normal]
	set ppmfile [menu $savefile.ppm -type normal]
	set jpgfile [menu $savefile.jpg -type normal]

	$savefile add cascade -label "PNG/PPM/PNM" -menu $pngfile
		$pngfile add command -label "no compression" \
			-command { MapToolBar::savepng 0}
		$pngfile add command -label "fastest compression"  \
			-command { MapToolBar::savepng 1}
		$pngfile add command -label "most compression"  \
			-command { MapToolBar::savepng 9}
	$savefile add command -label "TIF*" -command {MapToolBar::savegdal tif 0}
	$savefile add cascade -label "JPG*" -menu $jpgfile
		$jpgfile add command -label "low quality (50)"  \
			-command {MapToolBar::savegdal jpg 50}
		$jpgfile add command -label "mid quality (75)" \
			-command {MapToolBar::savegdal jpg 75}
		$jpgfile add command -label "high quality (95)" \
			-command {MapToolBar::savegdal jpg 95}
	$savefile add command -label "BMP*" -command {MapToolBar::savegdal bmp 0}
	$savefile add command -label "(* requires gdal)" -state disabled

	$mapsave configure -menu $savefile

}

###############################################################################

# procedures for saving files

# save png file
proc MapToolBar::savepng { compression } {
	global env
	global mon
	
	if { [info exists HOME] } {
		set dir $env(HOME)
	} else {
		set dir ""
	}
	
	set types {
    {{PNG} {.png}}
    {{PPM/PNM} {.ppm}}
	}
	
	set path [tk_getSaveFile -filetypes $types -initialdir $dir]
	
	mapcan::mapsettings $mon
	set env(GRASS_PNG_COMPRESSION) $compression
	set env(GRASS_PNGFILE) "$path"
	mapcan::drawmap $mon
	set env(GRASS_PNG_COMPRESSION) 0
	return
}

# save files to ppm and use gdal_translate to save them to another format
proc MapToolBar::savegdal { type quality } {
	global env
	global mon
	
	if { [info exists HOME] } {
		set dir $env(HOME)
	} else {
		set dir ""
	}
	
	set path [tk_getSaveFile -defaultextension .ppm -initialdir $dir]
	
	mapcan::mapsettings $mon
	set env(GRASS_PNGFILE) "$path"
	mapcan::drawmap $mon
	
	if { $path != "" } {
		if { $type == "tif" } { 
		    eval exec {gdal_translate $path $path.tif -of GTIFF}
			eval exec "rm $path"
		}
		if { $type == "bmp" } { 
			eval exec {gdal_translate $path $path.bmp -of BMP}
			eval exec "rm $path"
		}
		if { $type == "jpg" } { 
    		eval exec {gdal_translate $path $path.jpg -of JPEG -co \
    			QUALITY=$quality}
			eval exec "rm $path"
		}
	}
	return
}

