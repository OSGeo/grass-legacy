###############################################################
# maptool.tcl - toolbar file GRASS GIS Manager map display canvas
# January 2006 Michael Barton, Arizona State University
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
    variable toolbar
    
    set selcolor #88aa88
    set maptools "pointer"
    set toolbar $tb

    # DISPLAY AND MONITOR SELECTION
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 -background $bgcolor ]
    
    # display
    $bbox1 add -image [image create photo -file "$iconpath/gui-display.gif"] \
        -command "MapCanvas::drawmap $mon 0; MapCanvas::composite $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Display active layers in current region"]

    $bbox1 add -image [image create photo -file "$iconpath/module-nviz.gif"] \
        -command {GmGroup::nvdisplay "root"} \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Start NVIZ using active layers in current region"]

    # erase
    $bbox1 add -image [image create photo -file "$iconpath/gui-erase.gif"] \
        -command "MapCanvas::erase $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Erase to white"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical -background aquamarine2 ]
    pack $sep1 -side left -fill y -padx 5 -anchor w
    
    # DISPLAY TOOLS

    # pointer
    set pointer [radiobutton $tb.pointer \
    	-image [image create photo -file "$iconpath/gui-pointer.gif"] \
        -command "MapCanvas::stoptool $mon" \
		-variable maptools -value pointer \
        -indicatoron false -bg $bgcolor -selectcolor $selcolor]    

    # zoom in
    set zoomin [radiobutton $tb.zoomin \
    	-image [image create photo -file "$iconpath/gui-zoom_in.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::zoombind $mon 1" \
		-variable maptools -value zoomin \
        -indicatoron false -bg $bgcolor -selectcolor $selcolor]    
    
    #zoom out
    set zoomout [radiobutton $tb.zoomout \
		-image [image create photo -file "$iconpath/gui-zoom_out.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::zoombind $mon -1" \
		-variable maptools -value zoomout \
        -indicatoron false -bg $bgcolor -selectcolor $selcolor]    

    # pan
    set pan [radiobutton $tb.pan \
		-image [image create photo -file "$iconpath/gui-pan.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::panbind $mon" \
		-variable maptools -value pan \
        -indicatoron false -bg $bgcolor -selectcolor $selcolor]    

    # query
    set query [radiobutton $tb.query \
		-image [image create photo -file "$iconpath/gui-query.gif"] \
        -command "MapCanvas::stoptool $mon; MapCanvas::querybind $mon" \
		-variable maptools -value query \
        -indicatoron false -bg $bgcolor -selectcolor $selcolor]    

    # measure
    set measure [radiobutton $tb.measure \
		-image [image create photo -file "$iconpath/gui-measure.gif"]  \
    	-command "MapCanvas::stoptool $mon; MapCanvas::measurebind $mon"\
		-variable maptools -value measure \
        -indicatoron false -bg $bgcolor -selectcolor $selcolor]    

    pack $pointer $zoomin $zoomout $pan $query $measure -side left -anchor w


    set sep3 [Separator $toolbar.sep3 -orient vertical -background aquamarine2 ]
    pack $sep3 -side left -fill y -padx 5 -anchor w

    set bbox3 [ButtonBox $toolbar.bbox3 -background $bgcolor -spacing 0  ]
    
    # zoom.back
    $bbox3 add -image [image create photo -file "$iconpath/gui-zoom_back.gif"] \
        -command "MapCanvas::zoom_back $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2\
        -helptext [G_msg "Return to previous zoom"]

    # zoom to current region  
    $bbox3 add -image [image create photo -file "$iconpath/gui-zoom_current.gif"] \
        -command "MapCanvas::zoom_current $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Zoom to current region"]

    # zoom to saved region
    $bbox3 add -image [image create photo -file "$iconpath/gui-zoom_region.gif"] \
        -command "MapCanvas::zoom_region $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Zoom to saved region"]

    # zoom to default region  
    $bbox3 add -image [image create photo -file "$iconpath/gui-zoom_default.gif"] \
        -command "MapCanvas::zoom_default $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Zoom to default region"]

    pack $bbox3 -side left -anchor w

    set sep4 [Separator $toolbar.sep4 -orient vertical -background aquamarine2 ]
    pack $sep4 -side left -fill y -padx 5 -anchor w

    # FILE & PRINT
    set bbox4 [ButtonBox $toolbar.bbox4 -spacing 0 -background $bgcolor ]

    $bbox4 add -image [image create photo -file "$iconpath/file-print.gif"]  \
    	-command "MapCanvas::printcanvas $mon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Print raster & vector maps to eps file"]

	set mapsave [menubutton $tb.mapsave  \
		-image [image create photo -file "$iconpath/file-save.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -bg $bgcolor -width 28 -indicatoron 1 -direction below]


	pack $mapsave -side left -anchor w -expand no -fill y 
    pack $bbox4 -side left -anchor w

	# menu for saving display
	set savefile [menu $mapsave.sf -type normal -bg $bgcolor]
	set pngfile [menu $savefile.png -type normal -bg $bgcolor]
	set ppmfile [menu $savefile.ppm -type normal -bg $bgcolor]
	set jpgfile [menu $savefile.jpg -type normal -bg $bgcolor]

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

}

###############################################################################

# procedures for saving files

# save png file
proc MapToolBar::savefile { type quality } {
	global env
	global mon
	global outfile
	

	if { [info exists HOME] } {
		set dir $env(HOME)
		set path [tk_getSaveFile -initialdir $dir \
			-title "Save file: do not add extension to file name"]
	} else {
		set path [tk_getSaveFile  \
			 -title "Save file: do not add extension to file name"]
	}
	file copy -force $outfile($mon) $path.ppm
	puts "path is $path"
	
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

