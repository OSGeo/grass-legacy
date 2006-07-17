##########################################################################
#
# georect.tcl -TclTk canvas georectify display and controls 
#    for GIS Manager: GUI for GRASS 6 
#
# Author: Michael Barton (Arizona State University). Based on mapcanvas.tcl
#			by Michael Barton & Cedric Shock
#
# JUne 2006
#
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################


# All of these must be sourced before using georect.tcl:
# source $gmpath/gmtree.tcl
# source $env(GISBASE)/etc/gtcltk/gmsg.tcl
# source $env(GISBASE)/etc/gtcltk/select.tcl
# source $env(GISBASE)/etc/gui.tcl
# This one is going to be handled by pkgIndex:
# source $gmpath/georecttool.tcl

namespace eval GRMap {
	variable displayrequest # true if it wants to get displayed.

	# Something's modified the canvas or view
	
	# Degree of modification 0 - none, 1 - zoom, 2 - canvas
	variable array grcanmodified 
	variable grcan # The canvas widget of the georectify monitor
	variable grmapframe # Frame widget
	variable grcanvas_w # Width and height of canvas
	variable grcanvas_h
	variable driver_w # Actual width and height used while drawing / compositing
	variable driver_h # Actual width and height used while drawing / compositing
	variable map_ind # Indicator widgets
    variable initwd
    variable initht
	variable grcursor
	variable win
	variable tmpdir # TMP directory for raster display images used in canvas
	
	# variables for coordinate conversions and zooming
	variable linex1 
	variable liney1 
	variable linex2 
	variable liney2
	variable map_n
	variable map_s
	variable map_e
	variable map_w
	variable map_ew
	variable map_ns	
	variable scr_n
	variable scr_s
	variable scr_e
	variable scr_w
	variable scr_ew
	variable scr_ns
	variable map2scrx_conv
	variable map2scry_conv
	
	#variable grcoords # geographic coordinates from mouse click
	variable grcoords_mov # geographic coordinates from mouse movement to display in indicator widget
	variable grfile # Driver output file (.ppm)
	variable mappid	#process id to use for temp files

	# Current region and region historys
	# Indexed by history (1 (current) - zoomhistories), part (n, s, e, w, nsres, ewres).
	variable array monitor_zooms
	# Depth of zoom history to keep
	variable zoomhistories
	set zoomhistories 7

	# Regular order for region values in a list representing a region or zoom
	variable zoom_attrs
	set zoom_attrs {n s e w nsres ewres}

	# This variable keeps track of which monitor set the gism driver settings last.
	# They must always be redone if the monitor was different
	variable previous_monitor
	set previous_monitor {none}
	
	variable redrawrequest 0
	
	#variables for panning
	variable start_x 
	variable start_y
    variable from_x 
    variable from_y
    variable to_x
    variable to_y

	
	# gcp variables
	# use GCP in RMS calculations and rectification indexed by gcpnum
	variable array usegcp
	# entry widget for GCP xy coordinates indexed by gcpnum
	variable array xy
	# entry widget for GCP georectified coordinates indexed by gcpnum
	variable array geoc
	# checkbutton widget for GCP use indexed by gcpnum
	variable array chk
	# entry widget for RMS error value for each GCP indexed by gcpnum
	variable array error_rms
	# counter for GCP array variables and for loops indexed by gcpnum
	variable array errval
	
	variable gcpnum 
	variable totalrms #total rms error for GCP's, displayed in gcp manager indicator window

	#variables to keep track of location and mapset
	# current gisdbase
	variable currgdb
	# current location
	variable currloc
	# current mapset
	variable currmset
	# gisdbase of xy raster
	variable xygdb
	# location of xy raster
	variable xyloc
	# mapset of xy raster
	variable xymset
	# raster group to georectify
	variable xygroup
	# raster map to display
	variable xyrast
	# vector map to display and georectify
	variable xyvect
	# georectify raster or vector map
	variable maptype
	# is target mapset same as current mapset
	variable selftarget	
	
	# Temporary local region setting
	variable mapregion	
	
	    #initialize variables
	set initwd 640.0
	set initht 480.0
	set east 0.0
	set north 0.0
	set currgdb $env(GISDBASE)
	set currloc $env(LOCATION_NAME)
	set currmset $env(MAPSET)
	set xygdb ""
	set xyloc ""
	set xymset ""
	set xygroup ""
	set xyrast ""
	set xyvect ""
	set maptype ""
	set selftarget 0


}



###############################################################################
# Set location and mapset to selected xy 
proc GRMap::setxyenv { mset loc } {
	variable selftarget
	global env
	
	if { $selftarget == 1 } { return }
	
	if { $mset != "" && $loc != "" } {
		run "g.gisenv set=LOCATION_NAME=$loc"
		run "g.gisenv set=MAPSET=$mset"
		
		set env(LOCATION_NAME) $loc
		set env(MAPSET) $mset
	}
}


###############################################################################
# set location and mapset back to georectified
proc GRMap::resetenv { } {
	variable currloc
	variable currmset
	variable selftarget
	global env
	
	if { $selftarget == 1 } { return }

	run "g.gisenv set=LOCATION_NAME=$currloc"
	run "g.gisenv set=MAPSET=$currmset"
	
	set env(LOCATION_NAME) $currloc
	set env(MAPSET) $currmset

}


###############################################################################
# get xy group to georectify; set target to current location and mapset
proc GRMap::getxygroup { } {
	variable xyloc
	variable xymset
	variable xygroup
	variable currloc
	variable currmset
	variable selftarget

	# First, switch to xy mapset
	GRMap::setxyenv $xymset $xyloc
    
    set m [GSelect group]
    if { $m != "" } { 
        set GRMap::xygroup $m
        if { $selftarget == 1 } {
        	set cmd "i.target -c"
        } else {
			set cmd "i.target group=$GRMap::xygroup location=$currloc mapset=$currmset"
		}
		runcmd $cmd
    }
    
    
    # Return to georectified mapset
	GRMap::resetenv

}

###############################################################################
# get raster to display for georectification
proc GRMap::getxymap { type } {
	variable xyloc
	variable xymset
	variable xyrast
	variable xyvect
	variable maptype


	# First, switch to xy mapset
	GRMap::setxyenv $xymset $xyloc
    
    if { $type == "rast" } {
	    set m [GSelect cell]
    	if { $m != "" } { 
        	set GRMap::xyrast $m
        	set GRMap::xyvect ""
        	set maptype "rast"
    	}
    }
    
    if { $type == "vect" } {
	    set m [GSelect vector]
    	if { $m != "" } { 
        	set GRMap::xyvect $m
        	set GRMap::xyrast ""
        	set maptype "vect"
    	}
    }
    
    # Return to georectified mapset
	GRMap::resetenv

}

###############################################################################
# create or edit raster group to georectify
proc GRMap::group { } {
	variable xyloc
	variable xymset

	# First, switch to xy mapset
	GRMap::setxyenv $xymset $xyloc
	
    run_panel "i.group"
    
    # Return to georectified mapset
	GRMap::resetenv

}

###############################################################################
# get mapset of raster to georectify; automatically set location and gisdbase
proc GRMap::getmset { } {
	variable xygdb
	variable xyloc
	variable xymset
	variable currgdb
	variable currloc
	variable currmset
	variable mappid
	variable grfile
	variable tmpdir
	variable selftarget
	
	set path [tk_chooseDirectory -initialdir $currgdb \
		-title "Select mapset of raster to georectify" \
		-mustexist true ]
	# try to make sure that a valid mapset has been picked		
	if { $path == "" || $path == $currgdb || [file dirname $path] == $currgdb } { return }

	set xymset [file tail $path]
	set xylocdir [file dirname $path]
	set xyloc [file tail $xylocdir]
	set xygdb [file dirname $xylocdir]
	
	# check to see if the target location and mapset is the current one
	if { $xyloc == $currloc && $xymset == $currmset } {set selftarget 1 }
	
	set GRMap::xymset [file tail $path]
	
	# create files in tmp diretory for layer output
	# First, switch to xy mapset
	GRMap::setxyenv $xymset $xyloc

	set mappid [pid]
	set grfile [exec g.tempfile pid=$mappid]
	append grfile ".ppm"
	set tmpdir [file dirname $grfile]
	
	# Return to georectified mapset
	GRMap::resetenv
}


###############################################################################
# select mapset (and location) and raster map to profile, and start georectifying canvas

proc GRMap::startup { } {
	variable currgdb
	variable currloc
	variable currmset
	variable xygdb
	variable xyloc
	variable xymset
	variable xygroup
	variable xyrast
	variable xyvect
    variable initwd
    variable initht
	global env
        
    set grstarttitle "Set up enviroment for georectification"
	toplevel .grstart
    wm title .grstart [G_msg $grstarttitle]
    wm withdraw .grstart


	# create frame for georectify startup
	set grstartup [frame .grstart.fr ]
	set rastlabel [labelframe $grstartup.rl -text "2. Set up georectification for raster map(s)" \
		-padx 4 -pady 10 -fg mediumblue]
	set vectlabel [labelframe $grstartup.vl -text "2. Set up georectification for vector map"\
		-padx 4 -pady 10 -fg mediumblue]
		
    # set xy mapset
    set row [ frame $grstartup.mset ]
    Button $row.a -text [G_msg "1.  Select mapset"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Mapset of xy raster group"]\
        -width 16 -anchor w -highlightthickness 4  -fg mediumblue \
		-command {GRMap::getmset}
    Entry $row.b -width 35 -text "$GRMap::xymset" \
          -textvariable GRMap::xymset 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # Run i.group
    set row [ frame $rastlabel.igroup ]
    Button $row.a -text [G_msg "a.  Create/edit group"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Create/edit raster group"] \
        -width 16 -anchor w \
		-command {GRMap::group}
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # set xy group
    set row [ frame $rastlabel.group ]
    Button $row.a -text [G_msg "b.  Select group"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Select existing xy raster group to georectify"]\
        -width 16 -anchor w \
		-command {GRMap::getxygroup}
    Entry $row.b -width 35 -text "$GRMap::xygroup" \
          -textvariable GRMap::xygroup 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
			
    # set xy raster
    set row [ frame $rastlabel.rast ]
    Button $row.a -text [G_msg "c. Select raster"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Select xy raster to display for georectification"]\
        -width 16 -anchor w \
		-command {GRMap::getxymap "rast"}
    Entry $row.b -width 35 -text "$GRMap::xyrast" \
          -textvariable GRMap::xyrast 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

	pack $rastlabel -side top -fill both -expand yes 
	
    #or
    set row [ frame $grstartup.or ]
    Label $row.a -text [G_msg "OR..."] \
		-fg MediumBlue -pady 4
	pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # set xy vector
    set row [ frame $vectlabel.vect ]
    Button $row.a -text [G_msg "a. Select vector"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Select xy vector to display and georectify"]\
        -width 16 -anchor w \
		-command {GRMap::getxymap "vect" }
    Entry $row.b -width 35 -text "$GRMap::xyvect" \
          -textvariable GRMap::xyvect 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

	pack $vectlabel -side top -fill both -expand yes

    # Start georectify canvas
    set row [ frame $grstartup.start ]
    Button $row.a -text [G_msg "3. Georectify"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Start georectifying"]\
        -width 16 -anchor w -highlightthickness 4  -fg mediumblue \
		-command "GRMap::create"
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
    
    pack $grstartup -side top -fill both -expand yes
			    
    wm deiconify .grstart
	raise .grstart
    focus -force .grstart

}


###############################################################################

# Create window and canvas for displaying xy raster to georectify and for
# selecting coordinates in the xy system

proc GRMap::create { } {
    variable initwd
    variable initht
	variable grcursor
    variable win
    variable grcanvas_w
    variable grcanvas_h
	variable mappid
	variable grmapframe
	variable grcan
	variable map_ind
	variable grfile
	variable coords
	variable tmpdir
	variable xyloc
	variable xymset
	variable xygroup
	variable xyrast
	variable xyvect
	variable maptype
	variable mapregion

    global env
    global currmon
    global drawprog
	global GRMap::msg
	global grcoords
	
	if { $xymset=="" || (($xygroup=="" || $xyrast=="") && $xyvect=="")} {
		return
		}
	
	# close dialog to select mapset and raster
	destroy .grstart
	
	# set environment to xy location
	GRMap::setxyenv $xymset $xyloc	
	
	# need to turn off wind_override here

	# Initialize window and map geometry	
	set grcanvas_w $initwd
	set grcanvas_h $initht
	set env(GRASS_WIDTH) $initwd
	set env(GRASS_HEIGHT) $initht
	set drawprog 0
	set win ""

	# create temporary local WIND file to use with WIND_OVERRIDE whenever display has focus
	if {[info exists env(WIND_OVERRIDE)]} {unset env(WIND_OVERRIDE)}
	set mapregion "gr_region"
	run_panel "g.region -ug save=gr_region --o"
	set env(WIND_OVERRIDE) $mapregion

	# Zoom to map to georectify
	if { $maptype == "rast" } {
		GRMap::zoom_gregion [list "rast=$xyrast"]
	} elseif { $maptype == "vect" } {
		GRMap::zoom_gregion [list "vect=$xyvect"]
	}		

	# Create canvas monitor as top level mainframe
	toplevel .mapgrcan
	wm title .mapgrcan "Displaying xy map to be georectified"

    set grmapframe [MainFrame .mapgrcan.mf \
   		-textvariable GRMap::msg \
   		-progressvar drawprog -progressmax 100 -progresstype incremental]
   		
   	set mf_frame [$grmapframe getframe]

    # toolbar creation
    set map_tb  [$grmapframe addtoolbar]
    GRToolBar::create $map_tb

	# canvas creation
    set grcan [canvas $mf_frame.grcanvas \
		-borderwidth 0 -closeenough 10.0 -relief groove \
        -width $grcanvas_w -height $grcanvas_h ]
 
    # setting geometry
    place $grcan -in $mf_frame -x 0 -y 0 -anchor nw 
	
	pack $grcan -fill both -expand yes
 
    # indicator creation	
    set map_ind [$grmapframe addindicator -textvariable grcoords_mov \
    	-width 33 -justify left -padx 5 -bg white]

    pack $grmapframe -fill both -expand yes

	set grcursor [$grcan cget -cursor]

	GRMap::coordconv 

	# bindings for display canvas	
	
	# mouse handlers
	# The coordinate transforms should be done per monitor.
	bind $grcan <ButtonPress-1> {
		set eastcoord [eval GRMap::scrx2mape %x]
		set northcoord [eval GRMap::scry2mapn %y]
		set grcoords "$eastcoord $northcoord"
	}

	# When a monitor gets the keyboard focus
	# switch monitors in the tree if this isn't the selected one
	# I suspect that setting win is unnecessary (where is it used?)
	bind .mapgrcan <FocusIn> "
		global win env
		set env(WIND_OVERRIDE) $mapregion
		set win .mapgrcan
		"
		
	# Displays geographic coordinates in indicator window when cursor moved across canvas
	bind $grcan <Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval GRMap::scrx2mape %x]
		set northcoord [eval GRMap::scry2mapn %y]
		set grcoords_mov "$eastcoord $northcoord"
	}
	
	
	# TSW - inserting key command ability into gis.m
	# 9 May 2006
	# set some key commands to speed use
	
	# Return to previous zoom
	bind .mapgrcan <KeyPress-r> {
		GRMap::zoom_back
	}
	
	# set key strokes to change between tools
	# I've provided strokes for both right and left handed 
	# mouse users
	
	# Right handed
	# x - pointer
	# Zoom in - zoom in
	# zoom ouT - zoom out
	# pAn - pan
	# Query - query
	
	bind .mapgrcan <KeyPress-x> {
		GRToolBar::changebutton pointer
		GRMap::stoptool
	}
	bind .mapgrcan <KeyPress-z> {
		GRMap::stoptool
		GRToolBar::changebutton zoomin
		GRMap::zoombind 1
	}
	bind .mapgrcan <KeyPress-t> {
		GRMap::stoptool
		GRToolBar::changebutton zoomout
		GRMap::zoombind -1
	}
	bind .mapgrcan <KeyPress-a> {
		GRMap::stoptool
		GRToolBar::changebutton pan
		GRMap::panbind
	}

	# Left handed
	# poiNter - pointer
	# zoom In - zoom in
	# zoom Out - zoom out
	# Pan - pan
	# ? - query
	
	bind .mapgrcan <KeyPress-n> {
		GRToolBar::changebutton pointer
		GRMap::stoptool
	}
	bind .mapgrcan <KeyPress-i> {
		GRMap::stoptool
		GRToolBar::changebutton zoomin
		GRMap::zoombind 1
	}
	bind .mapgrcan <KeyPress-o> {
		GRMap::stoptool
		GRToolBar::changebutton zoomout
		GRMap::zoombind -1
	}
	bind .mapgrcan <KeyPress-p> {
		GRMap::stoptool
		GRToolBar::changebutton pan
		GRMap::panbind
	}
	

	# window configuration change handler for resizing
	bind $grcan <Configure> "GRMap::do_resize"

	# bindings for closing map display window
	bind .mapgrcan <Destroy> "GRMap::cleanup %W"
	
	#return to georectified location
	GRMap::resetenv
	
	#default selector tool
	GRMap::selector
	
	#start gcp window
	GRMap::gcpwin

}

###############################################################################
# create form for gcp management
proc GRMap::gcpwin {} {
	variable xygdb
	variable xyloc
	variable xymset
	variable xygroup
	variable gcpnum
	variable usegcp
	variable xyvect
	variable maptype
	variable xy
	variable geoc
	variable chk
	variable error_rms
	variable totalrms
	variable errval
	global xyentry
	global geoentry
	global grcoords
	global b1coords
	
	# initialize variables
	set gcpnum 1
	set usegcp($gcpnum) 1
	set xy($gcpnum) ""
	set geoc($gcpnum) ""
	set chk($gcpnum) ""
	set error_rms($gcpnum) ""
	set errval($gcpnum) 0.0
	
	toplevel .gcpwin

    set gcp_mf [MainFrame .gcpwin.mf \
   		-textvariable GRMap::gcpmsg]
   	
   	set GRMap::gcpmsg "                    Total RMS error"
   		
   	set gcp_frame [$gcp_mf getframe]

    # toolbar creation
    set gcp_tb  [$gcp_mf addtoolbar]
    GRMap::gcptb $gcp_tb

	# gcp form creation
    set gcp_sw [ScrolledWindow $gcp_frame.sw -relief flat \
    	-borderwidth 1 ]
    set gcp_sf [ScrollableFrame $gcp_sw.sf -height 150 -width 600]
    $gcp_sw setwidget $gcp_sf

	set gcpframe [$gcp_sf getframe]

	pack $gcp_sw -fill both -expand yes

	set gcp [frame $gcpframe.fr]
	pack $gcp -fill both -expand yes


    pack $gcp_mf -side top -expand yes -fill both -anchor n     


    # Scroll the options window with the mouse
    bind_scroll $gcp_sf
 
    set row [ frame $gcp.header ]
    Label $row.a -text "Use" \
    	-fg MediumBlue -width 3
    Label $row.b -text "xy coordinates" \
    	-fg MediumBlue -width 34
    Label $row.c -text "geographic coordinates" \
    	-fg MediumBlue -width 35
    Label $row.d -text "RMS error" \
    	-fg MediumBlue
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

	for {set gcpnum 1} {$gcpnum < 51 } { incr gcpnum } {
		set GRMap::usegcp($gcpnum) 1
		set row [ frame $gcp.row$gcpnum -bd 0]
		set chk($gcpnum) [checkbutton $row.a \
			-takefocus 0 \
			-variable GRMap::usegcp($gcpnum)]
			
		set xy($gcpnum) [entry $row.b -width 35  -bd 0 ]			  
		bind $xy($gcpnum) <FocusIn> "set xyentry %W"
		
		set geoc($gcpnum) [entry $row.c -width 35 -bd 0]
		bind $geoc($gcpnum) <FocusIn> "set geoentry %W"

		set error_rms($gcpnum) [entry $row.d -width 10  \
			-takefocus 0 \
			-textvariable GRMap::errval($gcpnum) -bd 0] 
			
		pack $chk($gcpnum) $xy($gcpnum) $geoc($gcpnum) $error_rms($gcpnum) -side left
		pack $row -side top -fill both -expand yes	
		
	}
	
	# import any existing points file for raster or gcp file for raster or vector
	if { $maptype == "rast" } {
		set gcpfile "$xygdb/$xyloc/$xymset/group/$xygroup/POINTS"
		if {[file exists $gcpfile] } {
			# do the import
			set gcpnum 1
			set pfile [open $gcpfile]
			set points [read $pfile]
			close $pfile
			regsub -all {[ ]+} $points " " points
			set plines [split $points "\n"]
			foreach gcpline $plines {
				if {[string match {\#*} $gcpline]} continue
				if {$gcpline == "" } continue
				set gcpline [string trim $gcpline " "] 
				set fields [split $gcpline { }]
				# assign variables
				$xy($gcpnum) insert	0 "[lindex $fields 0] [lindex $fields 1]"
				$geoc($gcpnum) insert 0 "[lindex $fields 2] [lindex $fields 3]"
				set usegcp($gcpnum)	"[lindex $fields 4]"
				incr gcpnum
			}
		}
	} elseif { $maptype == "vect" } {
		set gcpfile "$xygdb/$xyloc/$xymset/group/$xyvect/POINTS"
		if {[file exists $gcpfile] } {
			# do the import
			set gcpnum 1
			set pfile [open $gcpfile]
			set points [read $pfile]
			close $pfile
			regsub -all {[ ]+} $points " " points
			set plines [split $points "\n"]
			foreach gcpline $plines {
				if {[string match {\#*} $gcpline]} continue
				if {$gcpline == "" } continue
				set gcpline [string trim $gcpline " "] 
				set fields [split $gcpline { }]
				# assign variables
				$xy($gcpnum) insert	0 "[lindex $fields 0] [lindex $fields 1]"
				$geoc($gcpnum) insert 0 "[lindex $fields 2] [lindex $fields 3]"
				set usegcp($gcpnum)	1
				incr gcpnum
			}
		}
	}

	# set the focus to the first entry
	focus -force $xy(1)
	
    # setting geometry 
    #place $gcp_sw -in $gcp_frame -x 0 -y 0 -anchor nw 
    
    set GRMap::totalrms 0.0
	 
    # indicator creation	
    set gcp_ind [$gcp_mf addindicator -textvariable GRMap::totalrms \
    	-width 15 -justify left -padx 5 -bg white]    


	wm title .gcpwin [G_msg "Manage ground control points (GCPs)"]
	wm withdraw .gcpwin
	wm deiconify .gcpwin


}


###############################################################################
# toolbar for gcp manager window
proc GRMap::gcptb { gcptb } {
    global bgcolor
    global iconpath
        
    # gcp management buttons
    set bbox [ButtonBox $gcptb.bbox1 -spacing 0 -homogeneous 1 ]
    
    # save
    $bbox add -image [image create photo -file "$iconpath/file-save.gif"] \
        -command "GRMap::savegcp" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "save GCPs to POINTS file"]

    # clear
    $bbox add -image [image create photo -file "$iconpath/gui-gcperase.gif"] \
        -command "GRMap::cleargcp" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "clear ALL GCP entries"]

    # rms
    $bbox add -image [image create photo -file "$iconpath/gui-rms.gif"] \
        -command "GRMap::rmscalc" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -highlightbackground $bgcolor  -activebackground $bgcolor\
        -helptext [G_msg "calculate RMS error"]

    
    set recttype [menubutton $gcptb.recttype  \
		-image [image create photo -file "$iconpath/gui-georect.gif"] \
        -highlightthickness 0 -takefocus 0 -relief flat -borderwidth 1  \
        -highlightbackground $bgcolor -activebackground honeydew \
        -bg $bgcolor -width 32 -indicatoron 0 -direction below]
    DynamicHelp::register $recttype balloon [G_msg "Select georectification method and start georectification"]

	# menu to set rectification order
	set order [menu $recttype.order -type normal]
	
	$order add command \
		-label {1st order affine transformation (rasters & vectors). Requires 3+ GCPs.} \
        -command {GRMap::rectify 1}
	$order add command \
		-label {2nd order polynomial transformation (rasters). Requires 6+ GCPs.} \
        -command {GRMap::rectify 2}
	$order add command \
		-label {3rd order polynomial transformation (rasters). Requires 10+ GCPs.} \
        -command {GRMap::rectify 3}

	$recttype configure -menu $order
	
	pack $bbox -side left -anchor w -expand no -fill y
	pack $recttype -side left -anchor w -expand no -fill y 


}
###############################################################################
# save GCP's to POINTS file in xy location and mapset
proc GRMap::savegcp {} {
	variable currloc
	variable currmset
	variable xygdb
	variable xyloc
	variable xymset
	variable xygroup
	variable xyvect
	variable maptype
	variable gcpnum
	variable xy
	variable geoc
	variable usegcp
	variable array gcpline #array to store gcp coordinates as text for output

	if { $maptype == "rast" } {
		set gcpfile "$xygdb/$xyloc/$xymset/group/$xygroup/POINTS"
	} elseif { $maptype == "vect" } {
		set gcpfile "$xygdb/$xyloc/$xymset/group/$xyvect/POINTS"
		if {![file isdirectory [file dirname $gcpfile]] } {
			file mkdir [file dirname $gcpfile]
		}
	}
	set output [open $gcpfile w ] 
		puts $output "# Ground Control Points File"
		puts $output "# "
		puts $output "# target location: $currloc"
		puts $output "# target mapset: $currmset"
		puts $output "#unrectified xy     georectified east north"
		puts $output "#--------------     -----------------------"
		for {set gcpnum 1} {$gcpnum < 51 } { incr gcpnum } {
			set rowcount 0
			set gcpline($gcpnum) "[$xy($gcpnum) get]"
			append gcpline($gcpnum) "     [$geoc($gcpnum) get]"
			append gcpline($gcpnum) "     $usegcp($gcpnum)"
			if { [$xy($gcpnum) get] != "" && [$geoc($gcpnum) get] != "" && $usegcp($gcpnum) == 1} {
				puts $output $gcpline($gcpnum)
				incr rowcount
			}
		}
	close $output
}

###############################################################################
# calculate RMS error for GCP's
proc GRMap::rmscalc {} {
	variable gcpnum
	variable xy
	variable geoc
	variable usegcp
	variable errval
	variable totalrms
	
	set gcpcnt 0
	set rmssumsq 0.0
	set totalrms 0.0

	# calculate rms values for each point
	for {set gcpnum 1} {$gcpnum < 51 } { incr gcpnum } {
		if { [$xy($gcpnum) get] != "" && [$geoc($gcpnum) get] != "" && $usegcp($gcpnum) == 1} {
			set xyfields [split [$xy($gcpnum) get] { }]
			set geocfields [split [$geoc($gcpnum) get] { }]
			set x	[lindex $xyfields 0]
			set y	[lindex $xyfields 1]
			set e	[lindex $geocfields 0]
			set n	[lindex $geocfields 1]
			set GRMap::errval($gcpnum) [expr hypot($e-$x, $n-$y)]
			set rmssumsq [expr $rmssumsq + pow($errval($gcpnum),2)]
			incr gcpcnt
		}
	}

	# calculate total rms error for all points
	set GRMap::totalrms [expr sqrt($rmssumsq/$gcpcnt)]
}

###############################################################################
# run i.rectify to rectify raster group or v.transform to rectify vector
proc GRMap::rectify { rectorder } {
	variable xygdb
	variable xyloc
	variable currloc
	variable xymset
	variable currmset
	variable xygroup
	variable xyvect
	variable maptype
	variable mappid
	variable selftarget

	# First, switch to xy mapset
	GRMap::setxyenv $xymset $xyloc
	

	if { $maptype == "rast" } {
		# count useable GCP's in points file
		set gcpfile "$xygdb/$xyloc/$xymset/group/$xygroup/POINTS"
		if {[file exists $gcpfile] } {
			# do the import
			set gcpcnt 0
			set pfile [open $gcpfile]
			set points [read $pfile]
			close $pfile
			regsub -all {[ ]+} $points " " points
			set plines [split $points "\n"]
			foreach gcpline $plines {
				if {[string match {\#*} $gcpline]} continue
				if {$gcpline == "" } continue
				set gcpline [string trim $gcpline " "] 
				set fields [split $gcpline { }]
				# count gcps
				if {[lindex $fields 0]!="" && [lindex $fields 1]!="" && [lindex $fields 2]!=""  
					&& [lindex $fields 3]!="" && [lindex $fields 4]==1} {
					incr gcpcnt
				}
			}
			if { $gcpcnt<3 || ($gcpcnt<6 && $rectorder==2) || ($gcpcnt<10 && $rectorder==3) } {
				set msg "Insufficient ground control points for georectification method. \
					You need at least 3 points for 1st order, 6 points for 2nd order, and 10 points for 3rd order"
				tk_messageBox -message $msg -parent .gcpwin -type ok
			}
		} else { 
			set msg "There is no POINTS file of ground control points for group. \
				You must create ground control points before georectifying map."
			tk_messageBox -message $msg -parent .gcpwin -type ok
		}	
		set cmd "i.rectify -ca group=$xygroup extension=$mappid order=$rectorder"
		runcmd $cmd
	} elseif { $maptype == "vect" && $rectorder == 1} {
		# count useable GCP's in points file
		set gcpfile "$xygdb/$xyloc/$xymset/group/$xyvect/POINTS"
		if {[file exists $gcpfile] } {
			# do the import
			set gcpcnt 0
			set pfile [open $gcpfile]
			set points [read $pfile]
			close $pfile
			regsub -all {[ ]+} $points " " points
			set plines [split $points "\n"]
			foreach gcpline $plines {
				if {[string match {\#*} $gcpline]} continue
				if {$gcpline == "" } continue
				set gcpline [string trim $gcpline " "] 
				set fields [split $gcpline { }]
				# count gcps
				if {[lindex $fields 0]!="" && [lindex $fields 1]!="" && [lindex $fields 2]!=""  
					&& [lindex $fields 3]!=""} {
					incr gcpcnt
				}
			}
			if { $gcpcnt < 1 } {
				set msg "No valid ground control points in gcp file. \
				You must create valid ground control points before georectifying map."
				tk_messageBox -message $msg -parent .gcpwin -type ok
			}
		} else { 
			set msg "There is no gcp file of ground control points for vector. \
				You must create ground control points before georectifying map."
			tk_messageBox -message $msg -parent .gcpwin -type ok
		}
		set outname "$xyvect"
		append outname "_"
		append outname "$mappid"
		set cmd "v.transform -q input=$xyvect output=$outname pointsfile=$gcpfile"
		runcmd $cmd
		# copy vector files from source to target location and mapset
		if { $selftarget == 0 } {
			set xysource "$xygdb/$xyloc/$xymset/vector/$outname"
			set xytarget "$xygdb/$currloc/$currmset/vector/$outname"
			set xyfile "$xysource"
			append xyfile "/coor"
			set counter 1
			while { $counter < 100 } {
				if { [file exists $xyfile] } {
					file copy -force $xysource $xytarget
					file delete -force $xysource
					set counter 101
				}
				after 100
				incr counter
			}
		}
	} else { 
		GRMap::resetenv 
		return
	}
	

    # Return to georectified mapset
	GRMap::resetenv
}

###############################################################################
# clear all GCP entries
proc GRMap::cleargcp {} {
	variable xy
	variable geoc
	variable usegcp
	variable error_rms
	variable gcpnum
	variable grcan

	for {set gcpnum 1} {$gcpnum < 51 } { incr gcpnum } {
		set usegcp($gcpnum) 1
		$xy($gcpnum) delete 0 end
		$geoc($gcpnum) delete 0 end
		$error_rms($gcpnum) delete 0 end
	}
	$grcan delete gcpvert gcphoriz
}

###############################################################################

# Calculate boxes with a given aspect ratio.

# Sense - 0 means largest no larger, 1 means smallest no smaller
# We will change box 1
proc GRMap::shrinkwrap {sense nsew1 ar2 } {
	foreach {n1 s1 e1 w1} $nsew1 {break}

	set ns1 [expr {$n1 - $s1}]
	set ew1 [expr {$e1 - $w1}]

	# Width / height
	# Big aspect ratio is wide, small aspect ratio is tall
	set ar1 [expr { 1.0 * $ew1 / $ns1 }]
	
	# If box one is wider than box 2.
	# (or box one isn't wider than box 2 and the sense is inverted)
	if {($ar1 > $ar2) ^ $sense} {
		# n1 and s1 are unchanged
		# e1 and w1 must be scaled by ar2
		set rn1 $n1
		set rs1 $s1
		set goal [expr {$ns1 * $ar2}]
		set midpoint [expr {$w1 + $ew1 / 2}]
		set re1 [expr {$midpoint + $goal / 2}]
		set rw1 [expr {$midpoint - $goal / 2}]
	} else {
		# e1 and w1 are unchanged
		# n1 and s1 must be scaled by 1/ar2
		set re1 $e1
		set rw1 $w1
		set goal [expr {$ew1 / $ar2}]
		set midpoint [expr {$s1 + $ns1 / 2}]
		set rn1 [expr {$midpoint + $goal / 2}]
		set rs1 [expr {$midpoint - $goal / 2}]
	}

	set result [list $rn1 $rs1 $re1 $rw1]
	
	return $result
}

###############################################################################
# map display procedures

# draw map using png driver and open in canvas
proc GRMap::drawmap { } {
	variable grcanvas_h
	variable grcanvas_w
	variable grcan
	variable grcanmodified
	variable monitor_zooms
	variable previous_monitor
	variable xyloc
	variable xymset

	set w [winfo width $grcan]
	set h [winfo height $grcan]

	# Get whether or not the canvas was modified or zoomed
	# grcanmodified has levels: 0  is none, 1 is zoom, 2 is geometry.
	# 1 doesn't require new setting in explore mode
	set mymodified $grcanmodified

	# Make sure grcanvas_h and grcanvas_w are correct
	if { $grcanvas_w != $w || $grcanvas_h != $h } {
		# Flag this as a modified canvas
		# Modified canvas is level 2!
		set mymodified 2
		set grcanvas_w $w
		set grcanvas_h $h
	}

	# Set the region from our zoom settings
	GRMap::gregion_zoom

	# Redo the driver settings if the geometry has changed or
	# if we weren't the previous monitor.
	if {$mymodified != 0 } {
		set grcanmodified 0
		set previous_monitor "none"
		# The canvas or view has been modified
		# Redo the map settings to match the canvas
		GRMap::driversettings
	}
	
	# Render all the layers
	GRMap::runprograms [expr {$mymodified != 0}]
}

# set up driver geometry and settings
proc GRMap::driversettings { } {
	variable grcanvas_h
	variable grcanvas_w
	variable driver_w
	variable driver_h
	variable grfile
	variable xyloc
	variable xymset
	variable monitor_zooms
	global env
				
	if {[info exists env(MONITOR_OVERRIDE)]} {unset env(MONITOR_OVERRIDE)}

	set driver_h $grcanvas_h
	set driver_w $grcanvas_w

	# stop display driver in order to set display environment parameters
	runcmd "d.mon stop=gism"
		
	#set display environment
	# First, switch to xy mapset
    GRMap::setxyenv $xymset $xyloc
	set env(GRASS_WIDTH) "$driver_w"
	set env(GRASS_HEIGHT) "$driver_h"
	set env(GRASS_PNGFILE) "$grfile"
	set env(GRASS_BACKGROUNDCOLOR) "ffffff"
	set env(GRASS_TRANSPARENT) "FALSE"
	set env(GRASS_PNG_AUTO_WRITE) "TRUE"
	set env(GRASS_TRUECOLOR) "TRUE"
    # Return to georectified mapset
    GRMap::resetenv
		
	#restart display driver to apply environment settings
	runcmd "d.mon start=gism -s"

}

# Run the programs to clear the map and draw all of the layers
proc GRMap::runprograms { mod } {
	variable grcan
	variable grcanvas_w
	variable grcanvas_h
	variable grmapframe
	variable grfile
	variable tmpdir
	variable xygroup
	variable xyloc
	variable xymset
	variable xyrast
	variable xyvect
	variable maptype
	variable xygroup
	
	global env
	global drawprog
	global GRMap::msg

	# First, switch to xy mapset
    GRMap::setxyenv $xymset $xyloc
				
	set drawprog 0
		
    set GRMap::msg "please wait..."
    $grmapframe showstatusbar progression 
		
	incr drawprog
	set env(MONITOR_OVERRIDE) "gism"
	# Setting the font really only needs to be done once per display start
	set cmd {d.font romans}
	runcmd $cmd
	incr drawprog
	set cmd "d.frame -e"
	runcmd $cmd
	incr drawprog
	# display map for georectification
	if { $maptype == "rast" } {
		set cmd "d.rast map=$xyrast"
	} elseif { $maptype == "vect" } {
		set cmd "d.vect map=$xyvect"
	}
	runcmd $cmd
	incr drawprog

	if {[info exists env(MONITOR_OVERRIDE)]} {unset env(MONITOR_OVERRIDE)}
	incr drawprog

	$grcan delete all
	set currdir [pwd]
	cd $tmpdir
	incr drawprog

	image create photo grimg -file "$grfile" 
	incr drawprog
	$grcan create image 0 0 -anchor nw \
		-image "grimg" \
		-tag gr
	cd $currdir

	set drawprog 100

	GRMap::coordconv
	set drawprog 0
	if { $maptype == "rast" } {
		set GRMap::msg "Georectifying $xygroup"
	} elseif { $maptype == "vect" } {
		set GRMap::msg "Georectifying $xyvect"
	}
	$grmapframe showstatusbar status 

    # Return to georectified mapset
    GRMap::resetenv	
    return

}

###############################################################################
# map display server
# The job of these procedures is to make sure that:
# 1: we are never running more than one update at once.
# 2: we don't do exactly the same update multiple times.

proc GRMap::display_server {} {
	variable redrawrequest
	variable gcpnum
	variable xy
	
	set gcpcnt 0

	if {$redrawrequest} {
		# Mark that this monitor no longer wants to be redrawn
		set redrawrequest 0
		# Redraw the monitor canvas
		GRMap::drawmap
		#draw gcp marks
		for {set gcpnum 1} {$gcpnum < 51 } { incr gcpnum } {
			if { [$xy($gcpnum) get] != "" } {
				set xyfields [split [$xy($gcpnum) get] { }]
				set mapx [lindex $xyfields 0]
				set mapy [lindex $xyfields 1]
				set x [eval GRMap::mape2scrx $mapx]
				set y [eval GRMap::mapn2scry $mapy]
				GRMap::markgcp $x $y
				incr gcpcnt
			}
		}
	}

	# Do me again in a short period of time.
	# vwait might be appropriate here
	after 100 GRMap::display_server
}

# Request a redraw on a monitor
proc GRMap::request_redraw {modified} {
	variable redrawrequest
	variable grcanmodified

	set redrawrequest 1

	set grcanmodified $modified
}

# Start the server
after idle GRMap::display_server

###############################################################################

proc GRMap::do_resize {} {
	variable grcanvas_w
	variable grcanvas_h
	variable grcan
	

	# Get the actual width and height of the canvas
	set w [winfo width $grcan]
	set h [winfo height $grcan]

	# Only actually resize and redraw if the size is different
	if { $grcanvas_w != $w || $grcanvas_h != $h } {
		$grcan delete gr
		GRMap::request_redraw 1
	}
}


###############################################################################

# erase to white
proc GRMap::erase { } {
	variable grcan
		
	$grcan delete gr
	$grcan delete all
	
}


###############################################################################

# stop display management tools
proc GRMap::stoptool { } {
	global GRMap::msg
	variable grcan
	variable maptype
	variable xygroup
	variable xyvect
	
	# release bindings
	bind $grcan <1> ""
	bind $grcan <2> ""
	bind $grcan <3> ""
	bind $grcan <B1-Motion> ""
	bind $grcan <ButtonRelease-1> ""

	# reset status display to normal
	if { $maptype == "rast" } {
		set GRMap::msg "Georectifying $xygroup"
	} elseif { $maptype == "vect" } {
		set GRMap::msg "Georectifying $xyvect"
	}

	GRMap::restorecursor 		
	
}

###############################################################################
# set bindings for GCP selection tool
proc GRMap::selector { } {
	variable grcan
	global grcoords
	global xyentry
	variable grcoords_mov

	GRMap::setcursor "crosshair"

	bind $grcan <ButtonPress-1> {
		set eastcoord [eval GRMap::scrx2mape %x]
		set northcoord [eval GRMap::scry2mapn %y]
		set grcoords "$eastcoord $northcoord"
		GRMap::markgcp %x %y
		$xyentry delete 0 end
		$xyentry insert 0 $grcoords
		focus -force [tk_focusNext $xyentry]
	}
		
	# Displays geographic coordinates in indicator window when cursor moved across canvas
	bind $grcan <Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval GRMap::scrx2mape %x]
		set northcoord [eval GRMap::scry2mapn %y]
		set grcoords_mov "$eastcoord $northcoord"
	}

}

###############################################################################
# mark ground control point
proc GRMap::markgcp { x y } {
	
	# create gcp point on georectify canvas for each mouse click

	variable grcan
	$grcan create line $x [expr $y-5] $x [expr $y+5] -tag gcpv \
		-fill DarkGreen -width 2 -tag "gcpvert"
	$grcan create line [expr $x-5] $y [expr $x+5] $y -tag gcph \
		-fill red -width 2 -tag "gcphoriz"
	
}


###############################################################################
# procedures for zooming and setting region

# Get the current zoom region
# Returns a list in zoom_attrs order (n s e w nsres ewres)
proc GRMap::currentzoom { } {
	variable zoom_attrs
	variable monitor_zooms
	variable grcanvas_w
	variable grcanvas_h
	variable xyloc
	variable xymset

	# Fetch the current zoom settings
	set region {}
	foreach attr $zoom_attrs {
		lappend region $monitor_zooms(1,$attr)
	}

	# Set the region to the smallest region no smaller than the canvas
	set grcanvas_ar [expr {1.0 * $grcanvas_w / $grcanvas_h}]
	set expanded_nsew [GRMap::shrinkwrap 1 [lrange $region 0 3] $grcanvas_ar]
	foreach {n s e w} $expanded_nsew {break}
	# Calculate the resolutions
	lappend expanded_nsew [expr {1.0 * ($n - $s) / $grcanvas_h}]
	lappend expanded_nsew [expr {1.0 * ($e - $w) / $grcanvas_w}]
	set region $expanded_nsew

	return $region
}

# Zoom or pan to new bounds in the zoom history
# Arguments are either n s e w or n s e w nsres ewres
proc GRMap::zoom_new { args} {
	variable monitor_zooms
	variable zoomhistories
	variable zoom_attrs
	variable xyloc
	variable xymset

	# Demote all of the zoom history
	for {set i $zoomhistories} {$i > 1} {incr i -1} {
		set iminus [expr {$i - 1}]
		foreach attr $zoom_attrs {
			catch {set monitor_zooms($i,$attr) $monitor_zooms($iminus,$attr)}
		}
	}

	# If cols and rows aren't present we just use what was already here.
	set present_attrs [lrange $zoom_attrs 0 [expr {[llength $args] - 1}]]
	
	foreach value $args attr $present_attrs {
		set monitor_zooms(1,$attr) $value
	}

}

# Zoom to the previous thing in the zoom history
proc GRMap::zoom_previous {} {
	variable monitor_zooms
	variable zoomhistories
	variable zoom_attrs
	variable xyloc
	variable xymset

	# Remember the first monitor
	set old1 {}
	foreach attr $zoom_attrs {
		lappend old1 $monitor_zooms(1,$attr)
	}

	# Promote all of the zoom history
	for {set i 1} {$i < $zoomhistories } {incr i} {
		set iplus [expr {$i + 1}]
		foreach attr $zoom_attrs {
			catch {set monitor_zooms($i,$attr) $monitor_zooms($iplus,$attr)}
		}
	}

	# Set the oldest thing in the history to where we just were
	foreach value $old1 attr $zoom_attrs {
		set monitor_zooms($zoomhistories,$attr) $value
	}

}


# Zoom to something loaded from a g.region command
proc GRMap::zoom_gregion { args} {
	variable xyloc
	variable xymset
	variable mapregion
	global env

	# First, switch to xy mapset
    GRMap::setxyenv $xymset $xyloc

	# set current region to WIND file	
	unset env(WIND_OVERRIDE)
	
	if {![catch {open [concat "|g.region" "-ug" $args] r} input]} {
		while {[gets $input line] >= 0} {
			regexp -nocase {^([a-z]+)=(.*)$} $line trash key value
			set parts($key) $value	
		}
		close $input

		#set current region back to local wind settings
		set env(WIND_OVERRIDE) $mapregion

		GRMap::zoom_new $parts(n) $parts(s) $parts(e) $parts(w) $parts(nsres) $parts(ewres)
	}

    # Return to georectified mapset
    GRMap::resetenv
}

# Set the region from the current zoom
proc GRMap::gregion_zoom { args} {
	variable zoom_attrs
	variable xyloc
	variable xymset
	variable mapregion
	global env

	set values [GRMap::currentzoom]

	set options {}
	foreach attr $zoom_attrs value $values {
		lappend options "$attr=$value"
	}

	# First, switch to xy mapset
    GRMap::setxyenv $xymset $xyloc

	run_panel [concat g.region $options $args]

    # Return to georectified mapset
    GRMap::resetenv
}

# zoom to extents and resolution of displayed map for georectifying
proc GRMap::zoom_map { } {
	variable xyrast
	variable xyvect
	variable xymset
	variable xyloc
	variable maptype
	variable grcan

	# set region to match map to georectify
	if { $maptype == "rast" } {
		GRMap::zoom_gregion [list "rast=$xyrast"]
	} elseif { $maptype == "vect" } {
		GRMap::zoom_gregion [list "vect=$xyvect"]
	}		

	$grcan delete gr
	GRMap::request_redraw 1

}


# zoom back
proc GRMap::zoom_back { } {
	variable grcan

	GRMap::zoom_previous
	$grcan delete gr
	GRMap::request_redraw 1
}


###############################################################################
# interactive zooming procedures
# zoom bindings
proc GRMap::zoombind { zoom } {
	variable grcan
	global grcoords
	global GRMap::msg
    global areaX1 areaY1 areaX2 areaY2
    
    # initialize zoom rectangle corners

	set areaX1 0
	set areaY1 0
	set areaX2 0
	set areaY2 0

	GRMap::setcursor "plus"
	
	if {$zoom == 1} {
		set GRMap::msg "Drag or click mouse to zoom"
	} elseif {$zoom == -1} {
		set GRMap::msg "Drag or click mouse to unzoom"
	}

	bind $grcan <1> {
		GRMap::markzoom %x %y
		}
	bind $grcan <B1-Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval GRMap::scrx2mape %x]
		set northcoord [eval GRMap::scry2mapn %y]
		set grcoords_mov "$eastcoord $northcoord"
		GRMap::drawzoom %x %y
		}
	bind $grcan <ButtonRelease-1> "GRMap::zoomregion $zoom"

}

# start zoom rectangle
proc GRMap::markzoom { x y} {
    global areaX1 areaY1 areaX2 areaY2
    variable grcan

	# initialize corners
	set areaX1 0
	set areaY1 0
	set areaX2 0
	set areaY2 0
    
    set areaX1 [$grcan canvasx $x]
    set areaY1 [$grcan canvasy $y]
    $grcan delete area
}

# draw zoom rectangle
proc GRMap::drawzoom { x y } {
	variable grcan
    global areaX1 areaY1 areaX2 areaY2
	    
	set xc [$grcan canvasx $x]
	set yc [$grcan canvasy $y]
	
	if {($areaX1 != $xc) && ($areaY1 != $yc)} {
		$grcan delete area
		$grcan addtag area withtag \
			[$grcan create rect $areaX1 $areaY1 $xc $yc \
			-outline yellow -width 2]
		set areaX2 $xc
		set areaY2 $yc
	}
}


# zoom region
proc GRMap::zoomregion { zoom } {
	variable grcan
	variable grcanvas_h
	variable grcanvas_w
	variable monitor_zooms
	variable map_n
	variable map_s
	variable map_e
	variable map_w
	variable map_ew
	variable map_ns	
	
    global areaX1 areaY1 areaX2 areaY2
    
    # if click and no drag, zoom in or out by 80% of original area
    
	if {($areaX2 == 0) && ($areaY2 == 0)} {
		set X2 [expr {$areaX1 + ($grcanvas_w / (2 * sqrt(2)))} ]
		set X1 [expr {$areaX1 - ($grcanvas_w / (2 * sqrt(2)))} ]
		set Y2 [expr {$areaY1 + ($grcanvas_h / (2 * sqrt(2)))} ]
		set Y1 [expr {$areaY1 - ($grcanvas_h / (2 * sqrt(2)))}]	
		set areaX1 $X1
		set areaY1 $Y1
		set areaX2 $X2
		set areaY2 $Y2
	}
    
	
	# get region extents
	foreach {map_n map_s map_e map_w} [GRMap::currentzoom] {break}
	
	# get zoom rectangle extents in canvas coordinates
	if { $areaX2 < $areaX1 } {
		set cright $areaX1
		set cleft $areaX2
	} else {
		set cleft $areaX1 
		set cright $areaX2
	}
	
	if { $areaY2 < $areaY1 } {
		set cbottom $areaY1
		set ctop $areaY2
	} else {
		set ctop $areaY1 
		set cbottom $areaY2
	}

	# get zoom rectangle extents in map coordinates
	
	set north [scry2mapn $ctop]
	set south [scry2mapn $cbottom]
	set east  [scrx2mape $cright]
	set west  [scrx2mape $cleft]
	
	# zoom in
	if { $zoom == 1 } {
		GRMap::zoom_new $north $south $east $west
	}
	
	#zoom out
	# Guarantee that the current region fits in the new box on the screen.
	if { $zoom == -1 } {
		# This effectively zooms out by the maxmimum of the two scales
		set nsscale [expr { ($map_n - $map_s) / ($north - $south) }]
		set ewscale [expr { ($map_e - $map_w) / ($east - $west) }]

		set upnorth   [expr { $map_n + $nsscale * ($map_n - $north) }]
		set downsouth [expr { $map_s + $nsscale * ($map_s - $south) }]
		set backeast  [expr { $map_e + $ewscale * ($map_e - $east) }]
		set outwest   [expr { $map_w + $ewscale * ($map_w - $west) }]

		GRMap::zoom_new $upnorth $downsouth $backeast $outwest
	}

	# redraw map
	$grcan delete gr
	$grcan delete area
	GRMap::request_redraw  1
}



###############################################################################
#procedures for panning

# pan bindings
proc GRMap::panbind { } {
	variable grcan
	global grcoords
	global GRMap::msg

    set GRMap::msg "Drag with mouse to pan"
    	
	GRMap::setcursor "hand2"

	bind $grcan <1> {GRMap::startpan %x %y}
	bind $grcan <B1-Motion> {
		set scrxmov %x
		set scrymov %y
		set eastcoord [eval GRMap::scrx2mape %x]
		set northcoord [eval GRMap::scry2mapn %y]
		set grcoords_mov "$eastcoord $northcoord"
		GRMap::dragpan %x %y
		}
	bind $grcan <ButtonRelease-1> {
		GRMap::pan
		}
}


proc GRMap::startpan { x y} {
	variable start_x 
	variable start_y
    variable from_x 
    variable from_y
    variable to_x
    variable to_y
	variable grcan

    set start_x [$grcan canvasx $x]
    set start_y [$grcan canvasy $y]
	set from_x $start_x
    set from_y $start_y
	set to_x $start_x
    set to_y $start_y

}

proc GRMap::dragpan { x y} {
	variable start_x 
	variable start_y
    variable from_x 
    variable from_y
    variable to_x
    variable to_y
	variable grcan

    set to_x [$grcan canvasx $x]
    set to_y [$grcan canvasy $y]
    $grcan move current [expr {$to_x-$start_x}] [expr {$to_y-$start_y}]
    
    set start_y $to_y
   	set start_x $to_x
}

proc GRMap::pan { } {
	variable start_x 
	variable start_y
    variable from_x 
    variable from_y
    variable to_x
    variable to_y
	variable grcan
	variable monitor_zooms
	variable map_n
	variable map_s
	variable map_e
	variable map_w
	variable map_ew
	variable map_ns	
	
	# get map coordinate shift    
    set from_e [scrx2mape $from_x]
    set from_n [scry2mapn $from_y]
    set to_e   [scrx2mape $to_x]
    set to_n   [scry2mapn $to_y]
    
	# get region extents
	foreach {map_n map_s map_e map_w} [GRMap::currentzoom] {break}

	# set new region extents
	set north [expr {$map_n - ($to_n - $from_n)}]
	set south [expr {$map_s - ($to_n - $from_n)}]
	set east  [expr {$map_e - ($to_e - $from_e)}]
	set west  [expr {$map_w - ($to_e - $from_e)}]
	
	# reset region and redraw map
	GRMap::zoom_new $north $south $east $west
	
	$grcan delete gr
	GRMap::request_redraw 1
}

###############################################################################

proc GRMap::setcursor {  ctype } {
	variable grcan

	$grcan configure -cursor $ctype
	return
}

proc GRMap::restorecursor {} {
	variable grcursor
	variable grcan
	
	$grcan configure -cursor $grcursor
	return
}

###############################################################################

#	Set up initial variables for screen to map conversion
proc GRMap::coordconv { } {

	variable map_n
	variable map_s
	variable map_e
	variable map_w
	variable map_ew
	variable map_ns	
	variable scr_n
	variable scr_s
	variable scr_e
	variable scr_w
	variable scr_ew
	variable scr_ns
	variable map2scrx_conv
	variable map2scry_conv
	variable grcanvas_w
	variable grcanvas_h
	variable monitor_zooms
	global grimg

	# get region extents
	foreach {map_n map_s map_e map_w} [GRMap::currentzoom] {break}
	
# 	calculate dimensions

	set map_n [expr {1.0*($map_n)}]
	set map_s [expr {1.0*($map_s)}]
	set map_e [expr {1.0*($map_e)}]
	set map_w [expr {1.0*($map_w)}]
	
	set map_ew [expr {$map_e - $map_w}]
	set map_ns [expr {$map_n - $map_s}]


#	get current screen geometry
if { [info exists "grimg"] } {
	set scr_ew [image width "grimg"]
	set scr_ns [image height "grimg"]
	set scr_e [image width "grimg"]
	set scr_s [image height "grimg"]
}	else {
	set scr_ew $grcanvas_w
	set scr_ns $grcanvas_h
	set scr_e $grcanvas_w
	set scr_s $grcanvas_h
}

	set scr_n 0.0
	set scr_w 0.0

	
# 	calculate conversion factors. Note screen is from L->R, T->B but map
# 	is from L->R, B->T

	set map2scrx_conv [expr {$scr_ew / $map_ew}]
	set map2scry_conv [expr {$scr_ns / $map_ns}]
		
# 	calculate screen dimensions and offsets

	if { $map2scrx_conv > $map2scry_conv } {
		set map2scrx_conv $map2scry_conv
	} else {
		set map2scry_conv $map2scrx_conv
	}

}

###############################################################################


# screen to map and map to screen conversion procedures

# map north to screen y
proc GRMap::mapn2scry { north } {
	variable map_n
	variable scr_n
	variable map2scry_conv

	return [expr {$scr_n + (($map_n - $north) * $map2scry_conv)}]
}

# map east to screen x
proc GRMap::mape2scrx { east } {
	variable map_w
	variable scr_w
	variable map2scrx_conv

	return [expr {$scr_w + (($east - $map_w) * $map2scrx_conv)}]

}

# screen y to map north
proc GRMap::scry2mapn { y } {
	variable map_n
	variable scr_n
	variable map2scry_conv

	return [expr {$map_n - (($y - $scr_n) / $map2scry_conv)}]

}

# screen x to map east
proc GRMap::scrx2mape { x } {
	variable map_w
	variable scr_w
	variable map2scrx_conv

	return [expr {$map_w + (($x - $scr_w) / $map2scrx_conv)}]

}

###############################################################################
# cleanup procedure on closing window
proc GRMap::cleanup { destroywin} {
	variable mapregion
	variable xymset
	variable xyloc

	# First, switch to xy mapset
    GRMap::setxyenv $xymset $xyloc

	if { $destroywin == ".mapgrcan" } { 
		runcmd "g.mremove -f region=$mapregion "
	}
	
	# close GCP management window too
	if { [winfo exists .gcpwin] } { destroy .gcpwin }
	
	# reset to original location and mapset
	GRMap::resetenv

	
}

###############################################################################
