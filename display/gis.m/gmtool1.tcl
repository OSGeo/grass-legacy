###############################################################
# gmtool1.tcl - upper toolbar file for GRASS GIS Manager main window
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmToolBar1 {
    variable toolbar
}

proc GmToolBar1::create { tb } {
    global gmpath
    global bgcolor
    global mon
    variable toolbar

    set toolbar $tb



    # Raster Layers
    set bbox0 [ButtonBox $toolbar.bbox0 -spacing 0 -background $bgcolor ]
    
    # add monitor
    $bbox0 add -image [image create photo -file "$gmpath/startmon.gif"] \
        -command "Gm::startmon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Start new map display monitor"]

    pack $bbox0 -side left -anchor w

    set sep0 [Separator $toolbar.sep0 -orient vertical -background aquamarine2 ]
    pack $sep0 -side left -fill y -padx 5 -anchor w

    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 -background $bgcolor ]
    
    # add raster
    $bbox1 add -image [image create photo -file "$gmpath/raster.gif"] \
        -command "GmTree::add raster" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add raster layer"]

    # add RGB or HIS layer
    $bbox1 add -image [image create photo -file "$gmpath/rgbhis.gif"] \
        -command "GmTree::add rgbhis" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add RGB or HIS layer"]

    # add histogram layer
    $bbox1 add -image [image create photo -file "$gmpath/histogram.gif"] \
        -command "GmTree::add hist" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add histogram layer"]

    # add cell values layer
    $bbox1 add -image [image create photo -file "$gmpath/rastnums.gif"] \
        -command "GmTree::add rnums" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add cell values layer"]

    # add cell values layer
    $bbox1 add -image [image create photo -file "$gmpath/rastarrows.gif"] \
        -command "GmTree::add arrows" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add directional arrows layer"]

    # add legend
    $bbox1 add -image [image create photo -file "$gmpath/legend.gif"] \
    	-command "GmTree::add legend" -highlightthickness 0 -takefocus 0 \
    	-relief raised -borderwidth 2 -helptext [G_msg "Add raster legend layer"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical -background aquamarine2 ]
    pack $sep1 -side left -fill y -padx 5 -anchor w


    # VECTOR LAYERS
    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 0 -background $bgcolor ]

    # add vector
    $bbox2 add -image [image create photo -file "$gmpath/vector.gif"] \
        -command "GmTree::add vector" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add vector layer"]

    # add chart
    $bbox2 add -image [image create photo -file "$gmpath/chart.gif"] \
        -command "GmTree::add chart" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add thematic charts layer"]

    # add thematic
    $bbox2 add -image [image create photo -file "$gmpath/thematic.gif"] \
        -command "GmTree::add thematic" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add thematic map layer"]

    pack $bbox2 -side left -anchor w

    set sep2 [Separator $toolbar.sep2 -orient vertical -background aquamarine2 ]
    pack $sep2 -side left -fill y -padx 5 -anchor w

    # Text Layers
    set bbox3 [ButtonBox $toolbar.bbox3 -spacing 0 -background $bgcolor ]

    # add paint labels
    $bbox3 add -image [image create photo -file "$gmpath/labels.gif"] \
        -command "GmTree::add labels" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add paint labels layer (from directory paint/labels)"]

    # add text
    $bbox3 add -image [image create photo -file "$gmpath/maptext.gif"] \
        -command "GmTree::add ctext" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add text layer"]
    pack $bbox3 -side left -anchor w

    set sep3 [Separator $toolbar.sep3 -orient vertical -background aquamarine2 ]
    pack $sep3 -side left -fill y -padx 5 -anchor w
    
    # Command layer
    set bbox4 [ButtonBox $toolbar.bbox4 -spacing 0 -background $bgcolor ]

	# add command
    $bbox4 add -image [image create photo -file "$gmpath/cmd.gif"] \
        -command "GmTree::add cmd" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -helptext [G_msg "Add command layer"]

    pack $bbox4 -side left -anchor w



}

