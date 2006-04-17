###############################################################
# gmtool1.tcl - upper toolbar file for GRASS GIS Manager main window
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmToolBar1 {
    variable toolbar
}

proc GmToolBar1::create { tb } {
    global iconpath
    global mon
    global bgcolor
    variable toolbar

    set toolbar $tb



    # Raster Layers
    set bbox0 [ButtonBox $toolbar.bbox0 -spacing 0]
    
    # add monitor
    $bbox0 add -image [image create photo -file "$iconpath/gui-startmon.gif"] \
        -command "Gm::startmon" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Start new map display monitor"]

    pack $bbox0 -side left -anchor w

    set sep0 [Separator $toolbar.sep0 -orient vertical -background aquamarine2 ]
    pack $sep0 -side left -fill y -padx 5 -anchor w

    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 ]
    
    # add raster
    $bbox1 add -image [image create photo -file "$iconpath/element-cell.gif"] \
        -command "GmTree::add raster" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add raster layer"]

    # add RGB or HIS layer
    $bbox1 add -image [image create photo -file "$iconpath/channel-rgb.gif"] \
        -command "GmTree::add rgbhis" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add RGB or HIS layer"]

    # add histogram layer
    $bbox1 add -image [image create photo -file "$iconpath/module-d.histogram.gif"] \
        -command "GmTree::add hist" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add histogram layer"]

    # add cell values layer
    $bbox1 add -image [image create photo -file "$iconpath/module-d.rast.num.gif"] \
        -command "GmTree::add rnums" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add cell values layer"]

    # add cell values layer
    $bbox1 add -image [image create photo -file "$iconpath/module-d.rast.arrow.gif"] \
        -command "GmTree::add arrows" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add directional arrows layer"]

    # add legend
    $bbox1 add -image [image create photo -file "$iconpath/module-d.legend.gif"] \
    	-command "GmTree::add legend" -highlightthickness 0 -takefocus 0 \
        -highlightbackground $bgcolor \
    	-relief raised -borderwidth 2 -helptext [G_msg "Add raster legend layer"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical -background aquamarine2 ]
    pack $sep1 -side left -fill y -padx 5 -anchor w


    # VECTOR LAYERS
    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 0 ]

    # add vector
    $bbox2 add -image [image create photo -file "$iconpath/element-vector.gif"] \
        -command "GmTree::add vector" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add vector layer"]

    # add thematic
    $bbox2 add -image [image create photo -file "$iconpath/module-d.vect.thematic.gif"] \
        -command "GmTree::add thematic" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add thematic map layer"]

    pack $bbox2 -side left -anchor w

    # add chart
    $bbox2 add -image [image create photo -file "$iconpath/module-d.vect.chart.gif"] \
        -command "GmTree::add chart" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add thematic charts layer"]

    set sep2 [Separator $toolbar.sep2 -orient vertical -background aquamarine2 ]
    pack $sep2 -side left -fill y -padx 5 -anchor w

    # Text Layers
    set bbox3 [ButtonBox $toolbar.bbox3 -spacing 0 ]

    # add raster labels
    $bbox3 add -image [image create photo -file "$iconpath/module-d.labels.gif"] \
        -command "GmTree::add labels" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add raster labels layer (using v.labels file)"]

    # add TclTk labels
    $bbox3 add -image [image create photo -file "$iconpath/gui-maplabels.gif"] \
        -command "GmTree::add clabels" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add postscript labels layer (using v.labels file)"]

    # add text
    $bbox3 add -image [image create photo -file "$iconpath/gui-maptext.gif"] \
        -command "GmTree::add ctext" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 2  \
        -highlightbackground $bgcolor \
        -helptext [G_msg "Add posttscript text layer"]

    pack $bbox3 -side left -anchor w

    set sep3 [Separator $toolbar.sep3 -orient vertical -background aquamarine2 ]
    pack $sep3 -side left -fill y -padx 5 -anchor w
    

}

