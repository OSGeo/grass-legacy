
namespace eval DmToolBar {
    variable toolbar
}

proc DmToolBar::create { tb  } {
    global dmpath
    variable toolbar

    set toolbar $tb

    # DISPLAY/QUERY
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 2 ]

    # display
    $bbox1 add -image [image create photo -file "$dmpath/display.gif"] \
        -command "Dm::display" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Display selected layers (current region)"]

    # display all
    $bbox1 add -image [image create photo -file "$dmpath/display.all.gif"] \
        -command "Dm::displayall" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Display selected layers (default region)"]

    # display region
    $bbox1 add -image [image create photo -file "$dmpath/display.region.gif"] \
        -command "Dm::display_region" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Display from saved region settings"]

    # erase
    $bbox1 add -image [image create photo -file "$dmpath/erase.gif"] \
        -command "Dm::erase" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Erase to white"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical]
    pack $sep1 -side left -fill y -padx 5 -anchor w

    # ZOOM/QUERY
    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 8 ]

    # zoom
    $bbox2 add -image [image create photo -file "$dmpath/zoom.gif"] \
        -command "Dm::zoom" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 \
        -helptext [G_msg "Zoom"]
    
    # zoom.back
    $bbox2 add -image [image create photo -file "$dmpath/zoom.back.gif"] \
        -command "Dm::zoom_back" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1\
        -helptext [G_msg "Return to previous zoom"]

    # pan
    $bbox2 add -image [image create photo -file "$dmpath/pan.gif"] \
        -command "Dm::pan" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1\
        -helptext [G_msg "Pan and recenter"]

    # query
    $bbox2 add -image [image create photo -file "$dmpath/query.gif"] \
        -command "Dm::query" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 \
        -helptext [G_msg "Query map (select map first)"]

    pack $bbox2 -side left -anchor w

    set sep2 [Separator $toolbar.sep2 -orient vertical]
    pack $sep2 -side left -fill y -padx 5 -anchor w


    # LAYER MANIPULATION
    set bbox3 [ButtonBox $toolbar.bbox3 -spacing 8 ]
    
    # add group
    $bbox3 add -image [image create photo -file "$dmpath/add.group.gif"] \
        -command "Dm::add group" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 \
        -helptext [G_msg "Add group"]

    $bbox3 add -image [image create photo -file "$dmpath/add.raster.gif"] \
        -command "Dm::add raster" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Add raster"]

    $bbox3 add -image [image create photo -file "$dmpath/add.vector.gif"] \
        -command "Dm::add vector" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Add vector"]

    $bbox3 add -image [image create photo -file "$dmpath/add.labels.gif"] \
        -command "Dm::add labels" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Add paint labels (from directory paint/labels)"]

    $bbox3 add -image [image create photo -file "$dmpath/add.cmd.gif"] \
        -command "Dm::add cmd" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Create new command"]

    pack $bbox3 -side left -anchor w

    set sep3 [Separator $toolbar.sep3 -orient vertical]
    pack $sep3 -side left -fill y -padx 5 -anchor w

    # LAYER DUPLICATE/DELETE
    set bbox4 [ButtonBox $toolbar.bbox4 -spacing 8 ]

    $bbox4 add -image [image create photo -file "$dmpath/copy.gif"] \
        -command "Dm::duplicate" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Duplicate Layer"]    

    $bbox4 add -image [Bitmap::get cut] -command "Dm::delete" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Delete layer"]
       
    # $bbox2 add -image [Bitmap::get copy] \
    #    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
    #    -helptext "Copy selection"
    # $bbox2 add -image [Bitmap::get paste] \
    #    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
    #    -helptext "Paste selection"

    pack $bbox4 -side left -anchor w

    set sep4 [Separator $toolbar.sep4 -orient vertical]
    pack $sep4 -side left -fill y -padx 5 -anchor w
    
    # DIGITIZE
    set bbox5 [ButtonBox $toolbar.bbox5 -spacing 20]
    
    #digitize
    $bbox5 add -image [image create photo -file "$dmpath/dig.gif"] \
        -command "Dm::edit" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1  \
        -helptext [G_msg "Digitize map (select or create new map first)"]

    pack $bbox5 -side left -anchor w



}

