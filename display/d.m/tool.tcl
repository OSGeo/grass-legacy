
namespace eval DmToolBar {
    variable toolbar
}

proc DmToolBar::create { tb  } {
    global dmpath
    variable toolbar

    set toolbar $tb

    # DISPLAY/QUERY
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 -padx 1 -pady 1]
    # display
    $bbox1 add -image [image create photo -file "$dmpath/display.gif"] \
        -command "Dm::display" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Display selected layers (current region)"]

    # display all
    $bbox1 add -image [image create photo -file "$dmpath/display.all.gif"] \
        -command "Dm::displayall" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Display selected layers (default region)"]

    # zoom
    $bbox1 add -image [image create photo -file "$dmpath/zoom.gif"] \
        -command "Dm::zoom" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Zoom"]

    # display region
    $bbox1 add -image [image create photo -file "$dmpath/display.region.gif"] \
        -command "Dm::display_region" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Display from saved region settings"]

    # query
    $bbox1 add -image [image create photo -file "$dmpath/query.gif"] \
        -command "Dm::query" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Query map (select map first)"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical]
    pack $sep1 -side left -fill y -padx 4 -anchor w


    # LAYER MANIPULATION
    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 0 -padx 1 -pady 1]
    # add group
    $bbox2 add -image [image create photo -file "$dmpath/add.group.gif"] \
        -command "Dm::add group" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Add group"]

    $bbox2 add -image [image create photo -file "$dmpath/add.raster.gif"] \
        -command "Dm::add raster" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Add raster"]

    $bbox2 add -image [image create photo -file "$dmpath/add.vector.gif"] \
        -command "Dm::add vector" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Add vector"]

    $bbox2 add -image [image create photo -file "$dmpath/add.labels.gif"] \
        -command "Dm::add labels" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Add paint labels (from directory paint/labels)"]

    $bbox2 add -image [image create photo -file "$dmpath/add.cmd.gif"] \
        -command "Dm::add cmd" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Create new command"]

    $bbox2 add -image [image create photo -file "$dmpath/dig.gif"] \
        -command "Dm::edit" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Digitize vector map (select or create new vector first)"]

    $bbox2 add -image [Bitmap::get cut] -command "Dm::delete" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Cut selection"]
    # $bbox2 add -image [Bitmap::get copy] \
    #    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
    #    -helptext "Copy selection"
    # $bbox2 add -image [Bitmap::get paste] \
    #    -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
    #    -helptext "Paste selection"

    pack $bbox2 -side left -anchor w

    set sep2 [Separator $toolbar.sep2 -orient vertical]
    pack $sep2 -side left -fill y -padx 4 -anchor w

    # FILE
    set bbox3 [ButtonBox $toolbar.bbox3 -spacing 0 -padx 1 -pady 1]

     $bbox3 add -image [Bitmap::get new] -command "Dm::new" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Create a new d.m settings file (erase current settings first)"]
    $bbox3 add -image [Bitmap::get open] -command "Dm::OpenFileBox $toolbar"\
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Open an existing file"]
    $bbox3 add -image [Bitmap::get save]  -command "Dm::SaveFileBox $toolbar"\
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Save d.m settings file"]

    $bbox3 add -image [Bitmap::get print]  -command "Dm::print" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Print map"]

    pack $bbox3 -side left -anchor w
}

