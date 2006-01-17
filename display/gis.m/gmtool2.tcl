# Lower toolbar for GRASS GIS Manager
# January 2006
# Michael Barton (Arizona State University)
#

namespace eval GmToolBar2 {
    variable toolbar
    variable mon
}


proc GmToolBar2::create { tb } {
    global gmpath
    global bgcolor
    variable toolbar

    set toolbar $tb

    # LAYER MANAGEMENT
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0 -background $bgcolor ]

    # add group
    $bbox1 add -image [image create photo -file "$gmpath/group.gif"] \
        -command "GmTree::add group" -borderwidth 1\
        -highlightthickness 0 -takefocus 0 -relief raised \
        -helptext [G_msg "Add group"]

    $bbox1 add -image [image create photo -file "$gmpath/copy.gif"] \
        -command "GmTree::duplicate" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Duplicate Layer"]    

    $bbox1 add -image [image create photo -file "$gmpath/cut.gif"] -command \
    	"GmTree::delete" -highlightthickness 0 -takefocus 0 -relief raised \
    	-borderwidth 1 -helptext [G_msg "Delete layer"]
       
    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical -background aquamarine2 ]
    pack $sep1 -side left -fill y -padx 5 -anchor w
    

 # LAYER FILES
    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 0 -background $bgcolor ]

     $bbox2 add -image [image create photo -file "$gmpath/new.gif"] \
     	-command "GmTree::new" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Create new workspace file (erase current workspace settings first)"]
    $bbox2 add -image [image create photo -file "$gmpath/open.gif"] \
    	-command "GmTree::OpenFileBox $toolbar"\
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Open existing workspace file"]
    $bbox2 add -image [image create photo -file "$gmpath/save.gif"]  \
    	-command "GmTree::SaveFileBox $toolbar"\
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Save workspace file"]

    pack $bbox2 -side left -anchor w
    

    set sep2 [Separator $toolbar.sep2 -orient vertical -background aquamarine2 ]
    pack $sep2 -side left -fill y -padx 5 -anchor w


    # 3D AND ANIMATION
    set bbox3 [ButtonBox $toolbar.bbox3 -spacing 0 -background $bgcolor ]

    # zoom
    $bbox3 add -image [image create photo -file "$gmpath/nviz.gif"] \
        -command "Gm::nviz" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "NVIZ - n dimensional visualization"]
    
    # zoom.back
    $bbox3 add -image [image create photo -file "$gmpath/fly.gif"] \
        -command "Gm::fly" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1\
        -helptext [G_msg "Fly through path for NVIZ"]

    # pan
    $bbox3 add -image [image create photo -file "$gmpath/xganim.gif"] \
        -command "Gm::xganim" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1\
        -helptext [G_msg "Animate raster map series"]


    pack $bbox3 -side left -anchor w

    set sep3 [Separator $toolbar.sep3 -orient vertical -background aquamarine2 ]
    pack $sep3 -side left -fill y -padx 5 -anchor w


       # DIGITIZE
    set bbox4 [ButtonBox $toolbar.bbox4 -spacing 20 -background $bgcolor ]
    
    #digitize
    $bbox4 add -image [image create photo -file "$gmpath/dig.gif"] \
        -command "GmTree::edit" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "Digitize map (select or create new map first)"]

    pack $bbox4 -side left -anchor w

    

}

