
namespace eval DmMonitorsel {
    variable toolbar
    variable mon
}


proc DmMonitorsel::displmon { mon } {
    global dmpath
    if ![catch {open "|d.mon -L" r} input] {
        while {[gets $input line] >= 0} {
            if {[regexp -nocase "$mon.*not running" $line]} {
                run "d.mon start=$mon"
                return
            } elseif {[regexp -nocase "$mon.* running" $line]} {
                run "d.mon select=$mon"
                return       
            }              
        }
    }
}

proc DmMonitorsel::create { tb  } {
    global dmpath
    variable toolbar

    set toolbar $tb

    # Monitor selection
    set bbox1 [ButtonBox $toolbar.bbox1 -spacing 0]
    
    # monitor x0
    $bbox1 add  -text [G_msg "x0"] -command "DmMonitorsel::displmon x0" \
        -highlightthickness 0 -takefocus 1 -relief link -borderwidth 1  \
        -helptext [G_msg "Start/select display monitor x0"]

    #monitor x1
    $bbox1 add  -text [G_msg "x1"] -command "DmMonitorsel::displmon x1" \
        -highlightthickness 0 -takefocus 1 -relief link -borderwidth 1  \
        -helptext [G_msg "Start/select display monitor x1"]

    #monitor x2
    $bbox1 add  -text [G_msg "x2"] -command "DmMonitorsel::displmon x2" \
        -highlightthickness 0 -takefocus 1 -relief link -borderwidth 1  \
        -helptext [G_msg "Start/select display monitor x2"]

    #monitor x3
    $bbox1 add  -text [G_msg "x3"] -command "DmMonitorsel::displmon x3" \
        -highlightthickness 0 -takefocus 1 -relief link -borderwidth 1  \
        -helptext [G_msg "Start/select display monitor x3"]

    #monitor x4
    $bbox1 add  -text [G_msg "x4"] -command "DmMonitorsel::displmon x4" \
        -highlightthickness 0 -takefocus 1 -relief link -borderwidth 1  \
        -helptext [G_msg "Start/select display monitor x4"]

    #monitor x5
    $bbox1 add  -text [G_msg "x5"] -command "DmMonitorsel::displmon x5" \
        -highlightthickness 0 -takefocus 1 -relief link -borderwidth 1  \
        -helptext [G_msg "Start/select display monitor x5"]

    #monitor x6
    $bbox1 add  -text [G_msg "x6"] -command "DmMonitorsel::displmon x6" \
        -highlightthickness 0 -takefocus 1 -relief link -borderwidth 1  \
        -helptext [G_msg "Start/select display monitor x6"]

    pack $bbox1 -side left -anchor w

    set sep1 [Separator $toolbar.sep1 -orient vertical]
    pack $sep1 -side left -fill y -padx 4 -anchor w

    # FILE
    set bbox2 [ButtonBox $toolbar.bbox2 -spacing 10 -padx 1 -pady 1]

     $bbox2 add -image [Bitmap::get new] -command "Dm::new" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Create new workspace file (erase current workspace settings first)"]
    $bbox2 add -image [Bitmap::get open] -command "Dm::OpenFileBox $toolbar"\
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Open existing workspace file"]
    $bbox2 add -image [Bitmap::get save]  -command "Dm::SaveFileBox $toolbar"\
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Save workspace file"]

    pack $bbox2 -side left -anchor w

    set sep2 [Separator $toolbar.sep2 -orient vertical]
    pack $sep2 -side left -fill y -padx 5 -anchor w

    # PRINT
    set bbox3 [ButtonBox $toolbar.bbox3 -spacing 10 -padx 1 -pady 1]

    $bbox3 add -image [Bitmap::get print]  -command "Dm::print" \
        -highlightthickness 0 -takefocus 0 -relief link -borderwidth 1 -padx 1 -pady 1 \
        -helptext [G_msg "Print map"]

    pack $bbox3 -side left -anchor w
}

