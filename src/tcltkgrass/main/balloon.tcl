#########################################################################
#
# Balloon help, by John Haxby <jch@pwd.hp.com>, with slight changes
# by Axel Boldt <boldt@math.ucsb.edu>,
# by Jacques Bouchard <bouchard@onera.fr> (March 1999).
#
#########################################################################

proc BalloonInit {} {
    bind balloon <Enter> {
        set balloonHelp(%W,after) [after 400 {updateBalloonHelp %W}]
    }   
    bind balloon <Any-ButtonPress> {
        hideBalloonHelp %W
    }
    bind balloon <Any-ButtonRelease> {
        updateBalloonHelp %W
    }   
    bind balloon <Leave> {
        hideBalloonHelp %W
    }
} 

proc updateBalloonHelp {w} {
    global balloonHelp

    if {! [info exists balloonHelp($w)] || [$w cget -state] == "disabled"} {
        hideBalloonHelp $w
    } else {
        update idletasks
        set curpos [winfo pointerxy $w]
        set curwin [eval winfo containing $curpos]
        if {$w == $curwin} {
            if ![winfo exists .balloon] {
                toplevel .balloon
                wm overrideredirect .balloon true
                pack [label .balloon.l \
                        -foreground black \
                        -background LightGoldenrodYellow \
                        -highlightthickness 1 \
                        -highlightbackground black \
                        -justify left]
                wm withdraw .balloon
            }
            .balloon.l configure -text $balloonHelp($w)
            set X [winfo screenwidth  $w]
            set Y [winfo screenheight $w]
            set x [lindex $curpos 0]
            set y [lindex $curpos 1]
#           incr x -15
            incr y +15
            set newGeometry 1
            while {$newGeometry} {
                wm geometry .balloon +$x+$y

                # This update is important to have the geometry command take 
                # effect in all cases (A.B.)
                update idletasks
                raise .balloon
                wm deiconify .balloon

                scan [wm geometry .balloon] {%dx%d%d%d} dx dy x y
                set newGeometry 0
                if {$x + $dx > $X} {
                    set x [expr $X - $dx]
                    set newGeometry 1
                }
                if {$y + $dy > $Y} {
                    set y [expr $Y - $dy]
                    set newGeometry 1
                }
            }
        }
    }
}

proc hideBalloonHelp {w} {
    global balloonHelp

    if [info exists balloonHelp($w,after)] {
        after cancel $balloonHelp($w,after)
        unset balloonHelp($w,after)
    }
    catch {wm withdraw .balloon}
}
