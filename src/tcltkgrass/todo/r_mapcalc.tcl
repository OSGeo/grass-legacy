#!/usr/local/bin/wish
#############################################################################
# Visual Tcl v1.20 Project
#

#################################
# GLOBAL VARIABLES
#
global widget; 
    set widget(FORMULA) {.top17.ent36}
    set widget(rev,.top17.ent30) {FORMULA}
    set widget(rev,.top17.ent36) {FORMULA}
    set widget(rev,.top17.fra22.ent26) {FORMULA}

#################################
# USER DEFINED PROCEDURES
#
proc init {argc argv} {

}

init $argc $argv


proc {main} {argc argv} {

}

proc {Window} {args} {
global vTcl
    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set newname [lindex $args 2]
    set rest [lrange $args 3 end]
    if {$name == "" || $cmd == ""} {return}
    if {$newname == ""} {
        set newname $name
    }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists == "1" && $name != "."} {wm deiconify $name; return}
            if {[info procs vTclWindow(pre)$name] != ""} {
                eval "vTclWindow(pre)$name $newname $rest"
            }
            if {[info procs vTclWindow$name] != ""} {
                eval "vTclWindow$name $newname $rest"
            }
            if {[info procs vTclWindow(post)$name] != ""} {
                eval "vTclWindow(post)$name $newname $rest"
            }
        }
        hide    { if $exists {wm withdraw $newname; return} }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $base passive
    wm geometry $base 1x1+0+0
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt.tcl"
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.top17 {base} {
    if {$base == ""} {
        set base .top17
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel \
        -menu .top17.m42 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 545x357+252+242
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 0 0
    wm deiconify $base
    wm title $base "GRASS Map Calculator"
    label $base.lab41 \
        -borderwidth 1 -relief sunken -text {r.mapcalc - Map algebra} 
    label $base.lab42 \
        -borderwidth 1 -text Formula: 
    button $base.but44 \
        -command exit -padx 9 -pady 3 -text Quit 
    button $base.but34 \
        -command {$widget(FORMULA) delete 0 end} -padx 9 -pady 3 -text Clear 
    entry $base.ent36 \
        -font {Helvetica -14} 
    frame $base.fra48 \
        -background #1adea6 -borderwidth 2 -height 75 -relief groove \
        -width 125 
    button $base.fra48.but53 \
        -command {$widget(FORMULA) insert end " > "} -padx 9 -pady 3 -text > 
    button $base.fra48.but54 \
        -command {$widget(FORMULA) insert end " < "} -padx 9 -pady 3 -text < 
    button $base.fra48.but56 \
        -command {$widget(FORMULA) insert end " >= "} -padx 9 -pady 3 \
        -text >= 
    button $base.fra48.but57 \
        -command {$widget(FORMULA) insert end " <= "} -padx 9 -pady 3 \
        -text <= 
    button $base.fra48.but58 \
        -command {$widget(FORMULA) insert end " == "} -padx 9 -pady 3 \
        -text == 
    button $base.fra48.but59 \
        -command {$widget(FORMULA) insert end " != "} -padx 9 -pady 3 \
        -text != 
    label $base.fra48.lab62 \
        -borderwidth 1 -text {Comparison Operators} 
    button $base.fra48.but63 \
        -command {$widget(FORMULA) insert end " && "} -padx 9 -pady 3 \
        -text and 
    button $base.fra48.but64 \
        -command {$widget(FORMULA) insert end " || "} -padx 9 -pady 3 \
        -text or 
    button $base.fra48.but76 \
        -command {$widget(FORMULA) insert end " if()"} -padx 9 -pady 3 \
        -text if 
    frame $base.fra65 \
        -background #74dede -borderwidth 2 -height 75 -relief groove \
        -width 125 
    button $base.fra65.but66 \
        -command {$widget(FORMULA) insert end " sin()"} -padx 9 -pady 3 \
        -text sin 
    button $base.fra65.but67 \
        -command {$widget(FORMULA) insert end " cos()"} -padx 9 -pady 3 \
        -text cos 
    button $base.fra65.but68 \
        -command {$widget(FORMULA) insert end " tan()"} -padx 9 -pady 3 \
        -text tan 
    button $base.fra65.but69 \
        -command {$widget(FORMULA) insert end " atan()"} -padx 9 -pady 3 \
        -text atan 
    button $base.fra65.but74 \
        -command {$widget(FORMULA) insert end " exp()"} -padx 9 -pady 3 \
        -text exp 
    button $base.fra65.but78 \
        -command {$widget(FORMULA) insert end " log()"} -padx 9 -pady 3 \
        -text log 
    button $base.fra65.but82 \
        -command {$widget(FORMULA) insert end " sqrt()"} -padx 9 -pady 3 \
        -text sqrt 
    label $base.fra65.lab83 \
        -borderwidth 1 -text {Trigonometric Functions} 
    frame $base.fra70 \
        -background #7892de -borderwidth 2 -height 75 -relief groove \
        -width 125 
    button $base.fra70.but71 \
        -command {$widget(FORMULA) insert end " round()"} -padx 9 -pady 3 \
        -text round 
    button $base.fra70.but72 \
        -command {$widget(FORMULA) insert end " abs()"} -padx 9 -pady 3 \
        -text abs 
    button $base.fra70.but73 \
        -command {$widget(FORMULA) insert end " eval()"} -padx 9 -pady 3 \
        -text eval 
    button $base.fra70.but75 \
        -command {$widget(FORMULA) insert end " float()"} -padx 9 -pady 3 \
        -text float 
    button $base.fra70.but77 \
        -command {$widget(FORMULA) insert end " int()"} -padx 9 -pady 3 \
        -text int 
    button $base.fra70.but79 \
        -command {$widget(FORMULA) insert end " min()"} -padx 9 -pady 3 \
        -text min 
    button $base.fra70.but80 \
        -command {$widget(FORMULA) insert end " max()"} -padx 9 -pady 3 \
        -text max 
    button $base.fra70.but81 \
        -command {$widget(FORMULA) insert end " median()"} -padx 9 -pady 3 \
        -text median 
    frame $base.fra18 \
        -borderwidth 2 -height 75 -relief groove -width 125 
    button $base.fra18.but19 \
        -command {$widget(FORMULA) insert end "1"} -padx 9 -pady 3 -text 1 
    button $base.fra18.but20 \
        -command {$widget(FORMULA) insert end "2"} -padx 9 -pady 3 -text 2 
    button $base.fra18.but21 \
        -command {$widget(FORMULA) insert end "3"} -padx 9 -pady 3 -text 3 
    button $base.fra18.but22 \
        -command {$widget(FORMULA) insert end "4"} -padx 9 -pady 3 -text 4 
    button $base.fra18.but23 \
        -command {$widget(FORMULA) insert end "5"} -padx 9 -pady 3 -text 5 
    button $base.fra18.but24 \
        -command {$widget(FORMULA) insert end "6"} -padx 9 -pady 3 -text 6 
    button $base.fra18.but25 \
        -command {$widget(FORMULA) insert end "7"} -padx 9 -pady 3 -text 7 
    button $base.fra18.but26 \
        -command {$widget(FORMULA) insert end "8"} -padx 9 -pady 3 -text 8 
    button $base.fra18.but27 \
        -command {$widget(FORMULA) insert end "9"} -padx 9 -pady 3 -text 9 
    button $base.fra18.but28 \
        -command {$widget(FORMULA) insert end "0"} -padx 9 -pady 3 -text 0 
    button $base.fra18.but29 \
        -command {$widget(FORMULA) insert end "."} -padx 9 -pady 3 -text . 
    frame $base.fra30 \
        -background #dedcaa -borderwidth 2 -height 75 -relief groove \
        -width 125 
    button $base.fra30.but31 \
        -command {$widget(FORMULA) insert end " ( "} -padx 9 -pady 3 -text ( 
    button $base.fra30.but32 \
        -command {$widget(FORMULA) insert end " ) "} -padx 9 -pady 3 -text ) 
    button $base.fra30.but33 \
        -command {$widget(FORMULA) insert end " * "} -padx 9 -pady 3 -text * 
    button $base.fra30.but34 \
        -command {$widget(FORMULA) insert end " / "} -padx 9 -pady 3 \
        -text div 
    button $base.fra30.but35 \
        -command {$widget(FORMULA) insert end " mod "} -padx 9 -pady 3 \
        -text mod 
    button $base.fra30.but36 \
        -command {$widget(FORMULA) insert end " + "} -padx 9 -pady 3 -text + 
    button $base.fra30.but37 \
        -command {$widget(FORMULA) insert end " - "} -padx 9 -pady 3 -text - 
    button $base.fra30.but38 \
        -command {$widget(FORMULA) insert end " = "} -padx 9 -pady 3 -text = 
    button $base.but39 \
        -padx 9 -pady 3 -text {syntax help} 
    button $base.but41 \
        -padx 9 -pady 3 -text backspace 
    menu $base.m42 \
        -cursor {} 
    menubutton $base.men17 \
        -menu .top17.men17.m -padx 4 -pady 3 -relief raised \
        -text {raster map list} 
    menu $base.men17.m \
        -cursor {} 
    $base.men17.m add command \
        \
        -label {Here the raster map list should be directly (no extra window) integrated with scrollbar} 
    menubutton $base.men18 \
        -menu .top17.men18.m -padx 4 -pady 3 -relief raised -text about 
    menu $base.men18.m \
        -cursor {} 
    $base.men18.m add command \
        -label {GRASS r.mapcalc calculator for TclTKGRASS} 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.lab41 \
        -x 120 -y 5 -width 321 -height 18 -anchor nw -bordermode ignore 
    place $base.lab42 \
        -x 10 -y 305 -width 61 -height 18 -anchor nw -bordermode ignore 
    place $base.but44 \
        -x 465 -y 70 -width 75 -height 26 -anchor nw -bordermode ignore 
    place $base.but34 \
        -x 380 -y 235 -width 75 -anchor nw -bordermode ignore 
    place $base.ent36 \
        -x 15 -y 325 -width 528 -height 27 -anchor nw -bordermode ignore 
    place $base.fra48 \
        -x 305 -y 35 -width 150 -height 130 -anchor nw -bordermode ignore 
    place $base.fra48.but53 \
        -x 55 -y 65 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.but54 \
        -x 15 -y 65 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.but56 \
        -x 55 -y 35 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.but57 \
        -x 15 -y 35 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.but58 \
        -x 95 -y 35 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.but59 \
        -x 95 -y 65 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.lab62 \
        -x 5 -y 5 -width 136 -height 18 -anchor nw -bordermode ignore 
    place $base.fra48.but63 \
        -x 15 -y 95 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.but64 \
        -x 55 -y 95 -width 35 -anchor nw -bordermode ignore 
    place $base.fra48.but76 \
        -x 95 -y 95 -width 35 -anchor nw -bordermode ignore 
    place $base.fra65 \
        -x 135 -y 35 -width 160 -height 130 -anchor nw -bordermode ignore 
    place $base.fra65.but66 \
        -x 10 -y 35 -width 42 -anchor nw -bordermode ignore 
    place $base.fra65.but67 \
        -x 60 -y 35 -width 42 -anchor nw -bordermode ignore 
    place $base.fra65.but68 \
        -x 110 -y 35 -width 42 -anchor nw -bordermode ignore 
    place $base.fra65.but69 \
        -x 60 -y 65 -width 42 -anchor nw -bordermode ignore 
    place $base.fra65.but74 \
        -x 110 -y 95 -width 42 -anchor nw -bordermode ignore 
    place $base.fra65.but78 \
        -x 60 -y 95 -width 42 -anchor nw -bordermode ignore 
    place $base.fra65.but82 \
        -x 10 -y 95 -width 42 -anchor nw -bordermode ignore 
    place $base.fra65.lab83 \
        -x 5 -y 5 -width 151 -height 18 -anchor nw -bordermode ignore 
    place $base.fra70 \
        -x 15 -y 175 -width 445 -height 45 -anchor nw -bordermode ignore 
    place $base.fra70.but71 \
        -x 60 -y 10 -width 47 -anchor nw -bordermode ignore 
    place $base.fra70.but72 \
        -x 5 -y 10 -width 50 -anchor nw -bordermode ignore 
    place $base.fra70.but73 \
        -x 385 -y 10 -width 54 -anchor nw -bordermode ignore 
    place $base.fra70.but75 \
        -x 110 -y 10 -width 46 -anchor nw -bordermode ignore 
    place $base.fra70.but77 \
        -x 160 -y 10 -width 49 -anchor nw -bordermode ignore 
    place $base.fra70.but79 \
        -x 215 -y 10 -width 50 -anchor nw -bordermode ignore 
    place $base.fra70.but80 \
        -x 270 -y 10 -anchor nw -bordermode ignore 
    place $base.fra70.but81 \
        -x 325 -y 10 -width 56 -anchor nw -bordermode ignore 
    place $base.fra18 \
        -x 10 -y 35 -width 115 -height 130 -anchor nw -bordermode ignore 
    place $base.fra18.but19 \
        -x 5 -y 5 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but20 \
        -x 40 -y 5 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but21 \
        -x 75 -y 5 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but22 \
        -x 5 -y 35 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but23 \
        -x 40 -y 35 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but24 \
        -x 75 -y 35 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but25 \
        -x 5 -y 65 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but26 \
        -x 40 -y 65 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but27 \
        -x 75 -y 65 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but28 \
        -x 5 -y 95 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra18.but29 \
        -x 40 -y 95 -width 34 -height 28 -anchor nw -bordermode ignore 
    place $base.fra30 \
        -x 155 -y 230 -width 220 -height 70 -anchor nw -bordermode ignore 
    place $base.fra30.but31 \
        -x 110 -y 5 -width 49 -anchor nw -bordermode ignore 
    place $base.fra30.but32 \
        -x 165 -y 5 -width 44 -anchor nw -bordermode ignore 
    place $base.fra30.but33 \
        -x 10 -y 35 -width 49 -anchor nw -bordermode ignore 
    place $base.fra30.but34 \
        -x 60 -y 35 -width 49 -anchor nw -bordermode ignore 
    place $base.fra30.but35 \
        -x 110 -y 35 -width 52 -anchor nw -bordermode ignore 
    place $base.fra30.but36 \
        -x 10 -y 5 -width 49 -anchor nw -bordermode ignore 
    place $base.fra30.but37 \
        -x 60 -y 5 -width 49 -anchor nw -bordermode ignore 
    place $base.fra30.but38 \
        -x 165 -y 35 -width 43 -anchor nw -bordermode ignore 
    place $base.but39 \
        -x 460 -y 265 -width 80 -anchor nw -bordermode ignore 
    place $base.but41 \
        -x 380 -y 265 -width 75 -anchor nw -bordermode ignore 
    place $base.men17 \
        -x 15 -y 230 -width 134 -height 69 -anchor nw -bordermode ignore 
    place $base.men18 \
        -x 465 -y 35 -width 75 -height 26 -anchor nw -bordermode ignore 
}

Window show .
Window show .top17

main $argc $argv
