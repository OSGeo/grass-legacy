#########################################################################
# Default Priority for this panel
#
# priority is from 0 to 10
#  the lower the number, the quicker it will be bumped
#  10 cannot be bumped
#  Panels will be loaded by the greater of 5 or their current priority


###########################################################################
# procedure to make main control area
###########################################################################

proc mkmainPanel { BASE } {
    global Nv_
    global XY

    catch {destroy $BASE}

    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} $BASE "Main" 1 10]
    } else {
	set panel $Nv_($BASE)
    }

    frame $BASE  -relief groove -borderwidth 2
    set Nv_(main_BASE) $BASE

    # make redraw button area
    pack [frame $BASE.redrawf]		-side top -fill x -expand 1
    pack [frame $BASE.redrawf.f1] 	-side top -fill x
    pack [frame $BASE.redrawf.f11]       -side top -fill x
    pack [frame $BASE.redrawf.f2] 	-side top -fill x

    set labl1 [label $BASE.redrawf.f1.label1  -text Auto: ]
    set auto [checkbutton $BASE.redrawf.f1.autoclear -text "Clear" \
		  -variable autoc ]
    $auto select
    set auto_d [checkbutton $BASE.redrawf.f1.autodraw -text "Draw" \
                  -onvalue 1 -offvalue 0 -variable auto_draw ]
    $auto_d select
    pack $labl1 $auto $auto_d -side left -expand 1 -fill x

#checkbuttons for features to draw
    set labl2 [label $BASE.redrawf.f11.label1  -text Feature: ]

    set surf_b [checkbutton $BASE.redrawf.f11.surface -text "Surface" \
                  -onvalue 1 -offvalue 0 -variable surface]
    $surf_b select

    set vect_b [checkbutton $BASE.redrawf.f11.vector -text "Vectors" \
                  -onvalue 1 -offvalue 0 -variable vector]
    $vect_b select

    set site_b [checkbutton $BASE.redrawf.f11.sites -text "Sites" \
                  -onvalue 1 -offvalue 0 -variable sites]
    $site_b select

    set vol_b [checkbutton $BASE.redrawf.f11.volume -text "Volumes" \
                  -onvalue 1 -offvalue 0 -variable volume]
    $vol_b select

    pack $labl2 $surf_b $vect_b $site_b $vol_b -side left \
    -expand 1 -fill x


#Execute buttons

   button $BASE.redrawf.f2.exec -text DRAW 
   bind $BASE.redrawf.f2.exec <1> "Nset_cancel 1"
   bind $BASE.redrawf.f2.exec <B1-ButtonRelease> { \
        if {$surface == 1 && $vector == 1 && $sites == 1 && $volume == 1} {
        {Ndraw_all}
        } else {
        if {$surface == 1} {
        {Nsurf_draw_all}
        }
        if {$vector == 1} {
        {Nvect_draw_all}
        }
        if {$sites == 1} {
        {Nsite_draw_all}
        }
        if {$volume == 1} {
        {Nvol_draw_all}
        }
        }
   }

   button $BASE.redrawf.f2.clear -text Clear -command {do_clear}

   button $BASE.redrawf.f2.cancel -text Cancel -command {Nset_cancel 1}

   pack $BASE.redrawf.f2.exec  $BASE.redrawf.f2.clear $BASE.redrawf.f2.cancel \
    -side left -expand 1 -fill x


#pack frames
    pack [frame $BASE.midt ] -side top -expand 1 -fill x
    pack [frame $BASE.midf ] -side left -expand 1

    set draw_lab [label $BASE.midt.lab -text "View:" \
                 -relief flat]
    set draw_var1 [radiobutton $BASE.midt.b1 -text "eye" \
                 -variable draw_option -value 0 \
		-command "change_display 1" ]

    set draw_var2 [radiobutton $BASE.midt.b2 -text "center" \
                 -variable draw_option -value 1 \
                 -command "change_display 0" ]
	$draw_var1 select


    pack $draw_lab $draw_var1 $draw_var2 -side left -expand 0

    # make  position "widget"
    set XY [Nv_mkXYScale $BASE.midf.pos puck XY_POS 125 125 105 105 update_eye_position]
    set H [mk_hgt_slider $BASE.midf]
    set E [mk_exag_slider $BASE.midf]
    pack $XY $H $E -side left -expand y

    # make lookat buttons
    frame $BASE.midf.lookat

    label $BASE.midf.lookat.l -text LOOK

#button $BASE.midf.lookat.here -text here \
#-command {bind .top.canvas <Button> {if [%W islinked] {look_here %W %x %y}}}

    button $BASE.midf.lookat.here -text here \
	-command {bind .top.canvas <Button> {look_here %W %x %y
	if {[Nauto_draw] == 1} {Ndraw_all}
	}}

    button $BASE.midf.lookat.center -text center -command { look_center
	if {[Nauto_draw] == 1} {Ndraw_all} }
    button $BASE.midf.lookat.top -text top -command {
# Nv_itemDrag $Nv_(main_BASE).midf.pos $Nv_(XY_POS) 62.5 62.5
# note: below value is somewhat strange, but with 0.5 0.5 the map rotates:
#	update_eye_position 0.496802 0.50100
	set val2 [$Nv_(main_BASE).midf.height.f.entry get]
	Nset_focus_top $val2
	change_display 1
	update

        if {[Nauto_draw] == 1} {Ndraw_all}
}
    button $BASE.midf.lookat.cancel -text cancel -command no_focus
    pack $BASE.midf.lookat.l $BASE.midf.lookat.here \
	$BASE.midf.lookat.center $BASE.midf.lookat.top $BASE.midf.lookat.cancel \
	-side top -fill x -expand 1

    pack $BASE.midf.lookat -side left -expand 1
    pack $BASE.midf -side top -fill x -expand 1

    frame $BASE.bframe
    frame $BASE.bframe.cframe
    set P [Nv_mkScale $BASE.bframe.cframe.pers h perspective 120 3 40 Nchange_persp 0]
    set T [Nv_mkScale $BASE.bframe.cframe.tw h twist 180 -180 0 Nchange_twist 0]
    button $BASE.bframe.reset -text RESET -command "do_reset $XY $H $E $P"
    pack $BASE.bframe.cframe.pers -side top
    pack $BASE.bframe.cframe.tw -side top
    pack $BASE.bframe.reset  -side right -expand 1
    pack $BASE.bframe -side top -fill x -expand 1
    pack $BASE.bframe.cframe -side top -fill x -expand 1

# According to the documentation, the Main panel can never be closed
#	button $BASE.close -text Close -command "Nv_closePanel $BASE" -anchor s
#	pack $BASE.close -side right

    return $panel
}

# Procedure to reset the main panel
proc Nviz_main_reset {} {
    global Nv_

    # Simple, just invoke the reset button
    $Nv_(main_BASE).bframe.reset invoke
}

# Procedure to save camera parameters
proc Nviz_main_save { file_hook } {
    global Nv_

    set BASE $Nv_(main_BASE)

    # Figure out window geometry
    set temp [wm geometry .]
    regexp {[0-9]*x[0-9]*} $temp t_matched
    set t_list [split $t_matched {x}]

    # Need to make this accurate
    # Also need to save "look here" information
    # TODO prob. need focus indication AND realto (if focused)
    puts $file_hook "$t_list"
    puts $file_hook "[$BASE.bframe.cframe.pers.f.entry get]"
    puts $file_hook "[$BASE.midf.zexag.f.entry get]"
    puts $file_hook "[$BASE.midf.height.f.entry get]"
    puts $file_hook "[Nv_getXYPos  XY_POS]"
    puts $file_hook "[Nhas_focus]"
    puts $file_hook "[Nget_focus]"
    # if not focused, should use view_to
}

# Procedure to load camera parameters
proc Nviz_main_load { file_hook } {
    global Nv_

    # window size
    gets $file_hook data
    set win_width [lindex $data 0]
    set win_height [lindex $data 1]
    set geom_command "$win_width"
    append geom_command "x" "$win_height"
    wm geometry . "$geom_command"

    # perspective
    gets $file_hook data
    Nv_setEntry $Nv_(main_BASE).bframe.cframe.pers.f.entry [expr int($data)]
    Nv_scaleCallback $Nv_(main_BASE).bframe.cframe.pers e 0 null [expr int($data)]
    update

    # zexag
    gets $file_hook data
    Nv_setEntry $Nv_(main_BASE).midf.zexag.f.entry $data
    Nv_floatscaleCallback $Nv_(main_BASE).midf.zexag e 2 null $data
    update

    # height
    gets $file_hook data
    Nv_setEntry $Nv_(main_BASE).midf.height.f.entry $data
    Nv_floatscaleCallback $Nv_(main_BASE).midf.height e 2 null $data
    update

    # XY position
    gets $file_hook data
    set data [split "$data"]
    Nv_itemDrag $Nv_(main_BASE).midf.pos $Nv_(XY_POS) \
	[expr int([lindex $data 0] * 125)]  [expr int([lindex $data 1] * 125)]
    update_eye_position [lindex $data 0] [lindex $data 1]
    update

    # focus
    gets $file_hook data
    set data [split "$data"]
    if {"[lindex $data 0]" == "1"} then {
	gets $file_hook data
	set data [split "$data"]
	Nset_focus [lindex $data 0] [lindex $data 1] [lindex $data 2]
    } else {
	# insert code to set view_to here
	Nset_no_focus
    }
    update

}

proc do_clear {} {

# TEST    Nset_draw both
    Nset_draw front

    Nready_draw
    Nclear
    Ndone_draw
    Nset_draw back
}

# TODO - if started with view file, use these params for reset

proc do_reset {XY H E P} {
    global Nv_

    appBusy

    Nset_focus_map
    Nv_itemDrag $XY $Nv_(XY_POS) 105 105
    Nv_xyCallback Nchange_position 125 125 105 105

    set exag [Nget_first_exag]
    set val $exag
    Nv_floatscaleCallback $E b 2 Nchange_exag $val

    set list [Nget_height]
    set val [lindex $list 0]
    Nv_floatscaleCallback $H b 2 Nchange_height $val

    Nv_scaleCallback $P b 0 Nchange_persp 40

    appNotBusy
}


proc mk_exag_slider {W} {

    set exag [Nget_first_exag]
    set val $exag
    set exag [expr $val * 10]
    set min 0
#	if {$val < 1} then {
#		set min $val
#	} else {
#		set min 0
#	}

    Nv_mkFloatScale $W.zexag v zexag $exag $min $val update_exag 2

    return $W.zexag
}
proc mk_hgt_slider {W} {
    global Nv_

    set list [Nget_height]
    set val [lindex $list 0]
    set min [lindex $list 1]
    set max [lindex $list 2]

    # make sliders
    set Nv_(HEIGHT_SLIDER) $W.height
    Nv_mkFloatScale $W.height v height $max $min $val update_height 2

    return $W.height
}

proc update_exag {exag} {
    global Nv_

    if {$exag == 0.} {
	set exag [lindex [$Nv_(main_BASE).midf.zexag.scale configure -resolution] 4]
	Nv_setEntry $Nv_(main_BASE).midf.zexag.f.entry $exag
	Nv_floatscaleCallback $Nv_(main_BASE).midf.zexag e 2 null $exag
    }
    Nchange_exag $exag
    Nv_floatscaleCallback $Nv_(HEIGHT_SLIDER) b 2 update_height \
	[$Nv_(HEIGHT_SLIDER).f.entry get]
#    Nv_floatscaleCallback $Nv_(HEIGHT_SLIDER) b 2 update_height [lindex [Nget_height] 0]
#    Nquick_draw
}

proc update_eye_position {x y} {
    global Nv_

    Nchange_position $x $y

    if {$Nv_(FollowView)} {
	set_lgt_position $x $y
	set x [expr int($x*125)]
	set y [expr int($y*125)]
	Nv_itemDrag $Nv_(LIGHT_XY) $Nv_(LIGHT_POS) $x $y
    }
}

proc update_center_position {x y} {
    global Nv_

     Nset_focus_gui $x $y

    if {$Nv_(FollowView)} {
        set_lgt_position $x $y
        set x [expr int($x*125)]
        set y [expr int($y*125)]
        Nv_itemDrag $Nv_(LIGHT_XY) $Nv_(LIGHT_POS) $x $y
    }
}


proc change_display {flag} {
global XY Nv_

       set NAME $XY
       set NAME2 [winfo parent $NAME]
       catch "destroy $XY"

if {$flag == 1} {
#draw eye position
set XY [Nv_mkXYScale $NAME puck XY_POS 125 125 105 105 update_eye_position]

set E [lindex [Nget_position] 0]
if {$E < 0.} {set $E 0.}
if {$E > 1.} {set $E 1.}

set N [lindex [Nget_position] 1]
set N [expr 1. - $N]
if {$N < 0.} {set $N 0.}
if {$N > 1.} {set $N 1.}

set E [expr $E * 125.]
set N [expr $N * 125.]

Nv_itemDrag $Nv_(main_BASE).midf.pos $Nv_(XY_POS) $E $N
update

} else {
#draw center position
set XY [Nv_mkXYScale $NAME cross XY_POS 125 125 109 109 update_center_position]
pack $XY -side left -before $NAME2.height

set E [lindex [Nget_focus_gui] 0]
if {$E > 1.} { set E 1.}
if {$E < 0.} {set E 0.}

set N [lindex [Nget_focus_gui] 1]
if {$N > 1.} {set N 1.}
if {$N < 0.} {set N 0.}

set E [expr ($E * 125.)]
#reverse northing for canvas
set N [expr 125 - ($N * 125.)]

Nv_itemDrag $Nv_(main_BASE).midf.pos $Nv_(XY_POS) $E $N
update

}

       pack $XY -side left -before $NAME2.height

}

proc update_height {h} {
    global Nv_

    Nchange_height $h

# I don't think this is correct -
# Nget_height does the exag guess  BB

    if {$Nv_(FollowView)} {
	set list [Nget_height]
	set val [lindex $list 0]
	set min [lindex $list 1]
	set max [lindex $list 2]
	set h [expr int((100.0*($h -$min))/($max - $min))]
	Nv_floatscaleCallback $Nv_(LIGHT_HGT) b 2 set_lgt_hgt $h
    }
}


