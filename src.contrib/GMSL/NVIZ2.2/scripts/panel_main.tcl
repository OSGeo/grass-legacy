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
    pack [frame $BASE.redrawf.f2] 	-side top -fill x 

    set labl1 [label $BASE.redrawf.f1.label1  -text Auto: ]
    set auto [checkbutton $BASE.redrawf.f1.autoclear -text "Clear" \
		  -variable autoc ] 
    $auto select
    set auto_d [checkbutton $BASE.redrawf.f1.autodraw -text "Draw" \
                  -onvalue 1 -offvalue 0 -variable auto_draw ]
    $auto_d select
    set labl2 [label $BASE.redrawf.f1.label2  -text REDRAW] 
    set clr [button $BASE.redrawf.f1.clear -text Clear -command do_clear]
    pack $labl1 $auto $auto_d $labl2 $clr -side left -expand 1 -fill x

    pack \
	[button $BASE.redrawf.f2.surface -text Surface -command Nsurf_draw_all] \
	-side left -expand 1 -fill x
    pack \
	[button $BASE.redrawf.f2.vectors -text Vectors -command Nvect_draw_all] \
	-side left -expand 1 -fill x
    pack \
	[button $BASE.redrawf.f2.sites  -text Sites -command Nsite_draw_all] \
	-side left -expand 1 -fill x
    pack \
	[button $BASE.redrawf.f2.cancel -text Cancel -command {Nset_cancel 1} ] \
	-side left -expand 1 -fill x


    pack [frame $BASE.midf ] -side left -expand 1

    # make  position "widget"
    set XY [Nv_mkXYScale $BASE.midf.pos puck XY_POS 125 125 20 20 update_position]
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
    button $BASE.midf.lookat.cancel -text cancel -command no_focus
    pack $BASE.midf.lookat.l $BASE.midf.lookat.here \
	$BASE.midf.lookat.center $BASE.midf.lookat.cancel \
	-side top -fill x -expand 1

    pack $BASE.midf.lookat -side left -expand 1
    pack $BASE.midf -side top -fill x -expand 1
    
    frame $BASE.bframe
    set P [Nv_mkScale $BASE.bframe.pers h perspective 120 3 40 Nchange_persp 0]
    button $BASE.bframe.reset -text RESET -command "do_reset $XY $H $E $P"
    pack $BASE.bframe.pers -side left
    pack $BASE.bframe.reset  -side right -expand 1
    pack $BASE.bframe -side top -fill x -expand 1

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
    puts $file_hook "[$BASE.bframe.pers.f.entry get]"
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
    Nv_setEntry $Nv_(main_BASE).bframe.pers.f.entry [expr int($data)]
    Nv_scaleCallback $Nv_(main_BASE).bframe.pers e 0 null [expr int($data)]
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
    update_position [lindex $data 0] [lindex $data 1]
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
    Nv_itemDrag $XY $Nv_(XY_POS) 20 20
    Nv_xyCallback Nchange_position 125 125 20 20
    
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
    
    Nchange_exag $exag
    Nv_floatscaleCallback $Nv_(HEIGHT_SLIDER) b 2 update_height \
	[$Nv_(HEIGHT_SLIDER).f.entry get]
#    Nv_floatscaleCallback $Nv_(HEIGHT_SLIDER) b 2 update_height [lindex [Nget_height] 0]
#    Nquick_draw
}

proc update_position {x y} {
    global Nv_
    Nchange_position $x $y 
    if {$Nv_(FollowView)} {
	set_lgt_position $x $y
	set x [expr int($x*125)]
	set y [expr int($y*125)]
	Nv_itemDrag $Nv_(LIGHT_XY) $Nv_(LIGHT_POS) $x $y
    }
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


