##########################################################################
# Default Priority for this panel
#
# priority is from 0 to 10
#  the lower the number, the quicker it will be bumped
#  10 cannot be bumped
#  Panels will be loaded by the greater of 5 or their current priority
##########################################################################

proc mksdiffPanel { BASE } {
    global Nv_
    
#    set Nv_($BASE) $BASE
    
    catch {destroy $BASE}

    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} \
		       $BASE "Scaled Difference" 1 5]
    } else {
	set panel $Nv_($BASE)
    }

    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Scaled Difference Panel"

    frame $BASE.top 
    frame $BASE.bottom -bd 2 -relief groove
    frame $BASE.top2 
    
    set maplist [Nget_map_list surf]

    label $BASE.top.label -text "Reference surface:"
    Nv_mksdiffSurfacelist $BASE.top.list 
    pack $BASE.top.label $BASE.top.list -side left -fill y -pady 4
    
    Nv_mkScale $BASE.top2.sdscale h Exag 2500 0 100 set_sdexag 2 
    pack $BASE.top2.sdscale -side left -fill y -pady 4
    

    button $BASE.bottom.none -text None -command "unset_sdsurf"
    pack $BASE.bottom.none -side left
    
    button $BASE.bottom.close -text Close -command "Nv_closePanel $BASE" 
    pack $BASE.bottom.close -side right
    
    pack $BASE.top $BASE.top2 $BASE.bottom \
    -expand 1 -fill both -side top
    
    return $panel
}

proc set_sdsurf {val} {
    global Nv_

    if { $val != 0 } then {
	set L [Nget_map_list surf]
	set n [lsearch -exact $L $val]
    }

    Nset_SDsurf $val
    Nset_current sdiff $val

    # reset panel 
    set cmd mksdiffPanel
    set W $Nv_(P_AREA).sdiff
    set pos [Q_get_pos $Nv_(Q) $Nv_($W)]
    $cmd $W
    Nv_openPanel sdiff $pos
}

proc set_sdexag {val} {Nset_SDscale $val}
proc unset_sdsurf {} {
    global Nv_

    Nunset_SDsurf
    Nset_current sdiff 0

    # reset panel 
    set cmd mksdiffPanel
    set W $Nv_(P_AREA).sdiff
    set pos [Q_get_pos $Nv_(Q) $Nv_($W)]
    $cmd $W
    Nv_openPanel sdiff $pos

}

# TODO: get radio behaviour

proc Nv_mksdiffSurfacelist { P } {
    global Nv_

    catch {destroy $P}
    set list [Nget_map_list surf]
    set name [Nget_current sdiff]

    if {$name == 0} {
	set name "None Selected"
    } else {
	set n [lsearch $list $name]
	set list [lreplace $list $n $n]
	set name [Nget_map_name $name surf]
    }

    menubutton $P -text $name -menu $P.m
    menu $P.m -tearoff 0
    foreach i $list {
	set map_name [Nget_map_name $i surf]
	$P.m add command -label "$map_name" \
	    -command "set_sdsurf $i"
    }

    return $P
}
