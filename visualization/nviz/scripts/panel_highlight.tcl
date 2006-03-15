# $Id$
#***************************************************************
#*
#* MODULE:       panel_highlight.tcl 1.0
#*
#* AUTHOR(S):    ACS - Massimo Cuomo - m.cuomo at acsys.it
#*
#* PURPOSE:		 Creates a "highlight" panel and manages
#*					color/size/marker highligh values and enabing for
#*					"default" highlight request (see site_highlight_commands.c
#*					for details and "README")
#*
#* REQUIREMENTS: ACS_utils.tcl - site_highlight_commands.c
#*
#* COPYRIGHT:    (C) 2005 by the ACS / GRASS Development Team
#*
#*               This program is free software under the
#*               GNU General Public License (>=v2).
#*               Read the file COPYING that comes with GRASS
#*               for details.
#*
#**************************************************************

# Used for function "modal_edit_list_plain"
global ___ACS_utils___
if {![info exists ___ACS_utils___]} {
	source $default_panel_path/ACS_utils.tcl
}



# Procedure called by the automatic NVIZ panel creation mechanism
proc mkhighlightPanel { BASE } {
	global Nv_ highlight

    catch {destroy $BASE}

    #  Initialize panel info
    if [catch {set Nv_($BASE)}] {
	set panel [St_create {window name size priority} \
		       $BASE "Highlight" 1 5]
    } else {
	set panel $Nv_($BASE)
    }


    frame $BASE  -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Highlight"

    frame $BASE.top
    frame $BASE.bottom -bd 2 -relief groove
    frame $BASE.top2

    label $BASE.top.label -text " "
    pack $BASE.top.label -side left -fill y -pady 4


	button $BASE.bottom.close -text Close -command "Nv_closePanel $BASE"
    pack $BASE.bottom.close -side right

    pack $BASE.top $BASE.top2 $BASE.bottom \
    -expand 1 -fill both -side top



	set highlight(COLOR) [Nsite_highlight_get_default color]
	set highlight(SIZE) [Nsite_highlight_get_default size]
	set highlight(MARKER) [Nsite_highlight_get_default marker]

	set highlight(COLOR_VALUE) [Nsite_highlight_get_default_value color]
	set highlight(SIZE_VALUE) [Nsite_highlight_get_default_value size]
	set highlight(MARKER_VALUE) [Nsite_highlight_get_default_value marker]


	
	checkbutton $BASE.top.color -text color -variable highlight(COLOR) -command highlight_color
	checkbutton $BASE.top.size -text size -variable highlight(SIZE) -command highlight_size
	checkbutton $BASE.top.marker -text marker -variable highlight(MARKER) -command highlight_marker

	button $BASE.top.color_value -command "highlight_set_color $BASE.top.color_value" -bg $highlight(COLOR_VALUE) -width 0 -height 0
	button $BASE.top.size_value -command "highlight_set_size $BASE.top.size_value" -text $highlight(SIZE_VALUE) -width 0 -height 0
	highlight_set_marker_button $BASE.top.marker_value

	pack $BASE.top.color $BASE.top.color_value  -side left -expand 1
	pack $BASE.top.size $BASE.top.size_value -side left -expand 1
	pack $BASE.top.marker $BASE.top.marker_value -side left -expand 1

	return $panel
}

proc highlight_set_marker_button {_w} {
	global highlight

	set highlight(MARKERS_NAME) {"x" "box" "sphere" "diamond" "aster" "gyro"}
	set highlight(MARKERS_INDEX) {"1" "2"     "3"      "5"      "8"    "9"}

	set m $_w
	menubutton $m -menu $m.m -relief raised -indicatoron 1 -bd 2
	menu $m.m -tearoff 0

	foreach elt $highlight(MARKERS_NAME) i $highlight(MARKERS_INDEX) {
		set highlight(MARKERS_NAME.$i) $elt
		$m.m add radiobutton -label "$elt" -command "highlight_set_marker $_w" -value $i -variable highlight(MARKER_VALUE)
	}
	$m configure -text $highlight(MARKERS_NAME.$highlight(MARKER_VALUE))
}


###########################################################################################
# Here the action take place
###########################################################################################

proc highlight_set_color {_w} {
	global highlight

	set highlight(COLOR_VALUE) [mkColorPopup .colorPop color $highlight(COLOR_VALUE) 1]
	$_w configure -bg $highlight(COLOR_VALUE)
	Nsite_highlight_set_default_value color $highlight(COLOR_VALUE)
}

proc highlight_set_size {_w} {
	global highlight

	set labels [list "size"]
	set entries [list $highlight(SIZE_VALUE)]
	if {[modal_edit_list_plain "insert size" labels entries]} {
		set highlight(SIZE_VALUE) [lindex $entries 0]
		Nsite_highlight_set_default_value size $highlight(SIZE_VALUE)
		$_w configure -text $highlight(SIZE_VALUE)
	}
}

proc highlight_set_marker {_w} {
	global highlight

	Nsite_highlight_set_default_value marker $highlight(MARKER_VALUE)
	$_w configure -text $highlight(MARKERS_NAME.$highlight(MARKER_VALUE))
}


proc highlight_color {} {
	global highlight
	Nsite_highlight_set_default color $highlight(COLOR)
}
proc highlight_size {} {
	global highlight
	Nsite_highlight_set_default size $highlight(SIZE)
}
proc highlight_marker {} {
	global highlight
	Nsite_highlight_set_default marker $highlight(MARKER)
}
