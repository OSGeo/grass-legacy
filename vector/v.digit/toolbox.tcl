lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1 

source $env(GISBASE)/etc/gtcltk/gmsg.tcl
source $env(GISBASE)/etc/gtcltk/select.tcl

set vdpath $env(GISBASE)/etc/v.digit/ 
source $vdpath/settings.tcl
source $vdpath/cats.tcl

set env(GISDBASE) [exec g.gisenv get=GISDBASE]
set env(LOCATION_NAME) [exec g.gisenv get=LOCATION_NAME]
set env(MAPSET) [exec g.gisenv get=MAPSET]

set prompt [G_msg "Welcome to v.digit"]
set prompt_left [G_msg "Left"]
set prompt_middle [G_msg "Middle"]
set prompt_right [G_msg "Right"]
set coor ""

# GVariable stores variables by key, this variables are (should be) synchronized with
# variables in Variable array in C (synchronization should be done somehow better). Key is
# 'name' in VAR structure in C. Variables are initialized by var_init() on startup.
# For key list see VARN_* in global.h

# GWidget stores names of some widgets we need to access globaly, names:
# field - field Entry for new line
# cat - cat Entry for new line

# Create new line options
proc new_line_options { create } {
    global GVariable GWidget
    if { $create } {
	set lineopt [frame .lineopt]
	pack $lineopt -fill x -side top

	set row1 [frame $lineopt.row1]
	pack $row1 -fill x -side top

	Label $row1.flab -padx 2 -pady 2 -relief sunken -anchor w -text [G_msg "Layer"]
	set GWidget(field) [Entry $row1.fval -width 10 -textvariable GVariable(field) \
			    -command { c_var_set field $GVariable(field) } ]
        bind $GWidget(field) <KeyRelease> { c_var_set field $GVariable(field) } 
	Label $row1.clab -padx 2 -pady 2 -relief sunken -anchor w -text [G_msg "Category"]
	set GWidget(cat) [Entry $row1.cval -width 10 -textvariable GVariable(cat) \
			    -command { c_var_set cat $GVariable(cat) }]
        bind $GWidget(cat) <KeyRelease> { c_var_set cat $GVariable(cat) } 
	set GWidget(cat_mode) [ComboBox $row1.cmode -label [G_msg "Mode"] -width 20  -textvariable cmode \
			-modifycmd {
			    set GVariable(cat_mode) [ $GWidget(cat_mode) getvalue]
                            c_var_set cat_mode $GVariable(cat_mode)
			 }]

	pack $row1.flab $GWidget(field) $row1.clab $GWidget(cat) $GWidget(cat_mode) -fill x  -side left

	set row2 [frame $lineopt.row2]
	pack $row2 -fill x -side top
        
        checkbutton $row2.ins -variable GVariable(insert) \
                              -command { c_var_set insert $GVariable(insert) }
	Label $row2.ilab -padx 2 -pady 2 -anchor w -text [G_msg "Insert new record to table"]
    
	pack $row2.ins $row2.ilab -fill x  -side left

    } else {
        destroy .lineopt
    }
}

# button frame
set bbox [ButtonBox .bbox -spacing 1 -padx 1 -pady 1] 
pack $bbox -side top -anchor w 

# --- Draw new ---
$bbox add -image [image create photo -file "$vdpath/new.point.gif"] \
        -command "c_next_tool new_point" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Digitize new point"]

$bbox add -image [image create photo -file "$vdpath/new.line.gif"] \
        -command "c_next_tool new_line" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Digitize new line"]

$bbox add -image [image create photo -file "$vdpath/new.boundary.gif"] \
        -command "c_next_tool new_boundary" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Digitize new boundary"]

$bbox add -image [image create photo -file "$vdpath/new.centroid.gif"] \
        -command "c_next_tool new_centroid" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Digitize new centroid"]

# --- Edit old ---
$bbox add -image [image create photo -file "$vdpath/move.vertex.gif"] \
        -command "c_next_tool move_vertex" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Move vertex"]

$bbox add -image [image create photo -file "$vdpath/add.vertex.gif"] \
        -command "c_next_tool add_vertex" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Add vertex"]

$bbox add -image [image create photo -file "$vdpath/rm.vertex.gif"] \
        -command "c_next_tool rm_vertex" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Remove vertex"]

$bbox add -image [image create photo -file "$vdpath/split.line.gif"] \
        -command "c_next_tool split_line" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Split line"]

$bbox add -image [image create photo -file "$vdpath/move.line.gif"] \
        -command "c_next_tool move_line" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Move point, line, boundary, or centroid"]

$bbox add -image [image create photo -file "$vdpath/delete.line.gif"] \
        -command "c_next_tool delete_line" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Delete point, line, boundary, or centroid"]

# --- Zoom / Display ---
$bbox add -image [image create photo -file "$vdpath/zoom.window.gif"] \
        -command "c_next_tool zoom_window" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Zoom in by window"]

$bbox add -image [image create photo -file "$vdpath/zoom.out.centre.gif"] \
        -command "c_next_tool zoom_out_centre" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Zoom out"]

$bbox add -image [image create photo -file "$vdpath/zoom.pan.gif"] \
        -command "c_next_tool zoom_pan" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Pan"]

$bbox add -image [image create photo -file "$vdpath/zoom.default.gif"] \
        -command "c_next_tool zoom_default" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Zoom to default region"]

proc zoom_region { } {
    set reg [GSelect windows]
    if { $reg != "" } {
        c_var_set zoom_region $reg 
        c_next_tool zoom_region
    }
}

$bbox add -image [image create photo -file "$vdpath/zoom.region.gif"] \
        -command "zoom_region" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Zoom to region"]

$bbox add -image [image create photo -file "$vdpath/redraw.gif"] \
        -command "c_next_tool redraw" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Redraw"]

# --- Attributes ---
$bbox add -image [image create photo -file "$vdpath/display.cats.gif"] \
        -command "c_next_tool display_cats" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Display categories"]

$bbox add -image [image create photo -file "$vdpath/display.attributes.gif"] \
        -command "c_next_tool display_attributes" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Display attributes"]

# --- Stop ---
#$bbox add -image [image create photo -file "$vdpath/stop.gif"] \
#        -command "c_cancel" \
#        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
#        -helptext "Quit running tool"

# --- Others ---
$bbox add -image [image create photo -file "$vdpath/settings.gif"] \
        -command "c_next_tool settings" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Open settings"]

$bbox add -image [image create photo -file "$vdpath/exit.gif"] \
        -command "c_next_tool exit" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 3 \
        -helptext [G_msg "Exit"]

frame .pf
pack .pf -fill x -side top
Label .pf.prompt -width 62 -padx 2 -pady 2 -relief sunken -anchor w  -textvariable prompt
pack .pf.prompt -fill x  -side left

frame .bpf
pack .bpf -fill x -side top
Label .bpf.left -width 20 -padx 2 -pady 2 -relief sunken -anchor w -textvariable prompt_left
Label .bpf.middle -width 20 -padx 2 -pady 2 -relief sunken -anchor w -textvariable prompt_middle
Label .bpf.right -width 20 -padx 2 -pady 2 -relief sunken -anchor w -textvariable prompt_right
pack .bpf.left .bpf.middle .bpf.right -fill x -side left

frame .coorf
pack .coorf -fill x -side top
Label .coorf.prompt -width 62 -padx 2 -pady 2 -relief sunken -anchor w  -textvariable coor
pack .coorf.prompt -fill x  -side left

set destroyed 0 
bind . <Destroy> { if { "%W" == "."} { c_next_tool exit; set destroyed 1 } }

# Strart tool centre in C and wait until the end 
c_tool_centre

# Exit
#puts "Exit destroyed = $destroyed"
if { $destroyed == 0 } {
  destroy .
}
