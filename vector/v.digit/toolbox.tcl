lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1 

set vdpath $env(GISBASE)/etc/v.digit/ 

set prompt "Welcome to v.digit"
set prompt_left "Left"
set prompt_middle "Middle"
set prompt_right "Right"

# tool window
#proc tool_win {} {
#    toplevel .twin
#    Button .twin.exit -text "Tool Window"  -command "xxx"  
#    pack .twin.exit -side left -anchor w
#    tkwait visibility .twin
#}

# button frame
set bbox [ButtonBox .bbox -spacing 0 -padx 1 -pady 1] 
pack $bbox -side top -anchor w 

$bbox add -image [image create photo -file "$vdpath/new.line.gif"] \
        -command "c_next_tool new_line" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 -padx 1 -pady 1 \
        -helptext "Digitize new line"

$bbox add -image [image create photo -file "$vdpath/rm.line.gif"] \
        -command "c_next_tool delete_line" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 -padx 1 -pady 1 \
        -helptext "Delete line"

$bbox add -image [image create photo -file "$vdpath/stop.gif"] \
        -command "c_cancel" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 -padx 1 -pady 1 \
        -helptext "Quit running tool"

$bbox add -image [image create photo -file "$vdpath/exit.gif"] \
        -command "c_next_tool exit" \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 -padx 1 -pady 1 \
        -helptext "Exit"

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

set destroyed 0 
bind . <Destroy> { c_next_tool exit; set destroyed 1 }

# Strart tool centre in C and wait until the end 
c_tool_centre

# Exit
if { $destroyed == 0 } {
  destroy .
}
