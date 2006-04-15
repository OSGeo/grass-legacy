############################################################################
#
# LIBRARY:      runandoutput.tcl
# AUTHOR(S):    Cedric Shock (cedricgrass AT shockfamily.net)
# PURPOSE:      Interactive console for gis.m and other run commands
# COPYRIGHT:    (C) 2006 GRASS Development Team
#
#               This program is free software under the GNU General Public
#               License (>=v2). Read the file COPYING that comes with GRASS
#               for details.
#
############################################################################

#############################################
#Overloaded gui.tcl behaviour:

# Overload run_cmd (proc called when run button is pushed)
proc run_cmd {dlg} {
	global gronsole
	global opt

	set cmd [dialog_get_command $dlg]

	catch {$opt($dlg,run_button) configure -state disabled}

	$gronsole run $cmd {} "catch {$opt($dlg,run_button) configure -state active}"
	
	# Bring that output window up:
	raise [winfo toplevel $gronsole]
}

# no output or progress or buttons:
proc make_output {dlg path root} {}
proc make_progress {dlg path root} {}
proc make_buttons {dlg path root} {}

###########################################

proc make_fun_buttons {dlg path} {
	global opt env
	set pgm_name $opt($dlg,pgm_name)

	set buttonframe [frame $path.buttonframe]
	button $buttonframe.run   -text Run   -command "run_cmd $dlg"
	# In the future we'll have a button to make a layer from here:
	# button $buttonframe.layer -text Layer -command "layer_cmd $dlg"
	button $buttonframe.help  -text Help  -command "help_cmd $dlg"
	button $buttonframe.close -text Close -command "close_cmd $dlg"

	set opt($dlg,run_button) $buttonframe.run 

	# Turn off help button if the help file doesn't exist
	if {! [file exists $env(GISBASE)/docs/html/$pgm_name.html]} {
		$buttonframe.help configure -state disabled
	}

	pack $buttonframe.run $buttonframe.help $buttonframe.close \
		-side left -expand yes -padx 5 -pady 5
	pack $buttonframe -expand no -fill x -side bottom -before [lindex [pack slaves $path] 0]
}

proc run_ui {cmd} {
    global dlg path

    set program [lindex $cmd 0]

    set code [exec -- $program --tcltk]

    set path .dialog$dlg
    toplevel $path
    eval $code

    # Push the command to the dialog
    set thisdialog $dlg
    dialog_set_command $dlg $cmd

    # Add our ui
    make_fun_buttons $dlg $path
}

#################################################

proc run_disabled {gronsole button cmd} {
	catch {$button configure -state disabled}

	$gronsole run $cmd {running} "catch {$button configure -state active}"
}

proc gronsole_history {cmdtext ci cmd} {
	$cmdtext delete 1.0 end
	$cmdtext insert end $cmd
}

proc command_window {where} {
	global keycontrol

	set cmdpane [frame $where.command]
	set cmdwin [ScrolledWindow $where.win -relief sunken -borderwidth 2]
	set gronsole [Gronsole $where.gronsole -clickcmd "gronsole_history $cmdwin.text"]
	set cmdtext [text $cmdwin.text -height 4 -width 80] 
	$cmdwin setwidget $cmdtext
	set runbutton [button $cmdpane.run -text "Run" -command "run_disabled $gronsole $cmdpane.run \[string trim \[$cmdtext get 1.0 end\]\]"]
	set run2button [button $cmdpane.run2 -text "Run (Background)" -command "$gronsole run \[string trim \[$cmdtext get 1.0 end\]\] {} {}"]
	set runuibutton [button $cmdpane.runui -text "Run UI" -command "run_ui \[string trim \[$cmdtext get 1.0 end\]\]"]
	set runxterm [button $cmdpane.xterm -text "Run in Xterm" -command "$gronsole run_xterm \[string trim \[$cmdtext get 1.0 end\]\] {}"]
	set outpane [frame $where.output]
	set savebutton [button $outpane.save -text "Save" -command "$gronsole save"]
	set clearbutton [button $outpane.clear -text "Clear" -command "$gronsole clear"]

	pack $runbutton $run2button $runuibutton $runxterm \
		-side left -expand yes -padx 5 -pady 5
	pack $savebutton $clearbutton \
		-side left -expand yes -padx 5 -pady 5

	pack $cmdpane -fill x -side bottom
	pack $cmdwin -fill x -side bottom
	pack $outpane -fill x -side bottom
	pack $gronsole -side top -fill both -expand yes

	bind $cmdtext <$keycontrol-c> "tk_textCopy %W"
	bind $cmdtext <$keycontrol-v> "tk_textPaste %W"
	bind $cmdtext <$keycontrol-x> "tk_textCut %W"

	return $gronsole
}

toplevel .gronsole
wm title .gronsole "Output - GIS.m"

# If we had our own window manager we could withdraw windows instead of iconifying them:
wm protocol .gronsole WM_DELETE_WINDOW "wm iconify .gronsole"

set gronsole [command_window {.gronsole}]

###############################################################################
# Run procs for gis.m:

################################################################################

proc execute {cmd} {
    # Use run and output
    run_ui $cmd
}

###############################################################################
proc spawn {cmd args} {
	eval exec -- $cmd $args &
}

###############################################################################

proc run_panel {cmd} {
	global gronsole

	$gronsole run_wait $cmd gism
}

###############################################################################
proc run {cmd args} {
	# This and runcmd are being used to run command in the background
	# These used to go to stdout and stderr
	# but we don't want to pollute that console.
	# eval exec -- $cmd $args >@ stdout 2>@ stderr
	eval exec -- $cmd $args >& /dev/null
}

###############################################################################

proc runcmd {cmd args} {
	global gronsole

	set ci [$gronsole annotate $cmd [list gism running]]

	eval run $cmd $args

	$gronsole remove_tag $ci running	
}

###############################################################################
proc term_panel {cmd} {
	global gronsole

	$gronsole run_xterm $cmd gism
}

###############################################################################
proc term {cmd args} {
	global env
	eval exec -- xterm -name xterm-grass -e $env(GISBASE)/etc/grass-run.sh $cmd $args &
}

###############################################################################
# Annotation procs for gis.m:

proc monitor_annotation_start {mon title tags} {
	global gronsole
	set handle [$gronsole annotate $title $tags]
	$gronsole set_click_command $handle {}
	return $handle
}

proc monitor_annotate {handle text} {
	global gronsole
	$gronsole annotate_text $handle $text
}
