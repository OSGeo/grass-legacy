
lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1
source $env(GISBASE)/etc/gtcltk/select.tcl

set env(GISDBASE) [exec g.gisenv get=GISDBASE]
set env(LOCATION_NAME) [exec g.gisenv get=LOCATION_NAME]
set env(MAPSET) [exec g.gisenv get=MAPSET]

set dlg 0
set path {}
set imagepath $env(GISBASE)/bwidget/images/

set iconpath $env(GISBASE)/etc/gui/icons/

################################################################################
# Miscellanious

proc icon {class member} {
	global iconpath
	if {$class == "module"} {
		set memberparts [split $member "."]
		while {$memberparts != {}} {
			set member [join $memberparts "."]
			if {! [catch { set icon [image create photo -file "$iconpath/$class-$member.gif"]}]} {
				return $icon
			}
			set memberparts [lrange $memberparts 0 end-1]
		}
		return 0
	}
	if {[catch { set icon [image create photo -file "$iconpath/$class-$member.gif"]}]} {
		return 0
	} else {
		return $icon
	}
}

# Frame scrolling that works:
proc handle_scroll {window ammount} {
	if {[winfo exists $window] && [winfo ismapped $window]} {
		$window yview scroll [expr {-$ammount/120}] units
	}
}

################################################################################

proc mkcmd {dlg} {
	global opt
	set pgm_name $opt($dlg,pgm_name)
	set nopt $opt($dlg,nopt)

	set cmd [list $pgm_name]
	for {set i 1} {$i <= $nopt} {incr i} {
		switch -- $opt($dlg,$i,class) {
		multi {
			set nmulti $opt($dlg,$i,nmulti)
			set opts {}
			for {set j 1} {$j <= $nmulti} {incr j} {
				if {$opt($dlg,$i,val,$j) == 1} {
					lappend opts $opt($dlg,$i,valname,$j)
				}
			}
			if {$opts != {}} {
				lappend cmd "$opt($dlg,$i,name)=[join $opts ,]"
			}
		}
		opt {
			# Tempting, but buggy: && [string compare $opt($dlg,$i,val) $opt($dlg,$i,answer) ] != 0
			if {[string length $opt($dlg,$i,val)] > 0} {
				lappend cmd "$opt($dlg,$i,name)=$opt($dlg,$i,val)"
			}
		}
		flag {
			if {$opt($dlg,$i,val) == 1} {
				lappend cmd "-$opt($dlg,$i,name)"
			}
		}
		}
	}

	return $cmd
}

proc mkcmd_string {dlg} {
	set cmd [mkcmd $dlg]
	set cmd_string {}
	foreach word $cmd {
		if {[llength $word] > 1} {
			regsub -all -- {'} $word {'\''} newword
			append cmd_string {'} $newword {' }
		} {
			append cmd_string $word { }
		}
	}
	return $cmd_string
}

# Display the current command text in the label
proc show_cmd {dlg} {
	global opt
	set opt($dlg,cmd_string) [mkcmd_string $dlg]
}

proc prnout {dlg fh} {
	global opt imagepath
	set outtext $opt($dlg,outtext)

	if [eof $fh] {
		close $fh
		$outtext image create end -image [image create photo -file "$imagepath/info.gif"] 
		$outtext insert end " Done.\n"
	} else {
		set str [gets $fh]
		append str "\n"
		if { [fblocked $fh] } { set str [read $fh] }
		while {[set idx [string first "\b" $str]] != -1} {
			set last [expr $idx - 1]
			set str1 [string range $str 1 $last]
			set first [expr $idx + 1]
			set str [string range $str $first end]
			set pos [$outtext index "end - 1 chars"]
			$outtext delete $pos
			$outtext insert end $str1
		}
		if { [regexp -- {^GRASS_INFO_([^(]+)\(([0-9]+),([0-9]+)\): (.+)$} $str match key message_pid message_id val rest] } {
			if { $key == "MESSAGE" } {
				$outtext image create end -image [image create photo -file "$imagepath/info.gif"] 
			} elseif { $key == "WARNING" } {
				$outtext image create end -image [image create photo -file "$imagepath/warning.gif"] 
			} elseif { $key == "ERROR" } {
				$outtext image create end -image [image create photo -file "$imagepath/error.gif"] 
			}
			$outtext insert end $val
		} elseif { [regexp -- {^GRASS_INFO_PERCENT: (.+)$} $str match val rest] } {
			progress $dlg $val
			if { $val >= 100 } { 
				progress $dlg -1
				$outtext insert end "\n"
			}
		} elseif { [regexp -- {^GRASS_INFO_END.+} $str match key rest] } {
			# nothing
		} else {
			$outtext insert end $str
                }
		$outtext yview end
	}
}

proc get_file {dlg optn} {
	global opt
	set filename [tk_getOpenFile -title {Load File}]
	if {$filename != ""} {
		if {$opt($dlg,$optn,multi) && $opt($dlg,$optn,val) != ""} {
			append opt($dlg,$optn,val) "," $filename
		} {
			set opt($dlg,$optn,val) $filename
		}
	}
	show_cmd $dlg
}

proc get_map {dlg optn elem} {
	global opt
	set val [GSelect_::create $elem]
	if {$val != ""} {
		if {$opt($dlg,$optn,multi) && $opt($dlg,$optn,val) != ""} {
			append opt($dlg,$optn,val) "," $val
		} {
			set opt($dlg,$optn,val) $val
		}
	}
	show_cmd $dlg
}

proc run_cmd {dlg} {
	global opt env
	
	set path $opt($dlg,path)
	$path.nb raise out

	set outtext $opt($dlg,outtext)
        progress $dlg -1

	set cmd [mkcmd $dlg]
	set cmd_string {}
	foreach word $cmd {
		if {[llength $word] > 1} {
			regsub -all -- {'} $word {'\''} newword
			append cmd_string {'} $newword {' }
		} {
			append cmd_string $word { }
		}
	}
	$outtext insert end "\n"
	# Put a start icon in the output text
	$outtext image create end -image [icon gui cmd]
	$outtext insert end " $cmd_string\n"
	$outtext yview end
	set cmd [concat | $cmd 2>@ stdout]

        set env(GRASS_MESSAGE_FORMAT) gui
	set ret [catch {open $cmd r} fh]
        set env(GRASS_MESSAGE_FORMAT) standard
	if { $ret } {
		error $fh
	} {
		fconfigure $fh -blocking 0
		fileevent $fh readable [list prnout $dlg $fh]
	}
	update idletasks
}

proc help_cmd {dlg} {
	global opt env
	set pgm_name $opt($dlg,pgm_name)

	exec $env(GRASS_HTML_BROWSER) $env(GISBASE)/docs/html/$pgm_name.html &
}

proc clear_cmd {dlg} {
	global opt
	set outtext $opt($dlg,outtext)
        progress $dlg -1

	$outtext delete 1.0 end
}

proc close_cmd {dlg} {
	global opt
	set root $opt($dlg,root)

	destroy $root
}

proc progress {dlg percent} {
	global opt
	
	set opt($dlg,percent) $percent
	
	# it seems that there is a bug in ProgressBar and it is not always updated ->
	$opt($dlg,progress) _modify
}

################################################################################
# Section based layout

# Make a frame for part of the layout tree
proc make_frame {dlg guisection} {
	global opt

	if {$guisection == {}} {set guisection {{}}}
	if {[llength $guisection] == 1} {
		# A frame for a toplevel section
		# This uses a scrolled frame in a notebook tab
		set label [lindex $guisection 0]
		# Ungrouped options go under Options
		if {$label == {}} {
			set label "Options"
		}
		if {! [info exists opt($dlg,first_tab)]} {
			set opt($dlg,first_tab) $label
		}
		set path $opt($dlg,path)
		# Make a tab
		set optpane [$path.nb insert end-1 $label -text $label]
		# And the frames and scrollers:
		set optwin [ScrolledWindow $optpane.optwin -relief sunken -borderwidth 2]
		set optfra [ScrollableFrame $optwin.fra -height 200]
		$optwin setwidget $optfra
		pack $optwin -fill both -expand yes
		pack $optpane -fill both -expand yes

		# Bindings for scrolling the frame
		bind all <MouseWheel> "+handle_scroll $optfra %D"
		bind all <Button-4> "+handle_scroll $optfra 120"
		bind all <Button-5> "+handle_scroll $optfra -120"

		set suf [$optfra getframe]
		# Binding magic to make the whole program start at an appropriate size
		bind $suf <Configure> {+[winfo parent %W] configure -width [winfo reqwidth %W]}
		$path.nb raise $label

		return $suf
	} else {
		# Make a frame for things in this guisection
		# We could add labels, but I fear it would just make a clutter
		set parent_section [lrange $guisection 0 end-1]
		set parent_frame [get_frame $dlg $parent_section]
		set id [llength [winfo children $parent_frame]]
		set suf [frame $parent_frame.fra$id]
		pack $suf -side top -fill x
		return $suf
	}
}

# Get the frame for a guisection, or make it if it doesn't exist yet
proc get_frame {dlg guisection} {
	global opt
	if {! [info exists opt($dlg,section_frame,$guisection)] } {
		set frame [make_frame $dlg $guisection]
		set opt($dlg,section_frame,$guisection) $frame
	}
	return $opt($dlg,section_frame,$guisection)
}

################################################################################
# Make widgets

proc make_dialog {dlg path root} {
	global opt

	# Module information at the top
	if {$opt($dlg,label) != {}} {
		set l1 $opt($dlg,label)
		set l2 $opt($dlg,desc)
	} else {
		set l1 $opt($dlg,desc)
		set l2 {}
	}
	frame $path.module
	set icon [icon module $opt($dlg,pgm_name)]
	if {$icon != 0} {
		button $path.module.icon -relief flat -image $icon -anchor n
		pack $path.module.icon -side left		
	}
	frame $path.module.r
	label $path.module.r.labdesc1 -text $l1 -anchor w -justify left
	label $path.module.r.labdesc2 -text $l2 -anchor w -justify left
	pack $path.module.r.labdesc1 $path.module.r.labdesc2 -side top -fill x
	pack $path.module.r -side left -fill x
	pack $path.module -side top -fill x

	# Make the tabs (notebook)
	set pw [NoteBook $path.nb -side top]
	# Make the output text tab and widgets
	set outpane [$pw insert 1 out -text "Output"]
	pack $outpane -fill both -expand yes
	pack $pw -fill both -expand yes

	set outwin [ScrolledWindow $outpane.win -relief sunken -borderwidth 2]
	set outtext [text $outwin.text -height 5 -width 60] 
	$outwin setwidget $outtext
	pack $outwin -expand yes -fill both

	$pw raise out

	# Widget for displaying current command
	frame $path.cmd
	set cmdlabel [label $path.cmd.label -textvariable opt($dlg,cmd_string) -anchor w -justify left]
	bind $cmdlabel <Configure> "$cmdlabel configure -wraplength \[winfo width $cmdlabel\]"
	button $path.cmd.copy -image [icon edit copy] -anchor n -command "show_cmd $dlg\nclipboard clear -displayof $cmdlabel\nclipboard append -displayof $cmdlabel \$opt($dlg,cmd_string)"
	pack $path.cmd.copy -side left
	pack $cmdlabel -fill x -side top
	pack $path.cmd -expand no -fill x

	# Bindings for updating command
	bind $root <Button> "+show_cmd $dlg"
	bind $root <Key> "+show_cmd $dlg"
	bind $root <ButtonRelease> "+show_cmd $dlg"
	bind $root <KeyRelease> "+show_cmd $dlg"
	
	# Progress bar
	set opt($dlg,percent) -1
        set progress [ProgressBar $path.progress -fg green -height 20 -relief raised -maximum 100 -variable opt($dlg,percent) ]
	pack $progress -expand no -fill x


	set opt($dlg,path) $path
	set opt($dlg,root) $root
	set opt($dlg,outtext) $outtext
	set opt($dlg,progress) $progress
}

proc add_buttons {dlg} {
	global opt env
	set path $opt($dlg,path)
	set pgm_name $opt($dlg,pgm_name)

	button $path.run   -text Run   -command "run_cmd $dlg"
	button $path.help  -text Help  -command "help_cmd $dlg"
	button $path.clear -text Clear -command "clear_cmd $dlg"
	button $path.close -text Close -command "close_cmd $dlg"

	# Turn off help button if the help file doesn't exist
	if {! [file exists $env(GISBASE)/docs/html/$pgm_name.html]} {
		$path.help configure -state disabled
	}

	pack $path.run $path.help $path.clear $path.close \
		-side left -expand yes -padx 20 -pady 5
}

proc do_button_file {dlg optn suf} {
	global opt

	button $suf.val$optn.sel -text {>} -command [list get_file $dlg $optn] -image [icon file open]
	pack $suf.val$optn.sel -side left -fill x
}

proc do_button_old {dlg optn suf elem} {
	global opt
	
	button $suf.val$optn.sel -text {>} -command [list get_map $dlg $optn $elem]
	set icon [icon element $elem]
	if {$icon != 0} {
		$suf.val$optn.sel configure -image $icon
	}
	pack $suf.val$optn.sel -side left -fill x
}

proc do_entry {dlg optn suf} {
	global opt

	Entry $suf.val$optn.val -textvariable opt($dlg,$optn,val)
	pack $suf.val$optn.val -side left -fill x -expand yes
}

proc do_label {dlg optn suf} {
	global opt

	set label $opt($dlg,$optn,label_text)
	set type $opt($dlg,$optn,type)
	set req $opt($dlg,$optn,required)

	set req [expr {$req ? "required" : "optional"}]
	set frame [frame $suf.lab$optn]
	label $frame.label -text "$label:" -anchor w -justify left
	label $frame.req -text "($type; $req)" -anchor e -justify right
	pack $frame.label -side left
	pack $frame.req -side right
	pack $frame -side top -fill x
	DynamicHelp::register $frame balloon $opt($dlg,$optn,help_text)
}

proc do_check {dlg optn suf i s} {
	global opt

	checkbutton $suf.val$optn.val$i -text $s -variable opt($dlg,$optn,val,$i) -onvalue 1 -offvalue 0
	pack $suf.val$optn.val$i -side left
	set opt($dlg,$optn,valname,$i) $s
}

proc do_combo {dlg optn suf vals} {
	global opt

	ComboBox $suf.val$optn.val -underline 0 -labelwidth 0 -width 25 -textvariable opt($dlg,$optn,val) -values $vals -helptext $opt($dlg,$optn,help_text)
	pack $suf.val$optn.val -side left
}

################################################################################
# Input clean-up and normalization

# Make guisections match up with different spacing near delimiters:
proc normalize_guisection {dlg optn} {
	global opt
	#TODO: Trim each part
	set trimmed {}
	foreach untrimmed [split $opt($dlg,$optn,guisection) ";"] {
		lappend trimmed [string trim $untrimmed]
	}
	set opt($dlg,$optn,guisection) $trimmed
}

# Pick the text to use for visible labels and balloon help.
proc choose_help_text {dlg optn} {
	global opt

	# Set label text and help text
	# Use description for label if label is absent
	set opt($dlg,$optn,label_text) $opt($dlg,$optn,label)
	set opt($dlg,$optn,help_text) $opt($dlg,$optn,desc)

	if {$opt($dlg,$optn,label_text) == {}} {
		set opt($dlg,$optn,label_text) $opt($dlg,$optn,help_text)
		set opt($dlg,$optn,help_text) {}
	}
}

################################################################################

proc begin_dialog {pgm optlist} {
	global opt dlg path
	incr dlg
	
	array set opts $optlist

	foreach key {label desc} {
		set opt($dlg,$key) $opts($key)
	}

	set root [expr {$path == "" ? "." : $path}]

	set opt($dlg,pgm_name) $pgm
	wm title $root $pgm
	make_dialog $dlg $path $root
	
	# Use the default font instead for balloon help:
	if {[lsearch [font names] balloon-help] == -1} {
		font create balloon-help -family Helvetica -size -12 -weight bold
	}
	DynamicHelp::configure -font balloon-help -fg black -bg "#FFFF77"
}

proc end_dialog {n} {
	global opt dlg

	set opt($dlg,nopt) $n

	add_buttons $dlg

	set path $opt($dlg,path)
	$path.nb raise out
	$path.nb raise $opt($dlg,first_tab)

	update
	show_cmd $dlg
}

proc add_option {optn optlist} {
	global opt dlg

	array set opts $optlist

	set opts(class) [expr {$opts(multi) && $opts(options) != {} ? "multi" : "opt"}]

	foreach key {class name type multi desc required options answer prompt label guisection} {
		set opt($dlg,$optn,$key) $opts($key)
	}

	choose_help_text $dlg $optn

	normalize_guisection $dlg $optn

	set suf [get_frame $dlg $opt($dlg,$optn,guisection)]

	do_label $dlg $optn $suf
	frame $suf.val$optn

	if {$opts(options) != {}} {
		set vals [split $opts(options) ,]
		set answers [split $opts(answer) ,]
		set opt($dlg,$optn,nmulti) [llength $vals]
		if {$opts(multi)} {
			set i 1
			foreach x $vals {
				do_check $dlg $optn $suf $i $x
				if { [lsearch $answers $x] >= 0 } {
					set opt($dlg,$optn,val,$i) 1
				}
				incr i
			}
		} else {
			do_combo $dlg $optn $suf $vals
			set opt($dlg,$optn,val) $opts(answer)
		}
	} else {
		set prompt $opts(prompt)
		if {$prompt != {}} {
			if {[string match file* $prompt]} {
				do_button_file $dlg $optn $suf
			}
			if {[string match old* $prompt]} {
				set p [split $prompt ,]
				do_button_old $dlg $optn $suf [lindex $p 1]
			}
		}
		do_entry $dlg $optn $suf
		if {$opts(answer) != {}} {
			set opt($dlg,$optn,val) $opts(answer)
		}
	}

	pack $suf.val$optn -side top -fill x
	DynamicHelp::register $suf.val$optn balloon $opt($dlg,$optn,help_text)
}

proc add_flag {optn optlist} {
	global opt dlg
	
	array set opts $optlist

	set opt($dlg,$optn,class) flag

	foreach key {name desc label guisection} {
		set opt($dlg,$optn,$key) $opts($key)
	}

	choose_help_text $dlg $optn

	normalize_guisection $dlg $optn

	set suf [get_frame $dlg $opt($dlg,$optn,guisection)]

	frame $suf.val$optn
	checkbutton $suf.val$optn.chk -text $opt($dlg,$optn,label_text) -variable opt($dlg,$optn,val) -onvalue 1 -offvalue 0 -anchor w
	pack $suf.val$optn.chk -side left
	pack $suf.val$optn -side top -fill x
	DynamicHelp::register $suf.val$optn balloon $opt($dlg,$optn,help_text)
}

################################################################################
