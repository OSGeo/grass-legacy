
lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1
source $env(GISBASE)/etc/gtcltk/select.tcl

set env(GISDBASE) [exec g.gisenv get=GISDBASE]
set env(LOCATION_NAME) [exec g.gisenv get=LOCATION_NAME]
set env(MAPSET) [exec g.gisenv get=MAPSET]

set env(GRASS_MESSAGE_FORMAT) gui

set dlg 0
set path {}
set imagepath $env(GISBASE)/bwidget/images/

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

proc prnout {dlg fh} {
	global opt imagepath
	set outtext $opt($dlg,outtext)

	if [eof $fh] {
		close $fh
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
		if { [regexp -- {^GRASS_INFO_([^ ]+): (.+)$} $str match key val rest] } {
			if { $key == "PERCENT" } { 
                                progress $dlg $val
				if { $val >= 100 } { 
                                	progress $dlg -1
		    			$outtext insert end "\n"
				}
			} else { 
			    if { $key == "MESSAGE" } {
				$outtext image create end -image [image create photo -file "$imagepath/info.gif"] 
			    } 
			    if { $key == "WARNING" } {
				$outtext image create end -image [image create photo -file "$imagepath/warning.gif"] 
			    } 
			    if { $key == "ERROR" } {
				$outtext image create end -image [image create photo -file "$imagepath/error.gif"] 
			    }
			    $outtext insert end $val
			}
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
}

proc run_cmd {dlg} {
	global opt
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
	$outtext insert end "\n$cmd_string\n"
	$outtext yview end
	set cmd [concat | $cmd 2>@ stdout]
	if {[catch {open $cmd r} fh]} {
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

proc make_dialog {dlg path root} {
	global opt

	set pw [PanedWindow $path.pw -side right]
	set optpane [$pw add -minsize 50]
	set outpane [$pw add -minsize 30]

	set optwin [ScrolledWindow $optpane.optwin -relief sunken -borderwidth 2]
	set optfra [ScrollableFrame $optwin.fra -height 200]
	$optwin setwidget $optfra
	set suf [$optfra getframe]
	pack $optwin -fill both -expand yes
	pack $optpane $outpane -fill both -expand yes
	pack $pw -fill both -expand yes

	set outwin [ScrolledWindow $outpane.win -relief sunken -borderwidth 2]
	set outtext [text $outwin.text -height 5 -width 30] 
	$outwin setwidget $outtext
	pack $outwin $outtext -expand yes -fill both
    
        set opt($dlg,percent) -1
        set progress [ProgressBar $path.progress -fg green -height 20 -relief raised -maximum 100 -variable opt($dlg,percent) ]
	pack $progress -expand no -fill x

	set opt($dlg,path) $path
	set opt($dlg,root) $root
	set opt($dlg,suf) $suf
	set opt($dlg,outtext) $outtext
	set opt($dlg,progress) $progress
}

proc module_description {dlg desc} {
	global opt
	set suf $opt($dlg,suf)

	label $suf.labdesc1 -text $desc -anchor w -justify left -background white -foreground black
	label $suf.labdesc2 -text {} -anchor w -justify left
	pack $suf.labdesc1 $suf.labdesc2 -side top -fill x
}

proc add_buttons {dlg} {
	global opt
	set path $opt($dlg,path)

	button $path.run   -text Run   -command "run_cmd $dlg"
	button $path.help  -text Help  -command "help_cmd $dlg"
	button $path.clear -text Clear -command "clear_cmd $dlg"
	button $path.close -text Close -command "close_cmd $dlg"

	pack $path.run $path.help $path.clear $path.close \
		-side left -expand yes -padx 20 -pady 5
}

proc do_button_file {dlg optn} {
	global opt
	set suf $opt($dlg,suf)

	button $suf.val$optn.sel -text {>} -command [list get_file $dlg $optn]
	pack $suf.val$optn.sel -side left -fill x
}

proc do_button_old {dlg optn elem} {
	global opt
	set suf $opt($dlg,suf)

	button $suf.val$optn.sel -text {>} -command [list get_map $dlg $optn $elem]
	pack $suf.val$optn.sel -side left -fill x
}

proc do_entry {dlg optn} {
	global opt
	set suf $opt($dlg,suf)

	Entry $suf.val$optn.val -textvariable opt($dlg,$optn,val)
	pack $suf.val$optn.val -side left -fill x -expand yes
}

proc do_label {dlg optn desc type reqd} {
	global opt
	set suf $opt($dlg,suf)

	set req [expr {$reqd ? "required" : "optional"}]
	label $suf.lab$optn -text "$desc ($type, $req):" -anchor w -justify left
	pack $suf.lab$optn -side top -fill x
}

proc do_check {dlg optn i s} {
	global opt
	set suf $opt($dlg,suf)

	checkbutton $suf.val$optn.val$i -text $s -variable opt($dlg,$optn,val,$i) -onvalue 1 -offvalue 0
	pack $suf.val$optn.val$i -side left
	set opt($dlg,$optn,valname,$i) $s
}

proc do_combo {dlg optn vals} {
	global opt
	set suf $opt($dlg,suf)

	ComboBox $suf.val$optn.val -underline 0 -labelwidth 0 -width 25 -textvariable opt($dlg,$optn,val) -values $vals
	pack $suf.val$optn.val -side left
}

################################################################################

proc begin_dialog {pgm desc} {
	global opt dlg path
	incr dlg

	set root [expr {$path == "" ? "." : $path}]

	set opt($dlg,pgm_name) $pgm
	wm title $root $pgm
	make_dialog $dlg $path $root
	module_description $dlg $desc
}

proc end_dialog {n} {
	global opt dlg

	set opt($dlg,nopt) $n
	add_buttons $dlg
}

proc add_option {optn optlist} {
	global opt dlg
	set suf $opt($dlg,suf)

	array set opts $optlist

	set opts(class) [expr {$opts(multi) && $opts(options) != {} ? "multi" : "opt"}]

	foreach key {class name type multi desc required options answer prompt} {
		set opt($dlg,$optn,$key) $opts($key)
	}

	do_label $dlg $optn $opts(desc) $opts(type) $opts(required)
	frame $suf.val$optn

	if {$opts(options) != {}} {
		set vals [split $opts(options) ,]
		set opt($dlg,$optn,nmulti) [llength $vals]
		if {$opts(multi)} {
			set i 1
			foreach x $vals {
				do_check $dlg $optn $i $x
				incr i
			}
		} else {
			do_combo $dlg $optn $vals
		}
	} else {
		set prompt $opts(prompt)
		if {$prompt != {}} {
			if {[string match file* $prompt]} {
				do_button_file $dlg $optn
			}
			if {[string match old* $prompt]} {
				set p [split $prompt ,]
				do_button_old $dlg $optn [lindex $p 1]
			}
		}
		do_entry $dlg $optn
		if {$opts(answer) != {}} {
			set opt($dlg,$optn,val) $opts(answer)
		}
	}

	pack $suf.val$optn -side top -fill x
}

proc add_flag {optn key desc} {
	global opt dlg
	set suf $opt($dlg,suf)

	set opt($dlg,$optn,class) flag
	frame $suf.val$optn
	checkbutton $suf.val$optn.chk -text $desc -variable opt($dlg,$optn,val) -onvalue 1 -offvalue 0 -anchor w
	pack $suf.val$optn.chk -side left
	set opt($dlg,$optn,name) $key
	pack $suf.val$optn -side top -fill x
}

################################################################################

