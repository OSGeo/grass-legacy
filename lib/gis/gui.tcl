
lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1
wm title . $pgm_name
source $env(GISBASE)/etc/gtcltk/select.tcl
set env(GISDBASE) [exec g.gisenv get=GISDBASE]
set env(LOCATION_NAME) [exec g.gisenv get=LOCATION_NAME]
set env(MAPSET) [exec g.gisenv get=MAPSET]

set pw [PanedWindow .pw -side right]
set optpane [$pw add -minsize 50]
set outpane [$pw add -minsize 30]

set optwin [ScrolledWindow $optpane.optwin -relief sunken -borderwidth 2]
set optfra [ScrollableFrame $optwin.fra -height 200 ]
$optwin setwidget $optfra
set suf [$optfra getframe]
pack $optwin -fill both -expand yes
pack $optpane $outpane -fill both -expand yes
pack $pw -fill both -expand yes

set outwin [ScrolledWindow $outpane.win -relief sunken -borderwidth 2]
set outtext [text $outwin.text -height 5 -width 30] 
$outwin setwidget $outtext
pack $outwin $outtext -expand yes -fill both

proc module_description {desc} {
	global suf
	label $suf.labdesc1 -text $desc -anchor w -justify left -background white -foreground black
	label $suf.labdesc2 -text {} -anchor w -justify left
	pack $suf.labdesc1 $suf.labdesc2 -side top -fill x
}

proc mkcmd {} {
	global pgm_name nopt
	global optname optval opttype nmulti optvalname
	set cmd [list $pgm_name]
	for {set i 1} {$i <= $nopt } {incr i} {
		switch -- $opttype($i) {
			multi {
				set domulti 0
				for {set j 1} {$j <= $nmulti($i) } {incr j} {
					if { $optval($i,$j) == 1 } {
						set domulti 1
					}
				}
				if { $domulti == 1 } {
					set opt "$optname($i)="
					set first 1
					for {set j 1} {$j <= $nmulti($i) } {incr j} {
						if { $optval($i,$j) == 1 } {
							if { $first == 1 } {
								set first 0
							} else {
								append opt ","
							}
							append opt "$optvalname($i,$j)"
						}
					}
					lappend cmd $opt
				}
			}
			opt {
				if {[string length $optval($i)] > 0} {
					lappend cmd "$optname($i)=$optval($i)"
				}
			}
			flag {
				if { $optval($i) == 1 } {
					lappend cmd "-$optname($i)"
				}
			}
		}
	}
	return $cmd
}

proc prnout {fh} {
	global outtext
	if [eof $fh] {
		close $fh
	} else {
		set str [ read $fh ]
		while { [set idx [ string first {\b} $str ]] != -1 } {
			set last [expr $idx - 1]
			set str1 [ string range $str 1 $last]
			set first [expr $idx + 1]
			set str [ string range $str $first end]
			set pos [$outtext index "end - 1 chars"]
			$outtext delete $pos
			$outtext insert end $str1
		}
		$outtext insert end $str
		$outtext yview end
	}
}

proc add_buttons {} {
	global outtext

	button .run -text "Run" -command {
		global outtext pipe
		set cmd [ mkcmd ]
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
		if {[catch {open $cmd r} msg]} {
			error $msg
		} {
			fconfigure $msg -blocking 0
			fileevent $msg readable [ list prnout $msg ]
		}
		update idletasks
	}

	button .help -text Help -command {exec $env(GRASS_HTML_BROWSER) $env(GISBASE)/docs/html/$pgm_name.html &}
	pack .run .help -side left -expand yes -padx 20 -pady 5

	button .clear -text Clear -command { $outtext delete 1.0 end }
	pack .run .clear -side left -expand yes -padx 20 -pady 5
	
	button .close -text Close -command { exit }
	pack .run .close -side left -expand yes -padx 20 -pady 5
}

proc do_flag {optn key desc} {
	global opttype optname optval suf
	set opttype($optn) flag
	frame $suf.val$optn
	checkbutton $suf.val$optn.chk -text $desc -variable optval($optn) -onvalue 1 -offvalue 0 -anchor w
	pack $suf.val$optn.chk -side left
	set optname($optn) $key
	pack $suf.val$optn -side top -fill x
}

proc do_button_file {optn} {
	global optval optmulti suf
	button $suf.val$optn.sel -text {>} -command {
		set filename [tk_getOpenFile -title {Load File}]
		if {$filename != ""} {
			if {$optmulti($optn) && $optval($optn) != ""} {
				append optval($optn) "," $filename
			} {
				set optval($optn) $filename
			}
		}
	}
	pack $suf.val$optn.sel -side left -fill x
}

proc get_map {optn elem} {
	global optval optmulti
	set val [GSelect_::create $elem]
	if {$val != ""} {
		if {$optmulti($optn) && $optval($optn) != ""} {
			append optval($optn) "," $val
		} {
			set optval($optn) $val
		}
	}
}

proc do_button_old {optn elem} {
	global optval suf
	button $suf.val$optn.sel -text {>} -command "get_map $optn $elem"
	pack $suf.val$optn.sel -side left -fill x
}

proc do_entry {optn} {
	global optval suf
	Entry $suf.val$optn.val -textvariable optval($optn)
	pack $suf.val$optn.val -side left -fill x -expand yes
}

proc do_label {optn desc type reqd} {
	global suf
	set req [expr {$reqd ? "required" : "optional"}]
	label $suf.lab$optn -text "$desc ($type, $req):" -anchor w -justify left
	pack $suf.lab$optn -side top -fill x
}

proc do_check {optn i s} {
	global suf optval optvalname
	checkbutton $suf.val$optn.val$i -text $s -variable optval($optn,$i) -onvalue 1 -offvalue 0
	pack $suf.val$optn.val$i -side left
	set optvalname($optn,$i) $s
}

proc do_combo {optn vals} {
	global suf optval
	ComboBox $suf.val$optn.val -underline 0 -labelwidth 0 -width 25 -textvariable optval($optn) -values $vals
	pack $suf.val$optn.val -side left
}

