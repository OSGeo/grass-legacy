###########################

proc shortcut {where what to} {
	bind $where <Any-$what> "$to flash; $to invoke"
	bind $where <$what> "$to flash; $to invoke"
	}

###########################

proc entryOrder {win order} {
	if {[llength $order] <1} {return}
	bind $win <Tab> "focus [lindex $order 0]"
	if {[llength $order] <2} {return}
	set llen [llength $order]
	set first [lindex $order 0]
	set last [lindex $order [expr $llen -1]]
	bind $last <Tab> "focus $first"
	bind $last <Return> "focus $first"
	bind $first <Shift-Tab> "focus $last"
	bind $first <Shift-Return> "focus $last"
	for {set num 1} {$num < $llen} {incr num} {
		set next [lindex $order $num]
		bind $first <Tab> "focus $next"
		bind $first <Return> "focus $next"
		bind $next <Shift-Tab> "focus $first"
		bind $next <Shift-Return> "focus $first"
		set first $next
		}
	}

###########################

proc DisablePath {path save {skip "idontexist"}} {
	set childs [winfo children $path]
	upvar $save s
	if {[lsearch -glob [$path configure] "-state*"] != -1} {
		if {"[lindex [$path configure -state] 4]" != "disabled"} {
			$path configure -state disabled
			set s $path
			}
		}
	foreach i $childs {
		if {$i == $skip} {continue}
		if {$i == ".mn.help"} {continue}
		set lst ""
		DisablePath $i lst
		set s [concat $s $lst]
		}
	}

###########################

proc ReablePath {save} {
	upvar $save s
	foreach i $s {catch {$i configure -state normal}}
	}

###########################

proc DoTheGrab {win {cmd ";"}} {
	set ofocus "[focus]"
	set save ""
	DisablePath $ofocus save $win
	focus $win
	eval $cmd
	tkwait window $win
	update
	ReablePath save
	update
	focus $ofocus
	update
	}

###########################

proc progressopen {} {
	global progressfocus progressgrab
	global progressrect

	catch {destroy .progress}
	toplevel .progress -cursor watch
	wm transient .progress .
	wm protocol .progress WM_DELETE_WINDOW {;}
	wm title .progress "XContour- busy"
	wm geometry .progress +200+200
	frame .progress.f -relief groove -bd 4
	frame .progress.f.t -relief groove -bd 2
		label .progress.f.t.l -bitmap hourglass
		label .progress.f.t.c -text "Working..."
		label .progress.f.t.r -bitmap hourglass
		pack .progress.f.t.l -side left -fill y
		pack .progress.f.t.c -side left -fill both -expand yes
		pack .progress.f.t.r -side right -fill y
	frame .progress.f.gap -height 5
	canvas .progress.f.b -relief sunken -width 10c -height 7m
	set progressrect [.progress.f.b create rectangle 0 0 0 7m -width 0 -fill blue -outline blue]
	message .progress.f.m -width 10c
	pack .progress.f.t .progress.f.gap .progress.f.b .progress.f.m -side top -fill both -padx 2 -pady 2
	pack .progress.f -padx 2 -pady 2
	set progressfocus [focus]
	set progressgrab [grab current .progress]
	focus .progress
	tkwait visibility .progress
	grab .progress
	}

###########################

proc progressupdate {howmuch of txt} {
	global progressrect
	set pct [expr $howmuch*100/$of]
	set c m
	.progress.f.b coords $progressrect 0 0 $pct$c 7m
	if {"$txt" != ""} {.progress.f.m configure -text "$txt"}
	update
	}

###########################

proc progressclose {} {
	global progressfocus progressgrab
	focus $progressfocus
	if {"$progressgrab" != ""} {grab $progressgrab}
	destroy .progress
	}

###########################

proc Information {txt {root .}} {
	catch {destroy .info}
	toplevel .info -cursor gumby
	wm title .info "Information"
	wm protocol .info WM_DELETE_WINDOW {.info.b invoke}
	wm transient .info $root
	wm geometry .info +250+250
	frame .info.t -relief groove -bd 3
	message .info.t.t -text "$txt" -aspect 1200
	label .info.t.i -bitmap info
	pack .info.t.i .info.t.t -side left -fill both -expand yes -padx 5 -pady 5
	button .info.b -text "Ok" -command "destroy .info"
	pack .info.t .info.b -side top -fill both -expand yes -padx 4 -pady 4

	tkwait visibility .info
	shortcut .info Return .info.b
	DoTheGrab .info
	}

###########################

proc CreateMenuFromList {name list {prefix ""} {fg ""} {bg ""} {afg ""} {abg ""}} {
	menu $name
	if {$fg != ""} {$name configure -foreground $fg}
	if {$bg != ""} {$name configure -background $bg}
	if {$afg != ""} {$name configure -activeforeground $afg}
	if {$abg != ""} {$name configure -activebackground $abg}
	foreach ii $list {
		set i [lindex $ii 0]
		set ch1 [string index $i 0]
		if {$ii == "-"} {
			$name add separator
		} elseif {$ch1 == "?"} {
			set p1 "$prefix[lindex $ii 1]"
			if {"$p1" == "$prefix"} {set p1 ""}
			set p2 [lindex $ii 2]
			if {$p2 == ""} {set p2 [string range $i 1 end]}
			$name add checkbutton -label [string range $i 1 end] -variable $p2 -command $p1
		} elseif {$ch1 == "!"} {
			set p1 "$prefix[lindex $ii 1]"
			if {"$p1" == "$prefix"} {set p1 ""}
			set p2 [lindex $ii 2]
			if {$p2 == ""} {set p2 [string range $i 1 end]}
			set p3 [lindex $ii 3]
			if {$p3 == ""} {set p3 [string range $i 1 end]}
			$name add radiobutton -label [string range $i 1 end] -variable $p2 -command $p1 -value $p3
		} elseif {$ch1 == ">"} {
			regsub {[^a-zA-Z0-9]} [string range $i 1 end] "" nn
			set nn [string tolower $nn]
			set nextname "$name.$nn"
			$name add cascade -label [string range $i 1 end] -menu $nextname
			CreateMenuFromList $nextname [lrange $ii 1 end] $prefix $fg $bg $afg $abg
		} else {
			if {$ch1 == "*"} {
				set l [string range $i 1 end]
			} else {
				set l $i
				}
			$name add command -label $l -command "$prefix[lindex $ii 1]"
			}
		}
	}

###########################

proc mytk_entryLeft w {
	set x [expr {[$w index insert] -1}]
	if {$x != -1} {$w icursor $x}
	}

###########################

proc mytk_entryRight w {
	$w icursor [expr {[$w index insert] +1}]
	}

###########################

proc mytk_entryDelete w {
	$w delete [$w index insert]
	}

###########################

proc mytk_entryFloat {w a {maxlen 15}} {
	if {[string length "[$w get]"] < $maxlen} {
	       	set ok 0
		set content [$w get]
		set idx [$w index insert]
	       	if {[lsearch {0 1 2 3 4 5 6 7 8 9} $a] != -1 & ($idx != 0 | [string index $content 0] != "-")} {set ok 1}
	       	if {"$a" == "-" & $idx == 0 & [string index $content 0] != "-"} {set ok 1}
	       	if {"$a" == "." & [string first "." $content] == -1 & ($idx != 0 | [string index $content 0] != "-")} {set ok 1}
	     	if $ok {
     			catch {$w delete sel.first sel.last}
     	       		$w insert insert $a
	              	tk_entrySeeCaret $w
	       		}
	       	}
	}

###########################

proc mytk_entryInt {w a {maxlen 15}} {
	if {[string length "[$w get]"] < $maxlen} {
	       	set ok 0
		set content [$w get]
		set idx [$w index insert]
	       	if {[lsearch {0 1 2 3 4 5 6 7 8 9} $a] != -1 & ($idx != 0 | [string index $content 0] != "-")} {set ok 1}
	       	if {"$a" == "-" & $idx == 0 & [string index $content 0] != "-"} {set ok 1}
	     	if $ok {
     			catch {$w delete sel.first sel.last}
     	       		$w insert insert $a
	              	tk_entrySeeCaret $w
	       		}
	       	}
	}

###########################

proc mytk_entryPFloat {w a {maxlen 15}} {
	if {[string length "[$w get]"] < $maxlen} {
	       	set ok 0
		set content [$w get]
		set idx [$w index insert]
	       	if {[lsearch {0 1 2 3 4 5 6 7 8 9} $a] != -1} {set ok 1}
	       	if {"$a" == "." & [string first "." $content] == -1} {set ok 1}
	     	if $ok {
     			catch {$w delete sel.first sel.last}
     	       		$w insert insert $a
	              	tk_entrySeeCaret $w
	       		}
	       	}
	}

###########################

proc mytk_entryPInt {w a {maxlen 15}} {
	if {[string length "[$w get]"] < $maxlen} {
	       	set ok 0
		set content [$w get]
		set idx [$w index insert]
	       	if {[lsearch {0 1 2 3 4 5 6 7 8 9} $a] != -1} {set ok 1}
	     	if $ok {
     			catch {$w delete sel.first sel.last}
     	       		$w insert insert $a
	              	tk_entrySeeCaret $w
	       		}
	       	}
	}

###########################

proc mytk_entryRange {w a pat {maxlen 50}} {
	if {[string length "[$w get]"] < $maxlen} {
	     	if [regexp "\[$pat\]" $a] {
     			catch {$w delete sel.first sel.last}
            		$w insert insert $a
	              	tk_entrySeeCaret $w
	       		}
	       	}
	}

###########################

proc CompareDirs {dir1 dir2} {
	set here [pwd]
	if [catch {cd $dir1}] {return 0}
	set adir1 [pwd]
	cd $here
	if [catch {cd $dir2}] {return 0}
	set adir2 [pwd]
	cd $here
	if {$adir2 == $adir1} {return 1}
	return 0
	}

###########################

proc FileInDir {file dir} {
	return [CompareDirs [file dirname $file] $dir]
	}

###########################

proc RelName {file} {
	if [FileInDir $file [pwd]] {return [file tail $file]}
	return $file
	}

###########################


