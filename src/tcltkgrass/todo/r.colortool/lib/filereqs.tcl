proc filereqrd {dir ext} {
	global filepath
	catch {destroy .freq} 
	toplevel .freq
	wm title .freq "File Request"
	wm protocol .freq WM_DELETE_WINDOW {.freq.act.abort invoke}
	wm group .freq .
	wm transient .freq .
	wm geometry .freq +100+50
	wm minsize .freq 322 262
	frame .freq.l
	frame .freq.l.d
	frame .freq.l.f
	frame .freq.act
	label .freq.l.d.ttl -text "Directories:" 
	pack .freq.l.d.ttl -side top -fill x
	scrollbar .freq.l.d.s -command ".freq.l.d.b yview"
	pack .freq.l.d.s -side right -fill y
	listbox .freq.l.d.b -relief raised -yscrollcommand ".freq.l.d.s set" 
	pack .freq.l.d.b -side left -expand yes -fill both
	label .freq.l.f.ttl -text "Files:" 
	pack .freq.l.f.ttl -side top -fill x
	scrollbar .freq.l.f.s -command ".freq.l.f.b yview"
	pack .freq.l.f.s -side right -fill y
	listbox .freq.l.f.b -relief raised -yscrollcommand ".freq.l.f.s set"
	pack .freq.l.f.b -side left -expand yes -fill both
	frame .freq.act.m
	button .freq.act.m.parent -text "Parent" -command "filereqrdsetdir .. \"$ext\"" -width 10
	button .freq.act.m.root -text "Root" -command "filereqrdsetdir / \"$ext\"" -width 10
	button .freq.act.ok -text "Ok" -command {
		if [expr {"[selection own]" == ".freq.l.f.b"}] {
			global filepath
			set filepath "[pwd]/[lindex [selection get] 0]"
			destroy .freq
			}
		}  -width 10
	button .freq.act.abort -text "Abort" -command {destroy .freq; global filepath; set filepath ""} -width 10
	pack .freq.act.m.parent .freq.act.m.root -expand yes  -side left -fill x
	pack .freq.act.m -side top -fill x -expand yes
	pack .freq.act.ok .freq.act.abort -expand yes -side left -fill x
	label .freq.where -text "" -relief sunken -anchor w
	pack .freq.l -fill both -expand yes
	pack .freq.where .freq.act -fill x 
	pack .freq.l.d .freq.l.f -side left -fill both -expand yes

	filereqrdsetdir $dir $ext
	set a [concat {if [expr {[selection own] == ".freq.l.d.b"}]} "\{" filereqrdsetdir {[selection get]}]
	bind .freq.l.d.b <Double-1> [concat "catch \{" $a "\"$ext\"" "\}\}"]
	unset a
	bind .freq.l.f.b <Double-1> {.freq.act.ok invoke}
	shortcut .freq Return .freq.act.ok
	shortcut .freq Escape .freq.act.abort
	shortcut .freq period .freq.act.m.parent
	shortcut .freq slash .freq.act.m.root
	DoTheGrab .freq	
	if [catch {set zzz $filepath}] {return $dir}
	if [expr {$filepath != ""}] {return $filepath}
	return $dir
	}

proc filereqrdsetdir {dir ext} {

	.freq.act.ok configure -state disabled
	.freq.act.abort configure -state disabled
	.freq.act.m.parent configure -state disabled
	.freq.act.m.root configure -state disabled
	update
	.freq.l.f.b delete 0 end
	.freq.l.d.b delete 0 end

	set cdir $dir
	set travfile "/"
	if [expr ![file exists $cdir]] {set cdir "/"}
	set selfile ""
	if [file isfile $cdir] {
		set cdir [file dirname $dir] 
		set selfile [file tail $dir]
		}
	cd $cdir
	.freq.where configure -text [pwd]
	set theglob [expr [expr {"$ext" == ""}]?"*":"*.$ext"]
	foreach i [lsort [glob -nocomplain $theglob]] {
		if [file isfile $i] {
	  		.freq.l.f.b insert end $i
			if {"$selfile" == "$i"} {
		   	    	.freq.l.f.b select clear
				.freq.l.f.b select from end
	    			.freq.l.f.b select to end
				.freq.l.f.b yview end
				}
			}
		}
	foreach i [lsort [glob -nocomplain */]] {
		if [file exists $i/.] {.freq.l.d.b insert end $i}
		}
	.freq.act.ok configure -state normal
	.freq.act.abort configure -state normal
	.freq.act.m.parent configure -state normal
	.freq.act.m.root configure -state normal
	}

###############

proc filereqwr {dir deffn ext {title "File Request"}} {
	global filepath
	catch {destroy .freq} 
	toplevel .freq
	wm title .freq "$title"
	wm protocol .freq WM_DELETE_WINDOW {.freq.act.abort invoke}
	wm group .freq .
	wm transient .freq .
	wm geometry .freq +100+50
	wm minsize .freq 322 262
	frame .freq.l
	frame .freq.l.d
	frame .freq.l.f
	frame .freq.act
	label .freq.l.d.ttl -text "Directories:" 
	pack .freq.l.d.ttl -side top -fill x
	scrollbar .freq.l.d.s -command ".freq.l.d.b yview"
	pack .freq.l.d.s -side right -fill y
	listbox .freq.l.d.b -relief raised -yscrollcommand ".freq.l.d.s set" 
	pack .freq.l.d.b -side left -expand yes -fill both
	label .freq.l.f.ttl -text "Files:" 
	pack .freq.l.f.ttl -side top -fill x
	scrollbar .freq.l.f.s -command ".freq.l.f.b yview"
	pack .freq.l.f.s -side right -fill y
	listbox .freq.l.f.b -relief raised -yscrollcommand ".freq.l.f.s set"
	pack .freq.l.f.b -side left -expand yes -fill both
	frame .freq.m
	button .freq.m.parent -text "Parent" -command "filereqwrsetdir .. \"$ext\"" -width 10
	button .freq.m.root -text "Root" -command "filereqwrsetdir / \"$ext\"" -width 10
	button .freq.act.ok -text "Ok" -width 10
	if {"$ext" != ""} {
		.freq.act.ok configure -command [format "%s%s%s%s%s%s" {
		if {"[.freq.file.t get]" != ""}} " \{" {
			global filepath
			set filepath "[file rootname "[pwd]/[.freq.file.t get]"].} $ext {"
			destroy .freq
			} "\}"]
	} {
		.freq.act.ok configure -command {
		if {"[.freq.file.t get]" != ""} {
			global filepath
			set filepath "[pwd]/[.freq.file.t get]"
			destroy .freq
			}
			}
		}		
	button .freq.act.abort -text "Abort" -command {destroy .freq; global filepath; set filepath ""} -width 10
	pack .freq.m.parent .freq.m.root -expand yes  -side left -fill x
	pack .freq.act.ok .freq.act.abort -expand yes -side left -fill x
	label .freq.where -text "" -relief sunken -anchor w
	frame .freq.file
	label .freq.file.l -text "File: "
	if [file isfile $dir] {set curfn $dir} else {set curfn $deffn}
	if {$ext != ""} {
		set curfn "[file rootname [file tail $curfn]].$ext"
	} {
		set curfn "[file tail $curfn]"
		}
	entry .freq.file.t -relief ridge -bd 3
	.freq.file.t insert end $curfn
	pack .freq.file.l -side left -fill y
	pack .freq.file.t -side left -fill both -expand yes
	pack .freq.l -fill both -expand yes
	pack .freq.where -fill x
	pack .freq.file -fill x -padx 1 -pady 3
	pack .freq.m .freq.act -fill x
	pack .freq.l.d .freq.l.f -side left -fill both -expand yes

	filereqwrsetdir $dir $ext

	set a [concat {if [expr {[selection own] == ".freq.l.d.b"}]} "\{" filereqwrsetdir {[selection get]}]
	bind .freq.l.d.b <Double-1> [concat "catch \{" $a "\"$ext\"" "\}\}"]
	unset a

	bind .freq.l.f.b <1> {catch {set fnamreq [selection get]; .freq.file.t delete 0 end; .freq.file.t insert end $fnamreq;unset fnamreq}}
	bind .freq.file.t <Return> [concat {filereqwrsetdir .} "$ext"]
	shortcut .freq Escape .freq.act.abort
	shortcut .freq.file.t Escape .freq.act.abort
	entryOrder .freq .freq.file.t
	DoTheGrab .freq.file.t	
	if [catch {set zzz $filepath}] {return $dir}
	if [expr {$filepath != ""}] {return $filepath}
	return $dir
	}

proc filereqwrsetdir {dir ext} {

	.freq.act.ok configure -state disabled
	.freq.act.abort configure -state disabled
	.freq.m.parent configure -state disabled
	.freq.m.root configure -state disabled
	.freq.file.t configure -state disabled
	update
	.freq.l.f.b delete 0 end
	.freq.l.d.b delete 0 end

       	set selfile [.freq.file.t get]
	set cdir "$dir/$selfile"
	if {"$selfile" == ""} {set cdir $dir}
	if {([string index $selfile 0] == "/") && ($dir == ".")} {set cdir $selfile}
	while {![file exists $cdir] && ("$cdir" != "")} {set cdir "[file dirname $cdir]"}
	if {$cdir == ""} {set cdir "/"}
	if [string match "*/*" $selfile] {set selfile "[string range $selfile [expr [string length $cdir] - [string length $dir] + 2] end]"}
	set selfile [string trimleft $selfile "/"]
	if [file isdirectory $selfile] {set selfile ""}
	if {[file isfile $cdir] && ($selfile == "")} {
		set selfile [file tail $cdir]
		set cdir [file dirname $cdir] 
		}
	if {$ext != ""} {if {$selfile == ".$ext"} {set selfile ""}}
	while {![file isdirectory $cdir] && ("$cdir" != "")} {set cdir "[file dirname $cdir]"}
	cd $cdir
	.freq.where configure -text [pwd]
	.freq.file.t configure -state normal
	.freq.file.t delete 0 end
	.freq.file.t insert end "$selfile"
	.freq.file.t configure -state disabled
	if {$ext != ""} {set selfile "[file rootname $selfile].$ext"}
	set theglob [expr [expr {"$ext" == ""}]?"*":"*.$ext"]
	foreach i [lsort [glob -nocomplain $theglob]] {
		if [file isfile $i] {
	  		.freq.l.f.b insert end $i
			if {"$selfile" == "$i"} {
		   	    	.freq.l.f.b select clear
				.freq.l.f.b select from end
	    			.freq.l.f.b select to end
				.freq.l.f.b yview end
				}
			}
		}
	foreach i [lsort [glob -nocomplain */]] {
		if [file exists $i/.] {.freq.l.d.b insert end $i}
		}
	.freq.act.ok configure -state normal
	.freq.act.abort configure -state normal
	.freq.m.parent configure -state normal
	.freq.m.root configure -state normal
	.freq.file.t configure -state normal
	}
