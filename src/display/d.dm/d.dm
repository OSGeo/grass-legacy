#!/bin/sh
# the next line restarts using wish \
exec $GRASS_WISH "$0" "$@"

lappend auto_path $env(GISBASE)/bwidget
package require BWidget 

# ----- PROCS -----
# ----- set procs
proc set_create { } {
    global slb set nset mp sset
    set s $nset
    set set($s,nmap) 0
    set set($s,selmap) -1
    $slb insert end $s -text "Set $s"
    set page [$mp add $s]
    set msw [ScrolledWindow $page.sw -relief sunken -borderwidth 2]
    set msf [ScrollableFrame $msw.msf ]
    $msw setwidget $msf
    set frame [$msf getframe]
    set set($s,frame) $frame
    pack $msw $msf -fill both -expand yes
    $slb selection set $s
    set sset $s 
    $mp raise $s
    incr nset
    return $s
}

proc set_edit { node } {
    global slb
    set t [$slb edit $node [$slb itemcget $node -text]]
    if { $t != "" } {
        $slb itemconfigure $node -text $t   
    }
}

proc set_select { node } {
    global slb sset mp
    $slb selection set $node
    set sset $node   
    $mp raise $node
}

proc set_rm { } {
    global slb sset mp
    if { $sset < 0 } { 	puts stdout "Set not selected."; return }
    $mp delete $sset
    $slb delete $sset
    set sset -1
}

proc mon_open { mon } {
    if {[lsearch -exact [mon_get] $mon] < 0} {
	puts stdout "Monitor $mon is not opened. I will try to open."
	set cmd "d.mon start=$mon"
	execute $cmd
	return    
    }
}

proc set_display { dtype } {
    global set sset smon map
    set s $sset
    if { $s < 0 } { puts stdout "Set not selected."; return }

    mon_open $smon

    set cmon [eval exec d.mon -p]
    set cm ""
    regexp -- {Currently selected monitor: (.+)$} $cmon r cm 
    puts stdout "Current mon: $cm"

    set cmd "d.mon select=$smon"
    execute $cmd
    set cmd "g.region save=_d.dm.current"
    execute $cmd

    if { $dtype != "a" } {
	set reg [element_list "windows" ]
	set rege 0		
	foreach r $reg {
	    if { $r == "_d.dm.$smon" } { set rege 1; break } 
	}
	if { $rege == 1 } {
	    set cmd "g.region region=_d.dm.$smon"
	    execute $cmd
	} else {
	    set cmd "g.region -d"
	    execute $cmd
	}
    }	

    if { $dtype == "a" } {
	set cmd "g.region -d"
	execute $cmd
    }	

    if { $dtype == "z" } {
	set cmd "d.zoom"
	execute $cmd
    }	

    if { $dtype == "p" } {
	set cmd "d.pan"
	execute $cmd
    }

    set cmd "d.erase"
    execute $cmd
    set f $set($s,frame)
    foreach mw [pack slaves $f] {
	regexp -- {.*\.([^.]*)$} $mw p m 
        map_display $s $m
    }
    
    set cmd "g.region save=_d.dm.$smon"
    execute $cmd
    set cmd "g.region region=_d.dm.current"
    execute $cmd

    # display raster map legends
    foreach mw [pack slaves $f] {
	regexp -- {.*\.([^.]*)$} $mw p m
	if { ($map($s,$m,_type) == "r") && ($map($s,$m,_leg_mon) != "") } {
	    mon_open $map($s,$m,_leg_mon)	
	    set cmd "d.mon select=$map($s,$m,_leg_mon)"
	    execute $cmd	
	    if { $map($s,$m,_leg_mon) != $smon } {	
		set cmd "d.erase"
		execute $cmd	    
	    }
	    set cmd "d.legend map=$map($s,$m,map) color=$map($s,$m,_leg_color) show=$map($s,$m,_leg_show)"
	    if { $map($s,$m,_leg_lines) > 0 } { append cmd " lines=$map($s,$m,_leg_lines)" }
	    execute $cmd	    
    	    
	}
    }
    
    if { $cm != "" } {  
	set cmd "d.mon select=$cm"
	execute $cmd
    }
}

proc set_query { } {
    global set sset smon slb map
    set s $sset
    set m $set($s,selmap)    

    if { $s < 0 || $m < 0 } { puts stdout "Set or map not selected."; return }

    if {[lsearch -exact [mon_get] $smon] < 0} {
	puts stdout "Monitor $smon is not opened"
	return    
    }

    set cmon [eval exec d.mon -p]
    set cm ""
    regexp -- {Currently selected monitor: (.+)$} $cmon r cm 

    puts stdout "Current mon: $cm"

    set cmd "d.mon select=$smon"
    execute $cmd
    set cmd "g.region save=_d.dm.current"
    execute $cmd

    set reg [element_list "windows" ]
    set rege 0		
    foreach r $reg {
	if { $r == "_d.dm.$smon" } { set rege 1; break } 
    }
    if { $rege == 1 } {
	set cmd "g.region region=_d.dm.$smon"
	execute $cmd
    } else {
	set cmd "g.region -d"
	execute $cmd
    }

    map_query $s $m

    set cmd "g.region region=_d.dm.current"
    execute $cmd
    
    if { $cm != "" } {  
	set cmd "d.mon select=$cm"
	execute $cmd
    }
}

# ----- map procs

proc map_create { type } {
    global map set sset s m
    set s $sset
    if { $s < 0 } { puts stdout "Set not selected."; return }
    if {[lsearch -exact { r l a s pl} $type] < 0} {
	puts stdout "I don't know map type '$type'."
	return 
    }    
    set m $set($s,nmap)
    set f $set($s,frame)
    frame $f.$m
    pack $f.$m -side top

 
    pack configure $f.$m -anchor w
    set map($s,$m,_type) $type
    set map($s,$m,_widget) $f.$m    
    button $f.$m._sel -text $type -command "map_sel $m" -height 1 -width 2 -relief "raised"
    pack $f.$m._sel -side left 
    switch $type {
	r {
	    Entry $f.$m.map -width 10 -text "" -textvariable map($s,$m,map) \
		-helptext "raster map name\nuse right button to select from list"
	    bind $f.$m.map <ButtonPress-3> "map_par_set r $s $m map" 
	    checkbutton $f.$m.o -text "overlay" -variable map($s,$m,-o)
	    ComboBox $f.$m._leg_mon -label "legend" -underline 0 \
		-labelwidth 0 -width 2  -textvariable map($s,$m,_leg_mon) \
		-values {"" "x0" "x1" "x2" "x3" "x4" "x5" "x6"} \
		-helptext "Monitor for legend"
	    SelectColor $f.$m._leg_color -type menubutton -variable map($s,$m,_leg_color)
	    set map($s,$m,_leg_color) gray
	    Label $f.$m.lab1 -text "lines" 
	    Entry $f.$m._leg_lines -width 3 -text "" -textvariable map($s,$m,_leg_lines) \
		-helptext "Number of legend lines"
	    ComboBox $f.$m._leg_show -label "show" -underline 0 \
		-labelwidth 0 -width 7 -textvariable map($s,$m,_leg_show) \
		-values {"val,cat" "val" "cat"} \
		-helptext "Show either values or categories"
	    set map($s,$m,_leg_show) "val,cat"			
	    pack $f.$m.map $f.$m.o $f.$m._leg_mon $f.$m._leg_color $f.$m.lab1 $f.$m._leg_lines \
		 $f.$m._leg_show -side left   
	}    
	l {
	    Entry $f.$m.map -width 10 -text "" -textvariable map($s,$m,map) \
		-helptext "vector map name\nuse right button to select from list"	    
	    bind $f.$m.map <ButtonPress-3> "map_par_set l $s $m map" 	    
	    SelectColor $f.$m.color -type menubutton -variable map($s,$m,color)
	    set map($s,$m,color) white
	    pack $f.$m.map $f.$m.color -side left   
	}
	a {
	    Entry $f.$m.map -width 10 -text "" -textvariable map($s,$m,map) \
		-helptext "vector map name\nuse right button to select from list"	    
	    bind $f.$m.map <ButtonPress-3> "map_par_set a $s $m map"
	    Label $f.$m.lab1 -text "fill color" 	    
	    SelectColor $f.$m.fillcolor -type menubutton -variable map($s,$m,fillcolor)
	    set map($s,$m,fillcolor) white
	    Label $f.$m.lab2 -text "line color"	    
	    SelectColor $f.$m.linecolor -type menubutton -variable map($s,$m,linecolor)
	    set map($s,$m,linecolor) white	    
	    pack $f.$m.map $f.$m.lab1 $f.$m.fillcolor $f.$m.lab2 $f.$m.linecolor -side left
	}	
	s {
	    Entry $f.$m.sitefile -width 10 -text "" -textvariable map($s,$m,sitefile) \
		-helptext "site map name\nuse right button to select from list"	    
	    bind $f.$m.sitefile <ButtonPress-3> "map_par_set s $s $m sitefile" 	    	    
	    SelectColor $f.$m.color -type menubutton -variable map($s,$m,color)
	    set map($s,$m,color) white	    
	    SpinBox $f.$m.size -label "" -text 5 -underline 0 \
		-labelwidth 0 -width 2 \
		-range {1 100 1} -textvariable map($s,$m,size) \
		-helptext "Size"  
	    ComboBox $f.$m.type -label "" -underline 0 \
		-labelwidth 0 -width 7  -textvariable map($s,$m,type) \
		-values {"x" "diamond" "box" "+"} \
		-helptext "Type of the icon"
	    set map($s,$m,type) x	
	    pack $f.$m.sitefile $f.$m.color $f.$m.size $f.$m.type -side left   
	}
	pl {
	    Entry $f.$m.file -width 10 -text "" -textvariable map($s,$m,file) \
		-helptext "paint labels file name\nuse right button to select from list"
	    bind $f.$m.file <ButtonPress-3> "map_par_set pl $s $m file"
	    pack $f.$m.file -side left
	}		
    }
    incr set($s,nmap)
    return $m
}

proc map_par_set { t s m par } {
    global map
    set n [map_par_get $t]
    puts stdout "n= $n  set map($s,$m,$par)" 
    if { $n != "" } {  set map($s,$m,$par) $n }
}

proc map_sel { m } {
    global set map sset
    set s $sset
    set f $set($s,frame)
    foreach mw [pack slaves $f] {
	#regexp -- {.*\.([^.]*)$} $mw p m 
        #map_display $s $m
	$mw._sel configure -relief "raised"
    }  
    $f.$m._sel configure -relief "sunken"
    set set($s,selmap) $m
}

proc map_up {  } {
    global set slb map sset
    set s $sset
    set m $set($s,selmap)
    if { $s < 0 || $m < 0 } { puts stdout "Set or map not selected."; return }
    set f $set($s,frame)
    set smw $map($s,$m,_widget)
    set i 0
    foreach mw [pack slaves $f] {
	if { $smw == $mw && $i > 0 } {
	    pack configure $smw -before $lastmw
	    return
	}
	set lastmw $mw
	incr i
    }  
}

proc map_down {  } {
    global set slb map sset
    set s $sset
    set m $set($s,selmap)
    if { $s < 0 || $m < 0 } { puts stdout "Set or map not selected."; return }
    set f $set($s,frame)
    set smw $map($s,$m,_widget)
    set move 0
    foreach mw [pack slaves $f] {
	if { $move == 1 } {
	    pack configure $lastmw -after $mw
	    return
	}    
	if { $smw == $mw } { set move 1; set lastmw $mw; }
    }  
}

proc map_rm {  } {
    global set slb map sset
    set s $sset
    set m $set($s,selmap)
    if { $s < 0 || $m < 0 } { puts stdout "Set or map not selected."; return }
    set f $set($s,frame)
    destroy $map($s,$m,_widget)
    set set($s,selmap) -1
}

proc map_type_get { } {
     set list [list {r "raster"} {l "vector lines"} {a "vector areas"} {s "sites"} {pl "paint labels"}]
     return [list_select $list]
}  

proc map_par_get { type } {
    switch $type {
        r {
	    set list [element_list cell]
        }
        l {
	    set list [element_list dig]
        }
        a {
	    set list [element_list dig_plus]
        }		
        s {
	    set list [element_list site_lists]
        }
        pl {
	    set list [element_list paint/labels]
        }	
    }  

    if {[llength $list] > 0} {
        foreach i $list { 
	    lappend nlist  [list $i $i]
	}
        return [list_select $nlist]
    }
    return ""
}

proc map_display { s m } {
    global map
    set type $map($s,$m,_type)
    switch $type {
        r {
	    set cmd "d.rast map=$map($s,$m,map)"
	    if { $map($s,$m,-o) == 1 } { append cmd " -o" }
        }    
        l {
	    set col [color_get $map($s,$m,color)]
	    set cmd "d.vect map=$map($s,$m,map) color=$col"
        }
        a {
	    set fillcol [color_get $map($s,$m,fillcolor)]	
	    set linecol [color_get $map($s,$m,linecolor)]
	    set cmd "d.area map=$map($s,$m,map) fillcolor=$fillcol linecolor=$linecol"
        }	
        s {
	    set col [color_get $map($s,$m,color)]
            set cmd "d.sites sitefile=$map($s,$m,sitefile) color=$col \
    	             size=$map($s,$m,size) type=$map($s,$m,type)"
        }
        pl {
            set cmd "d.paint.labels file=$map($s,$m,file)"
        }	
	default {
	    puts stdout "I don't know how to display map type $type."
	    return
	}
    }    
    execute $cmd
}

proc map_query { s m } {
    global map
    set type $map($s,$m,_type)
    switch $type {
        r {
	    set cmd "d.what.rast map=$map($s,$m,map)"
        }    
        l {
	    set cmd "d.what.vect map=$map($s,$m,map)"
        }
        a {
	    set cmd "d.what.vect map=$map($s,$m,map)"
        }	
        s {
            set cmd "d.what.sites sites=$map($s,$m,sitefile)"
        }
	default {
	    puts stdout "I don't know how to query map type $type."
	    return
	}	
    }    
    execute $cmd
}

# ----- misc procs

proc dm_save { } {
    global env slb set map
    set slist [$slb items]
    set fpath "$env(GISDBASE)/$env(LOCATION_NAME)/$env(MAPSET)/.d.dmrc"
    puts stdout "Writting to $fpath" 
    set file [open $fpath w]

    foreach s $slist {
	set sname [$slb itemcget $s -text]
        puts $file "_set_name=$sname\n" 

	set f $set($s,frame)
	foreach mw [pack slaves $f] {
	    regexp -- {.*\.([^.]*)$} $mw p m 
	    set type $map($s,$m,_type)
	    switch $type {
       		r {
		    puts $file "_map_type=r\nmap=$map($s,$m,map)\n-o=$map($s,$m,-o)"
		    puts $file "_leg_mon=$map($s,$m,_leg_mon)\n_leg_color=$map($s,$m,_leg_color)"
		    puts $file "_leg_lines=$map($s,$m,_leg_lines)\n_leg_show=$map($s,$m,_leg_show)\n"		    
    		}    
    		l {
		    puts $file "_map_type=l\nmap=$map($s,$m,map)\ncolor=$map($s,$m,color)\n"		
    		}
    		a {
		    puts $file "_map_type=a\nmap=$map($s,$m,map)\nfillcolor=$map($s,$m,fillcolor)"
		    puts $file "linecolor=$map($s,$m,linecolor)\n"		    
    		}		
    		s {
		    puts $file "_map_type=s\nsitefile=$map($s,$m,sitefile)\ncolor=$map($s,$m,color)"
		    puts $file "size=$map($s,$m,size)\ntype=$map($s,$m,type)\n"		    				
    		}
    		pl {
		    puts $file "_map_type=pl\nfile=$map($s,$m,file)\n"
    		}		
	    }    
	}
    }
    close $file
}

proc dm_read { } {
    global env slb set map sset
    set s -1
    set m -1
    set fpath "$env(GISDBASE)/$env(LOCATION_NAME)/$env(MAPSET)/.d.dmrc"
    if { [file exist $fpath] } {
	if { [file readable $fpath] } {
	    puts stdout "Reading from $fpath" 
	} else {
    	    puts stdout "Cannot read $fpath" 
	    return
	}	
    } else {
        puts stdout "File $fpath doesn't exist." 
        return
    }
    set file [open $fpath r]
    
    while { [gets $file in] > -1 } { 
	set d(key) ""
	set d(val) ""
	regexp -- {([^=]+)=(.+)$} $in r d(key) d(val)
	if { $d(key) != "" } {
	    switch -- $d(key) {
       		_set_name {
		    set s [set_create]
    		    $slb itemconfigure $s -text $d(val)   		    
		    set m -1
		}
       		_map_type {
		    if { $s >= 0 } {
			set mtype $d(val)
			set m [map_create $mtype]
		    }
		}		
		default {
		    if { $s >= 0  && $m >= 0 } {
	    	        switch $mtype {
           		    r {
	    			switch -- $d(key) {
           			    map { set map($s,$m,map) $d(val) }
           			    -o  { set map($s,$m,-o) $d(val) }
           			    _leg_mon { set map($s,$m,_leg_mon) $d(val) }
           			    _leg_color { set map($s,$m,_leg_color) $d(val) }
           			    _leg_lines { set map($s,$m,_leg_lines) $d(val) }
           			    _leg_show { set map($s,$m,_leg_show) $d(val) }
    				}
			    }
           		    l {
	    			switch -- $d(key) {
           			    map { set map($s,$m,map) $d(val) }
           			    color { set map($s,$m,color) $d(val) }
    				}
			    }
           		    a {
	    			switch -- $d(key) {
           			    map { set map($s,$m,map) $d(val) }
           			    fillcolor { set map($s,$m,fillcolor) $d(val) }
           			    linecolor { set map($s,$m,linecolor) $d(val) }				    
    				}
			    }			    
           		    s {
	    			switch -- $d(key) {
           			    sitefile { set map($s,$m,sitefile) $d(val) }
           			    color { set map($s,$m,color) $d(val) }
           			    size { set map($s,$m,size) $d(val) }
           			    type { set map($s,$m,type) $d(val) }				
    				}
			    }	
           		    pl {
	    			switch -- $d(key) {
           			    file { set map($s,$m,file) $d(val) }
    				}				
			    }	
			}						
		    }
		}    
	    }
	}
    }
    close $file
}

proc element_list { element } {
    global env
    set pwd [pwd]
    set inpath 1
    set list ""
    cd $env(GISDBASE)/$env(LOCATION_NAME)
    foreach dir [concat [exec g.mapsets -p] . [glob *]] {
        if {[string compare $dir .] == 0} {
	    set inpath 0
    	    continue
	}  
	if [info exists dirstat($dir)] continue
        set dirstat($dir) 1
	if {[catch {eval eval cd $env(GISDBASE)/$env(LOCATION_NAME)/$dir/$element}]} {
	    if {0 && $dir == $env(MAPSET)} {
    		tk_messageBox -message "$typ directory\n'[subst [subst $element]]'\nnon-existent or unusable" \
		-type ok
	    } 
	} elseif {[catch {glob *} names]} {
	} elseif {$inpath} {
	    if {$dir == $env(MAPSET)} {
    		eval lappend list [lsort $names]
	    } else {
    		foreach name [lsort $names] {
		    lappend list "$name ($dir)"
    		}
	    }    
	} else {
    	    foreach name [lsort $names] {
		lappend list $name@$dir
	    }
	}
    }
    cd $pwd
    return $list
}

proc list_select { list } {
    global list_select_item
    set list_select_item ""
    toplevel .list
    wm title .list "Select item"
    regexp -- {(.+)x(.+)([+-].+)([+-].+)} [wm geometry .] g w h x y 
    set w [expr int($w/3)]
    wm geometry .list ${w}x$h$x$y 
    set sw [ScrolledWindow .list.sw]
    set lb [ListBox $sw.lb -width 10 -padx 0]
    $sw setwidget $lb
    $lb bindText <ButtonPress-1> list_select_item
    pack $sw -fill both -expand yes
    frame .list.buttons
    pack .list.buttons -side bottom -fill x
    button .list.buttons.cancel -text Cancel -command {
        set list_select_item ""
        destroy .list
    }
    pack .list.buttons.cancel -side left -expand yes
    foreach i $list { 
        $lb insert end [lindex $i 0] -text [lindex $i 1]
    }
    #grab set .list
    tkwait window .list
    return $list_select_item
}

proc list_select_item { item } {
    global list_select_item
    set list_select_item $item
    .list.sw.lb selection set $item
    puts stdout "$item clicked" 
    destroy .list
}         

proc color_get { col } {
    switch $col {
        \#00bfbf { return "aqua" }
        \#0080ff { return "indigo" }
	default { 
	    if { [string index $col 0] == "#" } {
		 return "grey" 
	    } else { 
		return $col 
	    }
	}
    }	
}

proc mon_get { } {
    #xlsclients is slow but d.mon -L now hangs up 
    #set mons [eval exec d.mon -L] 

    set xdriver_list ""
    if ![catch {open "|xlsclients -l" r} input] {
	while {[gets $input line] >= 0} {
	    if {[regexp -nocase {^ *Name:.*GRASS.*Monitor: *(.*)} $line buffer monitor]} {
		lappend xdriver_list $monitor
	    }
	}
    }	
    return [lsort $xdriver_list] 
}

proc execute { cmd } {
    global dm_output
    puts stdout $cmd 
    eval exec >@stdout 2>@stdout $cmd
}

# ----- redefine colors in SelectColor to Grass colors (I don't know how better)
SelectColor ._SC -type menubutton
namespace eval SelectColor {   \
    variable _tabcolors {
        red     orange   white  white
        green   \#00bfbf white  white
        blue    \#0080ff white  white
        white   grey     white  white
        magenta brown    white  white
        yellow  black    white  white
    }
}

# ----- here it starts
# ----- number of created sets (number for next set)
set nset 0
# ----- selected set (-1 not selected)
set sset -1

wm title . "Display Manager"  
# ----- paned window for list of sets and selected set of maps
set pw [PanedWindow .pw -side top]
pack $pw -fill both -expand yes 
set spa [$pw add -minsize 30]
set mpa [$pw add -minsize 30]
# ----- pane with list of sets
set ssw [ScrolledWindow $spa.sw]
set slb [ListBox $ssw.lb -width 10 -padx 0]
$ssw setwidget $slb
$slb bindText <ButtonPress-1> set_select
$slb bindText <Double-ButtonPress-1> "set_edit"
pack $ssw -fill both -expand yes
# ----- page manager for sets of maps
#
set mp [PagesManager $mpa.pm -height 200 -width 300]
pack $mp -fill both -expand yes -side top

# ----- read rc file
dm_read
if { $nset < 1 } { set_create } else { set_select 0 }


# ----- edit set/map buttons frame -----
set ebf [frame .ebf]
pack $ebf -fill x -side top 

# ----- set buttons
Button $ebf.set_add -text "+" -command { set_create } \
    -helptext "Add set"
pack $ebf.set_add -side left -padx 5 -pady 2

Button $ebf.set_rm -text "x" -command set_rm -foreground red -activeforeground red \
    -helptext "Remove selected set"
pack $ebf.set_rm -side left -padx 5 -pady 2

Separator $ebf.sep1 -orient vertical   
pack $ebf.sep1 -padx 5 -pady 2 -fill y -side left 

# ----- map buttons
Button $ebf.map_add -text "+" \
    -command { 	global $sset 
	if { $sset < 0 } { puts stdout "Set not selected."; return } 	
	set t [map_type_get]
	if { $t != "" } { map_create $t } } \
    -helptext "Add map"
pack $ebf.map_add -side left -padx 5 -pady 2

Button $ebf.map_rm -text "x" -foreground red -activeforeground red -command map_rm \
    -helptext "Remove selected map"
pack $ebf.map_rm -side left -padx 2 -pady 5

ArrowButton $ebf.map_up -type button -dir top -width 27 -height 27 \
    -command  "map_up" -helptext "Move selected map up" 
pack $ebf.map_up -side left -padx 5 -pady 2

ArrowButton $ebf.map_down -type button -dir bottom -width 27 -height 27 \
    -command  "map_down" -helptext "Move selected map down" 
pack $ebf.map_down -side left -padx 5 -pady 2

Separator $ebf.sep2 -orient vertical   
pack $ebf.sep2 -padx 5 -pady 2 -fill y -side left 

Button $ebf.save -text "Save" -command "dm_save"
Button $ebf.close -text "Close" -command { destroy . }
pack $ebf.close $ebf.save -side right -padx 2 -pady 5

Separator .sep1 -orient horizontal   
pack .sep1 -padx 5 -pady 2 -fill x -side top 

# ----- monitors frame -----
set mof [frame .mof]
pack $mof -fill x -side top 

foreach mon { x0 x1 x2 x3 x4 x5 x6 } {
    radiobutton $mof.$mon -text $mon -variable smon -value $mon
    pack $mof.$mon -side left 
}
$mof.x0 select

Separator .sep2 -orient horizontal   
pack .sep2 -padx 5 -pady 2 -fill x -side top 

# ----- display frame -----
set df [frame .df]
pack $df -fill x -side top 

button $df.display -text "Display"  -command { set_display d} 
pack $df.display -side left -padx 2 -pady 5

button $df.all -text "All"  -command { set_display a} 
pack $df.all -side left -padx 2 -pady 5

button $df.zoom -text "Zoom"  -command { set_display z} 
pack $df.zoom -side left -padx 2 -pady 5

button $df.pan -text "Pan"  -command { set_display p} 
pack $df.pan -side left -padx 2 -pady 5

button $df.query -text "Query"  -command { set_query } 
pack $df.query -side left -padx 2 -pady 5
