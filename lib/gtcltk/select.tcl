
proc GSelect { element } {
    
    set sel [GSelect_::create $element]
    return $sel

}

namespace eval GSelect_ {
    variable count 1
    variable dblclick
    variable selected ""
}

proc GSelect_::create { element } {
    global env
    variable selected

    toplevel .selwin -width 40 -height 80 
    set sw    [ScrolledWindow .selwin.sw -relief sunken -borderwidth 2 ]
    wm title .selwin "Select item"

    set tree  [Tree $sw.tree \
                   -relief flat -borderwidth 0 -width 15 -highlightthickness 0\
		   -redraw 1 -dropenabled 1 -dragenabled 1 \
                   -opencmd   "GSelect_::moddir 1 $sw.tree" \
                   -closecmd  "GSelect_::moddir 0 $sw.tree"] 

    $sw setwidget $tree

    regexp -- {(.+)x(.+)([+-].+)([+-].+)} [wm geometry .] g w h x y
    #set w [expr int(2*$w/3)]
    set w 300
    set h 500
    wm geometry .selwin ${w}x$h$x$y

    pack $sw    -side top  -expand yes -fill both
    pack $tree  -side top -expand yes -fill both 

    $tree bindText  <ButtonPress-1>        "GSelect_::select $tree"
    $tree bindImage <ButtonPress-1>        "GSelect_::select $tree"
    $tree bindText  <Double-ButtonPress-1> "GSelect_::selectclose $tree"
    $tree bindImage <Double-ButtonPress-1> "GSelect_::selectclose $tree"

    set location_path "$env(GISDBASE)/$env(LOCATION_NAME)/"
    set current_mapset $env(MAPSET)
    set sympath "$env(GISBASE)/etc/symbol/"
    
    if {$element != "symbol"} {
	foreach dir [exec g.mapsets -p] {
	    set windfile "$location_path/$dir/WIND"
	    if { ! [ file exists $windfile ] } { continue }
	    $tree insert end root ms_$dir -text $dir -data $dir -open 1 \
		-image [Bitmap::get openfold] -drawcross auto

	    set path "$location_path/$dir/$element/"
	    foreach fp [ lsort [glob -nocomplain $path/*] ]  {
		set file [file tail $fp]
		$tree insert end ms_$dir $file@$dir -text $file -data $file \
		    -image [Bitmap::get file] -drawcross never
	    }
	}
    }

    if {$element == "symbol"} {
	$tree insert end root ms_$sympath -text SYMBOLS -data $sympath -open 1 \
	    -image [Bitmap::get openfold] -drawcross auto
	
	foreach ic_dir [ lsort [glob -nocomplain $sympath/*] ]  {
	    set dir_tail [file tail $ic_dir]
	    $tree insert end ms_$sympath ms_$dir_tail  -text $dir_tail -data $dir_tail \
		-image [Bitmap::get folder] -drawcross auto

	    foreach ic_file [ lsort [glob -nocomplain $sympath/$dir_tail/*] ]  {
		set file [file tail $ic_file]
		$tree insert end ms_$dir_tail $file@$dir_tail -text $file -data $file \
		    -image [Bitmap::get file] -drawcross never
	    }
	}
    }

    $tree configure -redraw 1

    # buttons
    button .selwin.ok -text Ok -command {
        destroy .selwin
    }
    button .selwin.cancel -text Cancel -command {
        set GSelect_::selected ""
        destroy .selwin
    }
    pack .selwin.ok .selwin.cancel -side left -expand yes


    # ScrollView
    toplevel .selwintop -relief raised -borderwidth 2
    wm protocol .selwintop WM_DELETE_WINDOW {
        # don't kill me
    }
    wm overrideredirect .selwintop 1
    wm withdraw .selwintop
    wm transient .selwintop .
    ScrollView .selwintop.sv -window $tree -fill black
    pack .selwintop.sv -fill both -expand yes

    tkwait window .selwin

    destroy .selwintop

    if { $selected != "" } {
        regexp {([^@]+)@(.+)} $selected x file mapset
        
	foreach sel [ lsort [glob -nocomplain $sympath/*] ]  {
	    set sel [file tail $sel]
	     if {$mapset == $sel} {
		 return "$sel/$file"
		 exit
	     }  
	}
	
	if { $mapset == $current_mapset} {
            return $file    
        } 
 
	if {$mapset != $current_mapset} {
            return "$file@$mapset"
        }
    }

    return ""
}


proc GSelect_::select { tree node } {
    variable selected
 
    set parent [$tree parent $node]
    if { $parent == "root" } { return }
 
    $tree selection set $node
    update
    set selected $node
}

proc GSelect_::selectclose { tree node } {
    GSelect_::select $tree $node
    destroy .selwin
}

proc GSelect_::moddir { idx tree node } {
    if { $idx && [$tree itemcget $node -drawcross] == "allways" } {
        getdir $tree $node [$tree itemcget $node -data]
        if { [llength [$tree nodes $node]] } {
            $tree itemconfigure $node -image [Bitmap::get openfold]
        } else {
            $tree itemconfigure $node -image [Bitmap::get folder]
        }
    } else {
        $tree itemconfigure $node -image [Bitmap::get [lindex {folder openfold} $idx]]
    }
}


