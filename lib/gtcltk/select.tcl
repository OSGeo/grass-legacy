##########################################################################
#
# select.tcl
#
# tree/listbox control for interactive selection of GRASS GIS elements
#
# Author: Unknown. Possibly Jacques Bouchard, author of tcltkgrass for
#   GRASS 5. Subsequent modifications by members of the GRASS Development
#   team.
#
# Last update: January 2009
#
# COPYRIGHT:	(C) 1999 - 2009 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################
# Frame scrolling that works:
# Scroll if the window exists AND
# the window is mapped AND
# This window's parent's descendant has the focus (keyboard or mouse pointer in)
# We use the parent because the scrollbars are in the parent, and two scrollable
# Things shouldn't have the same parent.

namespace eval GSelect_ {
    variable count 1
    variable dblclick
    variable array selwin
    variable tree
    variable bind_scroll_list
    set bind_scroll_list {}
}

proc handle_scroll {amount window} {
    variable bind_scroll_list
    set scrlwin ""

    if {[lsearch -exact $bind_scroll_list $window] != -1} {
        #window is a scrollable window
        set scrlwin $window
    } else {
        # if window is not scrollable, find the parent that is scrollable
        foreach item $bind_scroll_list {
            if {[string first $item $window 0] != -1} {
                set scrlwin $item
                break
            }
        }    
    }

    if {$scrlwin == "" } {return}
    if {[tk windowingsystem] eq "win32"} {
        $scrlwin yview scroll [expr {-$amount/120}] units
    } else { 
        $scrlwin yview scroll [expr -1*$amount] units
    }
}

proc bind_scroll {frame} {
    variable bind_scroll_list

    lappend bind_scroll_list $frame    
    
    # We should thin out windows that don't exist anymore if we find them
    set new_bind_scroll_list {}
    foreach window $bind_scroll_list {
        if {[winfo exists $window]} {
            lappend new_bind_scroll_list $window
        }
    }
    set bind_scroll_list $new_bind_scroll_list

}

bind all <MouseWheel> "handle_scroll %D %W"
if {[tk windowingsystem] eq "x11"} {
    bind all <Button-4> "handle_scroll 1 %W"
    bind all <Button-5> "handle_scroll -1 %W"
}

##############################################################

proc GSelect { element args } {
    # startup procedure 

    set sel [eval [linsert $args 0 GSelect_::create $element]]
    return $sel

}


proc GSelect_::create { element args } {
    # main procedure for creating and managing selection window, which a tree
    # within a scrolling window.

    global env id
    variable selwin
    variable count
    variable tree
    
    incr count
    set id $count
    
    set selwin($id,self) selwin
    set title [G_msg "Select item"]
    set selwin($id,selected) {}
    
    if {[lsearch -exact $args "title"] > -1} {
        append title " - [lindex $args [expr [lsearch -exact $args title]+1]]"
    }
    
    # Leave selection on top of caller window till it's closed
    set parentwin "."
    if {[lsearch -exact $args "parent"] > -1} {
    	set parentwin [lindex $args [expr [lsearch -exact $args "parent"]+1]]
    	if { [string length $parentwin] > 1 } {
    		set selwin($id,self) [regsub -all {[[:space:]]|[[:punct:]]} ".selwin[string range $parentwin 1 [string length $parentwin]]" ""]
    	} elseif {[lsearch -exact $args "title"] > -1} { set selwin($id,self) [regsub -all {[[:space:]]|[[:punct:]]} ".selwin$title" ""] }
    } 
    set selwin($id,self) ".$selwin($id,self)"
    set selftop "$selwin($id,self)top"

    # Do not create another select window, if one already exists.
    if {[winfo exists $selwin($id,self)]} {
    	raise $selwin($id,self) 
    	focus $selwin($id,self) 
    	return
    }

    toplevel $selwin($id,self) -width 300 -height 400 
    set sw    [ScrolledWindow $selwin($id,self).sw -relief sunken -borderwidth 2 ]
    
    wm title $selwin($id,self) $title
    wm transient $selwin($id,self) $parentwin

    set tree  [Tree $sw.tree \
                   -relief flat -borderwidth 0 -width 15 -highlightthickness 0\
                   -redraw 1 -dropenabled 1 -dragenabled 1 \
                   -opencmd   "GSelect_::moddir 1 $sw.tree" \
                   -closecmd  "GSelect_::moddir 0 $sw.tree"] 

    $sw setwidget $tree
    bind_scroll $tree

    regexp -- {(.+)x(.+)([+-].+)([+-].+)} [wm geometry .] g w h x y
    #set w [expr int(2*$w/3)]
    set w 300
    set h 400
    wm geometry $selwin($id,self) ${w}x$h$x$y

    $tree bindText  <ButtonPress-1>        "GSelect_::select $id $tree"
    $tree bindImage <ButtonPress-1>        "GSelect_::select $id $tree"
    $tree bindText  <Double-ButtonPress-1> "GSelect_::selectclose $id $tree"
    $tree bindImage <Double-ButtonPress-1> "GSelect_::selectclose $id $tree"
    if {[lsearch $args "multiple"] >= 0} {
    	$tree bindText  <Control-ButtonPress-1> "GSelect_::select_toggle $id $tree"
    } else {
    	$tree bindText  <Control-ButtonPress-1> "GSelect_::select $id $tree"
    }
    
    set location_path [file normalize [file join "$env(GISDBASE)" "$env(LOCATION_NAME)"]]
    set current_mapset [file normalize "$env(MAPSET)"]
    set sympath [file normalize [file join "$env(GISBASE)" "etc" "symbol"]]
    
    # main selection subroutine
    if {$element != "symbol"} {
        foreach dir [exec g.mapsets -p] {
            set windfile [file normalize [file join "$location_path" "$dir" "WIND"]]
            if { ! [ file exists $windfile ] } { continue }
            if { $dir == [file tail "$current_mapset"] } {
                $tree insert end root ms_$dir -text $dir -data $dir -open 1 \
                    -image [Bitmap::get openfold] -drawcross auto
            } else {
                $tree insert end root ms_$dir -text $dir -data $dir -open 0 \
                    -image [Bitmap::get folder] -drawcross auto
            }
            set path [file normalize [file join "$location_path" "$dir" "$element"]]
            foreach fp [ lsort -dictionary [glob -nocomplain -directory $path *] ]  {
                set file [file tail $fp]
                $tree insert end ms_$dir $file@$dir -text $file -data $file \
                    -image [Bitmap::get file] -drawcross never
            }
        }
    }

    # vector symbol selection subroutine
    if {$element == "symbol"} {
        $tree insert end root ms_$sympath -text SYMBOLS -data $sympath -open 1 \
            -image [Bitmap::get openfold] -drawcross auto
        
        foreach ic_dir [ lsort -dictionary [glob -nocomplain -directory $sympath *] ]  {
            set dir_tail [file tail $ic_dir]
            $tree insert end ms_$sympath ms_$dir_tail  -text $dir_tail -data $dir_tail \
                -image [Bitmap::get folder] -drawcross auto
    
            foreach ic_file [ lsort -dictionary [glob -nocomplain -directory [file join "$sympath" "$dir_tail"] *] ]  {
                set file [file tail $ic_file]
                $tree insert end ms_$dir_tail [file join "$dir_tail" "$file"] -text $file -data $file \
                    -image [Bitmap::get file] -drawcross never
            }
        }
    }

    $tree configure -redraw 1

    # buttons
    button $selwin($id,self).ok -text [G_msg "Ok"] -command "destroy $selwin($id,self)"
    button $selwin($id,self).cancel -text [G_msg "Cancel"] -command "GSelect_::terminate $id"

    # ScrollView
    toplevel $selftop -relief raised -borderwidth 2
    wm protocol $selftop WM_DELETE_WINDOW {
        # don't kill me
    }
    wm overrideredirect $selftop 1
    wm withdraw $selftop
    wm transient $selftop $selwin($id,self)
    ScrollView $selftop.sv -window $tree -fill black

    pack $sw   -side top  -expand yes -fill both
    pack $tree  -side top -expand yes -fill both 
    pack $selwin($id,self).ok $selwin($id,self).cancel -side left -expand yes
    pack $selftop.sv -fill both -expand yes

    wm protocol $selwin($id,self) WM_DELETE_WINDOW "GSelect_::terminate $id"
    tkwait window $selwin($id,self)

    destroy $selftop 

    # return selected elements -- separated by commas if there are > 1 elements
    if { $selwin($id,selected) != "" } {
        set ret ""
        set len [llength $selwin($id,selected)]
        foreach elem $selwin($id,selected) {
            append ret $elem
            if {[lsearch $selwin($id,selected) $elem] != -1  && \
                [lsearch $selwin($id,selected) $elem] < [expr $len-1]} {
                append ret ","
            }
        }
        return $ret
    }

    return ""
}


proc GSelect_::select { id tree node } {
    # Single selection (default). Clicking an item will select it and 
    # deselect any other item selected
    variable selwin
 
    set parent [$tree parent $node]
    if { $parent == "root" } { return }
 
    $tree selection set $node
    update
    set selwin($id,selected) $node
}

proc GSelect_::select_toggle { id tree node} {
    # Multiple selections. Ctrl-1 will toggle an item as selected or not selected
    # and add it to a list of selected items
    variable selwin
 
    set parent [$tree parent $node]
    if { $parent == "root" } { return }
 
    if {[lsearch -exact [$tree selection get] $node] >= 0} {
        $tree selection remove $node
        update
        set nodeindex [lsearch $selwin($id,selected) $node]
        if {$nodeindex != -1} {
            set selwin($id,selected) [lreplace $selwin($id,selected) $nodeindex $nodeindex]
        }
    } else {
        $tree selection add $node
        update
        lappend selwin($id,selected) $node
    }
     
    #$tree selection add $node
#     set selwin($id,selected) [string trim $selwin($id,selected) ,]
}

proc GSelect_::selectclose { id tree node } {
    # return selection and close window (OK button)
    variable selwin

    GSelect_::select $id $tree $node
    destroy $selwin($id,self)
}


proc GSelect_::terminate { id } {
    # close window without returning selection (cancel)
	variable selwin
	
	set selwin($id,selected) {}
	destroy $selwin($id,self)
}

proc GSelect_::moddir { idx tree node } {
    if { $idx && [$tree itemcget $node -drawcross] == "always" } {
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


