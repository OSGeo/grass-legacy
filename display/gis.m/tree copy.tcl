
namespace eval GmTree {
    variable count
    variable selected ""
    variable dblclick
    variable legend_height 20
    variable legend_width 30
	variable array tree # mon

}


# redraw tree
proc GmTree::redraw { mon } {
    variable tree
  
    Tree::_update_scrollregion  $tree($mon) 
}

# create new layer tree
proc GmTree::create { mon } {
	variable tree
    variable legend_height
    variable legend_width
	global tree_pane
	global sw

        
	destroy $tree_pane.sw
    
    set sw    [ScrolledWindow $tree_pane.sw \
                  -relief sunken -borderwidth 2 ]

    set lw [expr $legend_width + 27]
    set lh [expr $legend_height + 6]
	
    set tree($mon)  [Tree $sw.tree_$mon \
            -relief flat -borderwidth 0 -width 15 -highlightthickness 0 \
            -redraw 1 -dropenabled 1 -dragenabled 1 \
            -dragevent 1 -dropcmd "GmTree::drop" \
            -opencmd   "GmTree::open $sw.tree" \
            -closecmd  "GmTree::close $sw.tree" \
            -deltay $lh -padx $lw \
            -width 50 ]
            
	$tree($mon) configure -height 6
    $sw setwidget $tree($mon)
	
    pack $sw  -side top -expand yes -fill both
    pack $tree($mon)  -side top -expand yes -fill both

    $tree($mon) bindText  <ButtonPress-1> "GmTree::select $tree($mon)"
    $tree($mon) bindImage <ButtonPress-1> "GmTree::select $tree($mon)"
    $tree($mon) bindText  <Double-ButtonPress-1> "GmTree::edit $tree($mon)"

    $tree($mon) configure -redraw 1
    
    return $tree($mon)

}

	
# ScrollView
proc GmTree::scrollview { mon } {
	variable tree

    toplevel .top -relief raised -borderwidth 2
    wm protocol .top WM_DELETE_WINDOW {
        # don't kill me
    }
    wm overrideredirect .top 1
    wm withdraw .top
    wm transient .top .
    ScrollView .top.sv -window $tree($mon) -fill black
    pack .top.sv -fill both -expand yes

}

# make tree
proc GmTree::maketree { mon } {
	variable tree
	global sw
	
	puts "before tree $tree($mon)"

    
    $sw setwidget $tree($mon)
	
    pack $sw  -side top -expand yes -fill both
    pack $tree($mon)  -side top -expand yes -fill both
    
    puts "after tree $tree($mon)"
    
    set treemon $tree$(mon)

	return $treemon


}


proc GmTree::drop { from to where operation type data } {
    variable tree
    global mon


    set old_parent [$from parent $data]
    set old_index [$from index $data]
    if { [lindex $where 0] == "position" } { 
        set new_parent [lindex $where 1]
        set new_index [lindex $where 2]
    } elseif { [lindex $where 0] == "widget" } {
        set new_parent "root"
        set new_index [llength [$from nodes "root"] ]
    } else {
        set node [lindex $where 1]
        if { [Gm::node_type $node] == "group" } {
	    set new_parent $node
	    set new_index 0
       } else { 
	    set new_parent [$from parent $node]
	    set new_index [$from index $node]
	    incr new_index
       }
    }

    # test if new is not in childrens
    set parent $new_parent
    while { $parent != "root" } {
        if { $parent == $data } { return }
	set parent [$from parent $parent]
    }

    if { ($old_parent == $new_parent) && ($new_index > $old_index) } { 
        set new_index [expr $new_index - 1]
    }

    $from move $new_parent $data $new_index

}

proc GmTree::open { tree node } {
    global mon

    GmGroup::open [Gm::node_id $node]
}

proc GmTree::close { tree node } {
    global mon

    GmGroup::close [Gm::node_id $node]
}

proc GmTree::select { tree node } {
    variable selected 
    global mon

    if { $selected == $node } {
        $tree selection clear $node
        set selected ""
        Gm::deselect $node
    } else {
        $tree selection set $node
        update
        set selected $node
        Gm::select $node
    }

}

proc GmTree::edit { tree node } {
    global mon

    set res [$tree edit $node [$tree itemcget $node -text]]
    if { $res != "" } {
	$tree($mon) itemconfigure $node -text $res
    }
}

proc GmTree::autoname { tree node name} {
    global mon

	$tree($mon) itemconfigure $node -text $name
}
