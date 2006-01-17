
namespace eval GmGroup {
    variable array opt 
    variable count 1 
    variable array tree # mon
}


proc GmGroup::create { tree parent } {
    variable opt
    variable count 

    set fon [font create -size 10]
    set frm [ frame .groupicon$count ]
    set check [checkbutton $frm.check -text "" -font $fon \
                           -variable GmGroup::opt($count,_check) \
                           -height 1]
    set image [label $frm.image -image [Bitmap::get folder] ]
    pack $check $image -side left

    set node "group:$count"
    $tree insert end $parent $node \
	-text      "group $count" \
	-window    $frm \
	-drawcross auto \
        -open 1
	
    set opt($count,_check) 1
    set opt($count,treeimagepath) $image

    incr count

    return $node
}

proc GmGroup::save { tree depth node } {
    variable opt

    if { $node != "root" } {
	set id [GmTree::node_id $node] 
       GmTree::rc_write $depth _check $opt($id,_check)
    }

    foreach n [$tree nodes $node] {
        GmTree::save_node $depth $n
    }

}

proc GmGroup::display { node } {
    variable opt
    variable tree
	global mon

    set tree($mon) $GmTree::tree($mon)
    if { $node != "root" } {
	set id [GmTree::node_id $node] 
        if { ! ( $opt($id,_check) ) } { return }
    }

    foreach n [$tree($mon) nodes $node] {
        GmTree::display_node $n
    }

}

proc GmGroup::print { file  node } {
    variable opt
    global raster_printed

    set tree $GmTree::tree
    if { $node != "root" } {
	set id [GmTree::node_id $node] 
        if { ! ( $opt($id,_check) ) } { return }
    } else {
        set raster_printed 0
    }

    set lst ""
    foreach n [$tree nodes $node] {
        set lst [ concat $n $lst ]
    }

    foreach n $lst {
        GmTree::print_node $file $n
    }

}

proc GmGroup::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
 
    set opt($id,$key) $value
}

proc GmGroup::open { id } {
    variable opt


    $GmGroup::opt($id,treeimagepath) configure -image [Bitmap::get openfold]

}

proc GmGroup::close { id } {
    variable opt

    $GmGroup::opt($id,treeimagepath) configure -image [Bitmap::get folder]
}

proc GmGroup::duplicate { tree parent_node sel id } {
    puts "Duplicate for Groups not yet implemented."
}
