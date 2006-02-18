###############################################################
# group.tcl - group layer creation and management procedures 
# for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmGroup {
    variable array opt 
    variable count 1 
    variable array tree # mon
    variable nvelev ""
    variable nvcolor ""
    variable nvpoints ""
	variable nvlines ""
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
	global mon
	
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
	global drawprog

    set tree($mon) $GmTree::tree($mon)
    if { $node != "root" } {
		set id [GmTree::node_id $node] 
        if { ! ( $opt($id,_check) ) } { return }
    }

    foreach n [$tree($mon) nodes $node] {
        GmTree::display_node $n
        incr drawprog
    }

}

###############################################################################


proc GmGroup::nvdisplay { node } {
    variable opt
    variable tree
    variable nvelev 
    variable nvcolor
    variable nvpoints
	variable nvlines
	global mon
	global drawprog

    set tree($mon) $GmTree::tree($mon)
    if { $node != "root" } {
		set id [GmTree::node_id $node] 
        if { ! ( $opt($id,_check) ) } { return }
    }

    foreach n [$tree($mon) nodes $node] {
        GmGroup::nviz $n
        incr drawprog
    }

	if { $nvelev!= "" } {
		set cmd "nviz elevation=$nvelev color=$nvcolor"
		if {$nvlines != ""} {
			append cmd " vector=$nvlines"
		}
		if {$nvpoints != ""} {
			append cmd " points=$nvpoints"
		}
		
		append cmd " &"
		eval exec $cmd
	}

	set nvelev ""
	set nvcolor ""
	set nvlines ""
	set nvpoints ""
}


# display raster maps in NVIZ (base maps for elevation and drape maps for color)
proc GmGroup::nviz { node } {
    variable opt
    variable tree
    variable nvelev 
    variable nvcolor
    variable nvpoints
	variable nvlines
	global mon
	global drawprog
		
	#set id [GmTree::node_id $node] 

	set type [GmTree::node_type $node]

	
	switch $type {
		"group" {
			GmGroup::nvdisplay $node 
		}
		"raster" {
			if {$nvelev == "" } {
				set nvelev [GmRaster::addelev $node $nvelev]
			} else {
				append nvelev ",[GmRaster::addelev $node $nvelev]"
			}
				
			if {$nvcolor == "" } {
				set nvcolor [GmRaster::addcolor $node $nvcolor]
			} else {
				append nvcolor ",[GmRaster::addcolor $node $nvcolor]"
			}
		}
		"vector" {
			set vect [GmVector::addvect $node]	
			set vecttype [GmVector::vecttype $vect]
			if {$vecttype == "points"} {
				if {$nvpoints == "" } {
					set nvpoints $vect
				} else {
					append nvpoints ",$vect"
				}
			} else {
				if {$nvlines == "" } {
					set nvlines $vect
				} else {
					append nvlines ",$vect"
				}
			}
		}
	}
}


###############################################################################
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
