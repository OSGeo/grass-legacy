#!/bin/sh
# the next line restarts using wish \
exec $GRASS_WISH "$0" "$@"

lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1

set dmpath $env(GISBASE)/etc/dm/

source $dmpath/cmd.tcl
source $dmpath/group.tcl
source $dmpath/raster.tcl
source $dmpath/select.tcl
source $dmpath/tool.tcl
source $dmpath/tree.tcl
source $dmpath/vector.tcl

namespace eval Dm {
    variable mainframe
    variable options
    variable status
    variable prgtext
    variable prgindic
    variable max_prgindic 20
    variable tree
    variable rcfile
}

proc Dm::create { } {
    global dmpath
    variable mainframe
    variable options
    variable tree
    variable prgtext
    variable prgindic

    set prgtext "Loading Dm"
    set prgindic -1
    _create_intro
    update
    
    # eval "exec sleep 20"

    # Menu description
    set descmenu {
        "&File" all file 0 {
            {command "E&xit" {} "Exit BWidget demo" {} -command exit}
        }
        "&Options" all options 0 {
        }
    }

    set prgtext   "Creating MainFrame..."
    set mainframe [MainFrame .mainframe \
                       -menu         $descmenu \
                       -textvariable Dm::status \
                       -progressvar  Dm::prgindic ]

    set mainwindow [$mainframe getframe]

    # toolbar 1 creation
    set tb1  [$mainframe addtoolbar]
    DmToolBar::create $tb1

    set pw [PanedWindow $mainwindow.pw -side left ]
   
    pack $pw -side top -expand yes -fill both -anchor n 

    # tree 
    set tree_pane  [$pw add -minsize 50 -weight 1]
    set tree [ DmTree::create $tree_pane]
    pack $tree_pane -side top -expand yes -fill both

    # options
    set options_pane  [$pw add -minsize 50 -weight 1]
    set options_sw [ScrolledWindow $options_pane.sw -relief sunken -borderwidth 2]
    set options_sf [ ScrollableFrame $options_sw.sf]
    $options_sf configure -height 150
    $options_sw setwidget $options_sf
    set options [$options_sf getframe]
    pack $options_pane -expand yes -fill both 
    pack $options_sw $options_sf -fill both -expand yes
 
    set prgtext   "Done"

    set Dm::status "Welcome to Display manager"
    $mainframe showstatusbar status 

    pack $mainframe -fill both -expand yes
 
    set fon [font create -family Times -size 16]
    DynamicHelp::configure -font $fon -background yellow

    update idletasks
}

proc Dm::_create_intro { } {
    global dmpath
    variable max_prgindic

    set top [toplevel .intro -relief raised -borderwidth 2]

    wm withdraw $top
    wm overrideredirect $top 1

    set ximg  [label $top.x -image [image create photo -file "$dmpath/intro.gif"] ]

    set frame [frame $ximg.f -background white]
    set lab1  [label $frame.lab1 -text "GRASS 5.1 - Display Manager" \
                     -background white -foreground black -font {times 16}]
    set lab2  [label $frame.lab2 -textvariable Dm::prgtext -background white -font {times 12} -width 35]
    set prg   [ProgressBar $frame.prg -width 50 -height 15 -background white \
                   -variable Dm::prgindic -maximum $max_prgindic]
    pack $lab1 $prg $lab2 -side left
    place $frame -x 0 -y 0 -anchor nw
    pack $ximg
    BWidget::place $top 0 0 center
    wm deiconify $top
}

# create new empty
proc Dm::new { } {
    variable tree
    variable options
    
    $tree delete [$tree nodes root]
    destroy $options.fr
}

# add new group/layer to tree
proc Dm::add { type } {
    variable tree

    # selected node
    set parent_node [ lindex [$tree selection get] 0 ]
    if { $parent_node == "" } {
       set parent_node root
    } 

    set parent_type [Dm::node_type $parent_node]
    if { $parent_type != "group" } {
        set parent_node [$tree parent $parent_node]
    }
    

    switch $type {
        group {
            DmGroup::create $tree $parent_node
        }
        raster {
            DmRaster::create $tree $parent_node
        }
        vector {
            DmVector::create $tree $parent_node
        }
        cmd {
            DmCmd::create $tree $parent_node
        }
    }
}

# selected node ( show options )
proc Dm::select { node } {
    variable tree
    variable options

    set type [Dm::node_type $node]
    set id [Dm::node_id $node]

    # destroy old panel with options
    destroy $options.fr
 
    set opt [frame $options.fr ]
    pack $opt -fill both -expand yes

    switch $type {
        raster {
            DmRaster::options $id $opt
        }
        vector {
            DmVector::options $id $opt
        }
        cmd {
            DmCmd::options $id $opt
        }
    }
}

# deselect ( hide options )
proc Dm::deselect { node } {
    variable tree
    variable options

    destroy $options.fr
}

# delete selected node
proc Dm::delete { } {
    variable tree
    variable options

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } { return }

    $tree delete $sel
    destroy $options.fr
}

# open monitor if no one is runnning
proc Dm::monitor { } {
    if ![catch {open "|d.mon -L" r} input] {
        while {[gets $input line] >= 0} {
            if {[regexp -nocase {(x.).*display *running} $line buffer monitor]} {
                return
            }
        }
    }
    Dm::execute "d.mon start=x0"
}

# display
proc Dm::display { } {

    Dm::monitor
    Dm::execute "d.erase"
    DmGroup::display "root"
}

# display all
proc Dm::displayall { } {
    
    set cmd "g.region -d"
    Dm::execute $cmd 

    Dm::display
}

# zoom
proc Dm::zoom { } {
    
    set cmd "d.zoom"
    Dm::execute $cmd 

}

# display node
proc Dm::display_node { node } {
    variable tree

    set type [Dm::node_type $node]

    switch $type {
        group {
            DmGroup::display $node
	}
	raster {
	    DmRaster::display $node
	}
	vector {
	    DmVector::display $node
	}
	cmd {
	    DmCmd::display $node
	}
    } 
}

# query selected map
proc Dm::query { } {
    variable tree
    variable options

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } { return }

    set type [Dm::node_type $sel]

    switch $type {
        raster {
            DmVector::query $sel
        }
        vector {
            DmVector::query $sel
        }
        cmd {
            DmCmd::query $sel
        }
    }

}

# save tree/options to file
proc Dm::save { } {
    global env
    variable rcfile
    variable tree

    set fpath "$env(GISDBASE)/$env(LOCATION_NAME)/$env(MAPSET)/.dmrc"
    set rcfile [open $fpath w]

    DmGroup::save $tree 0 "root"

    close $rcfile
}

# save node to file
proc Dm::save_node { depth node } {
    variable rcfile
    variable tree

    set type [Dm::node_type $node]
    set name [$tree itemcget $node -text]

    switch $type {
        group {
            Dm::rc_write $depth Group $name
            incr depth
            DmGroup::save $tree $depth $node
	}
	raster {
            Dm::rc_write $depth Raster $name
            incr depth
	    DmRaster::save $tree $depth $node
	}
	vector {
            Dm::rc_write $depth Vector $name
            incr depth
	    DmVector::save $tree $depth $node
	}
	cmd {
            Dm::rc_write $depth Cmd $name
            incr depth
	    DmCmd::save $tree $depth $node
	}
    } 
    set depth [expr $depth - 1]
    Dm::rc_write $depth End
    
}

# load tree/options from file
proc Dm::load { } {
    global env
    variable rcfile
    variable tree
    variable max_prgindic
    variable prgtext

    set prgtext "Loading layers..."
    set fpath "$env(GISDBASE)/$env(LOCATION_NAME)/$env(MAPSET)/.dmrc"
    if { ![file exist $fpath] || ![file readable $fpath] } { return }
    set rcfile [open $fpath r]
    set file_size [file size $fpath]
    set nrows [expr $file_size / 15]

    set parent root
    set row 0
    while { [gets $rcfile in] > -1 } {
	set key ""
	set val ""
        set in [string trim $in " "] 
	if { $in == "" } { continue }

	if { ![regexp -- {([^ ]+) (.+)$} $in r key val] } {
           set key $in
        }
	
        switch $key {
            Group {
                set current_node [DmGroup::create $tree $parent]
                $tree itemconfigure $current_node -text $val 
                set parent $current_node
            }
            Raster {
                set current_node [DmRaster::create $tree $parent]
                $tree itemconfigure $current_node -text $val 
            }
            Vector {
                set current_node [DmVector::create $tree $parent]
                $tree itemconfigure $current_node -text $val 
            }
            Cmd {
                set current_node [DmCmd::create $tree $parent]
                $tree itemconfigure $current_node -text $val 
            }
            End {
                set type [Dm::node_type $current_node]
                if { $type == "group"  } {
                    set parent [$tree parent $parent]
                }
                set current_node [$tree parent $current_node]
            }
            default { 
                set type [Dm::node_type $current_node]
                switch $type {
                    group { 
                        DmGroup::set_option $current_node $key $val
                    }
                    raster { 
                        DmRaster::set_option $current_node $key $val
                    }
                    vector { 
                        DmVector::set_option $current_node $key $val
                    }
                    cmd { 
                        DmCmd::set_option $current_node $key $val
                    }
                }

            }           
        }
        incr row
        set prg [expr $max_prgindic * $row / $nrows]
        if { $prg > $max_prgindic } { set prg $max_prgindic }
        set Dm::prgindic $prg
    }
    close $rcfile
    set Dm::prgindic $max_prgindic
    set prgtext "Layers loaded"
}

# write one row
proc Dm::rc_write { depth args } {
    variable rcfile

    set offset [string repeat "  " $depth]

    set key [lindex $args 0]
    if { [llength $args] > 1 } {
       set value [lindex $args 1]
       set row "$offset$key $value"
    } else {
       set row "$offset$key"
    }
    puts $rcfile $row
}

# returnes node type
proc Dm::node_type { node } {
    variable tree

    if { [string compare $node "root"] == 0 } {
       return "group"
    }  
    if { [string compare -length 5 $node "group"] == 0 } {
       return "group"
    }  
    if { [string compare -length 6 $node "raster"] == 0 } {
       return "raster"
    }  
    if { [string compare -length 6 $node "vector"] == 0 } {
       return "vector"
    }  
    if { [string compare -length 3 $node "cmd"] == 0 } {
       return "cmd"
    }  
    
    return ""
}

# returnes node id
proc Dm::node_id { node } {
    variable tree

    if { ![regexp {[^:]+:(.+)$} $node x id] } {
        return 0
    } else {
        return $id
    }
}

# execute command
proc Dm::execute { cmd } {
    global env

    #puts stdout $cmd
    # warning: DBMI - be careful and test 'd.vect where=' after changes,
    # there are some pipes used there
    #eval "exec >@stdout 2>@stdout $cmd"
    set shell $env(SHELL)
    set cmd [ string map { \" \\\" \$ \\\$ } $cmd ]
    #puts stdout $cmd
    eval "exec echo \"$cmd\" >@stdout "
    eval "exec echo \"$cmd\" | $shell >@stdout 2>@stdout"
}

proc main {} {
    global auto_path

    wm withdraw .
    wm title . "GRASS 5.1 Display Manager"

    Dm::create
    Dm::load
    BWidget::place . 0 0 center
    wm deiconify .
    raise .
    focus -force .
    destroy .intro
}

main
wm geom . [wm geom .]

