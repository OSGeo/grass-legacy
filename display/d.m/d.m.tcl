##########################################################################
#
# d.m.tcl
#
# Primary tcltk script for GIS Manager: GUI for GRASS 6 
# Author: Michael Barton (Arizona State University)
# Based on Display Manager for GRASS 5.7 by Radim Blazek (ITC-IRST)
# and tcltkgrass for GRASS 5.7 by Michael Barton (Arizona State University)--
# with contributions by Glynn Clements, Markus Neteler, Lorenzo Moretti, 
# Florian Goessman, and others
#
# COPYRIGHT:	(C) 1999 - 2005 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1

set env(GISDBASE) [exec g.gisenv get=GISDBASE]
set env(LOCATION_NAME) [exec g.gisenv get=LOCATION_NAME]
set env(MAPSET) [exec g.gisenv get=MAPSET]

set gisdbase [exec g.gisenv get=GISDBASE]
set location_name [exec g.gisenv get=LOCATION_NAME]
set mapset [exec g.gisenv get=MAPSET]

set dmpath $env(GISBASE)/etc/dm/

#fetch GRASS Version number:
set fp [open $env(GISBASE)/etc/VERSIONNUMBER r]
set GRASSVERSION [read -nonewline $fp]
close $fp

source $env(GISBASE)/etc/gtcltk/gmsg.tcl

source $env(GISBASE)/etc/gtcltk/select.tcl
source $env(GISBASE)/etc/gui.tcl

source $dmpath/cmd.tcl
source $dmpath/tree.tcl
source $dmpath/tool1.tcl
source $dmpath/tool2.tcl
source $dmpath/group.tcl
source $dmpath/vector.tcl
source $dmpath/raster.tcl
source $dmpath/labels.tcl
source $dmpath/print.tcl

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


###############################################################################
proc execute {cmd} {
    global dlg path

    set code [exec -- $cmd --tcltk]

    set path .dialog$dlg
    toplevel $path
    eval $code
}

###############################################################################

append regexp .* $env(GISBASE) {[^:]*}
regsub -- $regexp $env(PATH) "&:$env(GISBASE)/etc/dm/script" env(PATH)


###############################################################################
proc spawn {cmd args} {
    eval exec -- $cmd $args &
}


###############################################################################
proc run {cmd args} {
    eval exec -- $cmd $args >@ stdout 2>@ stderr
}


###############################################################################
proc term {cmd args} {
    global env
    eval exec -- xterm -e $env(GISBASE)/etc/grass-run.sh $cmd $args &
}


###############################################################################

proc read_moncap {} {
	global env moncap

	set file [open [file join $env(GISBASE) etc monitorcap] r]
	set data [read $file]
	close $file

	set data [subst -nocommands -novariables $data]
	set moncap {}
	foreach line [split $data \n] {
		if {[string match {\#*} $line]} continue
		if {![string match {*:*:*:*:*:*} $line]} continue
		set fields {}
		foreach field [split $line :] {
			lappend fields [string trim $field]
		}
		lappend moncap $fields
	}
}

###############################################################################

proc monitor_menu {op} {
	global moncap

	set submenu {}
	set last_driver {}
	foreach mon $moncap {
		set name [lindex $mon 0]
		set driver [lindex $mon 1]
		if {$last_driver != "" && $last_driver != $driver} {
			lappend submenu {separator}
		}
		set last_driver $driver
		lappend submenu [list command $name {} "" {} -command "run d.mon $op=$name"]	}

	return [list $submenu]
}

###############################################################################

read_moncap

proc Dm::create { } {
    global dmpath
    global mainwindow
    variable mainframe
    variable options
    variable tree
    variable prgtext
    variable prgindic

    set prgtext [G_msg "Loading Dm"]
    set prgindic -1
    _create_intro
    update
    
    # eval "exec sleep 20"

    source $dmpath/menu.tcl 

    set prgtext   "Creating MainFrame..."
    set mainframe [MainFrame .mainframe \
                       -menu $descmenu \
                       -textvariable Dm::status \
                       -progressvar  Dm::prgindic ]

    set mainwindow [$mainframe getframe]

    # toolbar 1 creation
    set tb1  [$mainframe addtoolbar]
    DmToolBar::create $tb1
    set pw [PanedWindow $mainwindow.pw -side right ]   
    pack $pw -side top -expand yes -fill both -anchor n 

    # toolbar 2 creation
    set tb1  [$mainframe addtoolbar]
    DmMonitorsel::create $tb1
    set pw2 [PanedWindow $mainwindow.pw2 -side right ]   
    pack $pw2 -side bottom -expand yes -fill both -anchor n 

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
 
    set prgtext   [G_msg "Done"]

    set Dm::status [G_msg "Welcome to GRASS GIS manager"]
    $mainframe showstatusbar status 

    pack $mainframe -fill both -expand yes
 
    set fon [font create -family Times -size 16]
    DynamicHelp::configure -font $fon -background yellow

    update idletasks
}

proc Dm::_create_intro { } {
    global dmpath
    global GRASSVERSION
    global location_name
    variable max_prgindic

    set top [toplevel .intro -relief raised -borderwidth 2]

    wm withdraw $top
    wm overrideredirect $top 1

    set ximg  [label $top.x -image [image create photo -file "$dmpath/intro.gif"] ]

    set frame [frame $ximg.f -background white]
    set lab1  [label $frame.lab1 -text "GRASS $GRASSVERSION GIS Manager - $location_name" \
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
    global new_root_node
    
    $tree delete [$tree nodes root]
    destroy $options.fr

    catch {unset ::Dm::filename}
    DmPrint::init
    set new_root_node [DmGroup::create $tree "root"]
    $tree itemconfigure $new_root_node -text "UNTITLED"
    
    set ::Dm::filename Untitled.dmrc 
}

#Ctrl-W to close file
proc Dm::FileClose { stay_alive} {
    variable tree
    variable options
    
    $tree delete [$tree nodes root]
    destroy $options.fr

    if { $stay_alive == ""} {
    	catch {unset ::Dm::filename}
    }
}

# add new group/layer to tree
proc Dm::add { type } {
    variable tree
    global new_root_node

    if { [catch {match string {} $new_root_node}] } {
    set new_root_node root
    }
    # selected node
    set parent_node [ lindex [$tree selection get] 0 ]
    if { $parent_node == "" } {
       set parent_node $new_root_node
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
        labels {
            DmLabels::create $tree $parent_node
        }
        cmd {
            DmCmd::create $tree $parent_node
        }
    }
}

# autoname layer when a map is selected
proc Dm::autoname { name } {
	variable tree
	variable node
	set node [ lindex [$tree selection get] 0 ]
	DmTree::autoname $tree $node $name
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
        labels {
            DmLabels::options $id $opt
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
    run "d.mon start=x0"
}

#digitize
proc Dm::edit { } {
    variable tree
    variable options

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } {
        set message "No map selected"
        tk_messageBox -type ok -icon warning -message $message
        return
    }

    set type [Dm::node_type $sel]

    switch $type {
        raster {
        term r.digit $sel
            return
        }
        labels {
            return
        }
        vector {
	    DmVector::WorkOnVector $sel
        }
        cmd {
            return
        }
    }

}

# display
proc Dm::display { } {

    Dm::monitor
    run "d.frame -e"
#    execute "d.erase"
    DmGroup::display "root"
}

# display all
proc Dm::displayall { } {
    
    set cmd "g.region -d"
    run $cmd 

    Dm::display
}

# display region
proc Dm::display_region { } {
   
    set reg [GSelect windows]
    if { $reg != "" } {
	set cmd "g.region region=$reg"
	run $cmd 

	Dm::display
    }
}

# zoom
proc Dm::zoom { } {
    
    set cmd "d.zoom"
    term $cmd

}

# zoom back
proc Dm::zoom_back { } {
    
    set cmd "d.zoom -r"
    term $cmd

}

# pan
proc Dm::pan { } {
    
    set cmd "d.zoom -p"
    term $cmd 

}

# erase to white
proc Dm::erase { } {
    
#    set cmd "d.erase white"
    set cmd "d.frame -e"
    run $cmd 

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
	labels {
	    DmLabels::display $node
	}
	vector {
	    DmVector::display $node
	}
	cmd {
	    DmCmd::display $node
	}
    } 
}

# display node
proc Dm::print_node { file node } {
    variable tree
    global raster_printed

    set type [Dm::node_type $node]

    switch $type {
        group {
            DmGroup::print $file $node
	}
	raster {
            if { ! $raster_printed } { 
	        DmRaster::print $file $node
                set raster_printed 1
            }
	}
	labels {
	    DmLabels::print $file $node
	}
	vector {
	    DmVector::print $file $node
	}
	cmd {
            puts "Command may not be printed to postscript file"
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
            DmRaster::query $sel
        }
        labels {
            DmLabels::query $sel
        }
        vector {
            DmVector::query $sel
        }
        cmd {
            DmCmd::query $sel
        }
    }
}

# duplicate selected layer
proc Dm::duplicate { } {
    variable tree
    variable options
    variable id
    global new_root_node

    if { [catch {match string {} $new_root_node}] } {
    set new_root_node root
    }
    # selected node
    set parent_node [ lindex [$tree selection get] 0 ]
    if { $parent_node == "" } {
       set parent_node $new_root_node
    } 

    set parent_type [Dm::node_type $parent_node]
    if { $parent_type != "group" } {
        set parent_node [$tree parent $parent_node]
    }

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } { return }
    
    set type [Dm::node_type $sel]
    set id [Dm::node_id $sel]

    switch $type {
        raster {
            DmRaster::duplicate $tree $parent_node $sel $id
        }
        labels {
            DmLabels::duplicate $tree $parent_node $sel $id
        }
        vector {
            DmVector::duplicate $tree $parent_node $sel $id
        }
        cmd {
            DmCmd::duplicate $tree $parent_node $sel $id
        }
        group {
            DmGroup::duplicate $tree $parent_node $sel $id
        }
    }
}

# save tree/options to file
proc Dm::save { spth } {
    global gisdbase location_name mapset
    global env
    variable rcfile
    variable tree

    set fpath $spth
    set rcfile [open $fpath w]

    DmPrint::save
    DmGroup::save $tree 0 "root"

    close $rcfile
}

# save node to file
proc Dm::save_node { depth node } {
    variable rcfile
    variable tree

    set type [Dm::node_type $node]
    set name [$tree itemcget $node -text]

    if { $type == "group" && $name == "UNTITLED" } {
    set name "File $::Dm::filename"
    }

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
	labels {
            Dm::rc_write $depth Labels $name
            incr depth
	    DmLabels::save $tree $depth $node
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
proc Dm::load { lpth } {
    global gisdbase location_name mapset
    global env
    variable rcfile
    variable tree
    variable max_prgindic
    variable prgtext

    set prgtext "Loading layers..."

    set fpath $lpth

    if { ![file exist $fpath] || ![file readable $fpath] } { 
            return 
    }

    set rcfile [open $fpath r]
    set file_size [file size $fpath]
    set nrows [expr $file_size / 15]

    set print_section 0
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
        
        # Print
        if { $print_section } {
            if { $key == "End" } { 
                set print_section 0 
            } else {
	        DmPrint::set_option $key $val
            }
        } else {
            if { $key == "Print" } {
                 set print_section 1
	    } else {  
		# Tree of layers	
		switch $key {
		    Group {

			if { [regexp -- {^File (.+)} $val r leftover]  && ($leftover !=
			$::Dm::filename)} {
			    	set val "<-- $leftover"
			}

			set current_node [DmGroup::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
			set parent $current_node
		    }
		    Raster {
			set current_node [DmRaster::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    Labels {
			set current_node [DmLabels::create $tree $parent]
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
		      if {[catch {Dm::node_type $current_node}] } {
			tk_messageBox -type ok -message "Can't open $fpath - bad file format"
			break
		      } else {

			set type [Dm::node_type $current_node]
			switch $type {
			    group { 
				DmGroup::set_option $current_node $key $val
			    }
			    raster { 
				DmRaster::set_option $current_node $key $val
			    }
			    labels { 
				DmLabels::set_option $current_node $key $val
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
		}
	    }
	    incr row
	    set prg [expr $max_prgindic * $row / $nrows]
	    if { $prg > $max_prgindic } { set prg $max_prgindic }
	    set Dm::prgindic $prg
        }
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

# returns node type
proc Dm::node_type { node } {
    variable tree

    if { [string compare $node "root"] == 0 } {
       return "group"
    }  
    if { [string match group* $node] } {
       return "group"
    }  
    if { [string match raster* $node] } {
       return "raster"
    }  
    if { [string match labels* $node] } {
       return "labels"
    }  
    if { [string match vector* $node] } {
       return "vector"
    }  
    if { [string match cmd* $node] } {
       return "cmd"
    }  
    
    return ""
}

# returns node id
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

    # warning: DBMI - be careful and test 'd.vect where=' after changes
    puts stdout $cmd
    
      ## This was old version - does not work, because $shell have not LD_LIBRARY_PATH to GRASS libs ? 
      #set shell $env(SHELL)
      #set cmd [ string map { \" \\\" \$ \\\$ } $cmd ]
      #eval "exec echo \"$cmd\" | $shell >@stdout 2>@stdout"

    eval "exec $cmd >@stdout 2>@stdout"
}

# open print window
proc Dm::print { } {
    DmPrint::window
}

#open dialog box
proc Dm::OpenFileBox {w} {
    global mainwindow
    variable win

    set win $w
    
    if { $win == ""} {set win $mainwindow}
    
    set types {
        {{Adm Resource Files} {{.dm} {.dmrc}}}
        {{All Files} *}
    }

        if {[catch {tk_getOpenFile \
                -parent $win \
                -filetypes $types \
                -title {Load File}} \
                ::Dm::filename_new] || \
                [string match {} $::Dm::filename_new]} return
	
	if {[catch {if { [ regexp -- {^Untitled.dmrc$} $::Dm::filename r]} {}}] } {
		set ::Dm::filename $::Dm::filename_new
	}
	
	Dm::load $::Dm::filename_new
		
};

#save dialog box
proc Dm::SaveFileBox {w} {
    global mainwindow
    variable win

    set win $w
    if { $win == ""} {set win $mainwindow}
    catch {if { [ regexp -- {^Untitled.dmrc$} $::Dm::filename r]} {unset ::Dm::filename}}
    if {[catch {Dm::save $::Dm::filename}]} {
        set types {
            {{Adm Resource Files} {{.dm} {.dmrc}}}
            {{All Files} *}
        }
        if {[catch {tk_getSaveFile \
                -parent $win \
                -filetypes $types \
                -title {Save File}} \
                ::Dm::filename] || \
                [string match {} $::Dm::filename]} return
	
	Dm::save $::Dm::filename
	Dm::FileClose stay_alive
	Dm::load $::Dm::filename
    }
};

proc main {argc argv} {
    global auto_path
    global GRASSVERSION
    global location_name

    wm withdraw .
    wm title . [G_msg "GRASS $GRASSVERSION GIS Manager - $location_name"]

    bind . <Control-Key-o> {
	Dm::OpenFileBox {}
    }
    bind . <Control-Key-n> {
	Dm::new
    }
    bind . <Control-Key-s> {
	Dm::SaveFileBox {}
    }
    bind . <Control-Key-q> {
	DmPrint::clean;  exit
    }
    bind . <Control-Key-x> {
	Dm::delete
    }
    bind . <Control-Key-w> {
	Dm::FileClose {}
    }
    bind . <Control-Key-p> {
    Dm::print
    }

    Dm::create
    DmPrint::init
    DmPrint::init_tmpfiles
    BWidget::place . 0 0 center
    wm deiconify .
    raise .
    focus -force .
    destroy .intro
    
    if { $argc == "1"} { 
    	set ::Dm::filename $argv
	Dm::load $::Dm::filename
    }

}

bind . <Destroy> { if { "%W" == "."} { DmPrint::clean } } 
main $argc $argv
wm geom . [wm geom .]

