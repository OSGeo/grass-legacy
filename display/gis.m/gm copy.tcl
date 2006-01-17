##########################################################################
#
# gm.tcl
#
# Primary tcltk script for GIS Manager: GUI for GRASS 6 
# Author: Michael Barton (Arizona State University)
# Based on Display Manager for GRASS 5.7 by Radim Blazek (ITC-IRST)
# and tcltkgrass for GRASS 5.7 by Michael Barton (Arizona State University)--
# with contributions by Glynn Clements, Markus Neteler, Lorenzo Moretti, 
# Florian Goessmann, and others
#
# 12 November 2005
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


set gmpath $env(GISBASE)/etc/gm

set keycontrol "Control"
set tmenu "1"
set keyctrl "Ctrl"
set execom "execute"
set msg 0
set mon 0

if {[info exists env(HOSTTYPE)]} {
	set HOSTTYPE $env(HOSTTYPE)
} else {
	set HOSTTYPE ""
}

set bgcolor HoneyDew2

# add for OSX aqua
if {[info exists env(osxaqua)]} {
    set osxaqua $env(osxaqua)
} else {
    set osxaqua "0"
}

if { $osxaqua == "1"} {
    set keycontrol "Command"
    set tmenu "0"
    set keyctrl "Command"
    set execom "spawn"
}

#fetch GRASS Version number:
set fp [open $env(GISBASE)/etc/VERSIONNUMBER r]
set GRASSVERSION [read -nonewline $fp]
close $fp

source $env(GISBASE)/etc/gtcltk/gmsg.tcl

source $env(GISBASE)/etc/gtcltk/select.tcl
source $env(GISBASE)/etc/gui.tcl

source $gmpath/cmd.tcl
source $gmpath/tree.tcl
source $gmpath/tool1.tcl
source $gmpath/tool2.tcl
source $gmpath/group.tcl
source $gmpath/vector.tcl
source $gmpath/raster.tcl
source $gmpath/labels.tcl
source $gmpath/gridline.tcl
source $gmpath/rgbhis.tcl
source $gmpath/legend.tcl
source $gmpath/frames.tcl
source $gmpath/barscale.tcl
source $gmpath/chart.tcl
source $gmpath/thematic.tcl
source $gmpath/fttext.tcl
source $gmpath/dtext.tcl
source $gmpath/print.tcl
source $gmpath/cmonitor.tcl


namespace eval Gm {
    variable mainframe
    variable options
    variable status
    variable prgtext
    variable prgindic
    variable max_prgindic 20
    variable array tree # mon
    variable rcfile
}




################################################################################

proc execute {cmd} {
    global dlg path

    set code [exec -- $cmd --tcltk]

    set path .dialog$dlg
    toplevel $path
    eval $code
}

###############################################################################

append regexp .* $env(GISBASE) {[^:]*}
regsub -- $regexp $env(PATH) "&:$env(GISBASE)/etc/gm/script" env(PATH)


###############################################################################
proc spawn {cmd args} {
    eval exec -- $cmd $args &
}

###############################################################################
	proc cmd_output {fh} {
		global outtext
		while {![eof $fh]} {
			set str [gets $fh]
			if {[regexp -- {^GRASS_INFO_PERCENT: (.+)$} $str match val rest]} {
				#do nothing
			} else {
				append str "\n"
				if { [fblocked $fh] } { set str [read $fh] }
				while {[set idx [string first "\b" $str]] != -1} {
					set last [expr $idx - 1]
					set str1 [string range $str 1 $last]
					set first [expr $idx + 1]
					set str [string range $str $first end]
					set pos [$outtext index "end - 1 chars"]
					$outtext delete $pos
					$outtext insert end $str1
					update idletasks
				}
				$outtext insert end $str
				$outtext yview end
				update idletasks
			}
		}
		catch {close $fh }
		return
	}

###############################################################################

	proc run_panel {cmd} {
		global outtext
		global env
		set message_env [exec g.gisenv get=GRASS_MESSAGE_FORMAT]
		set env(GRASS_MESSAGE_FORMAT) gui
		set cmd_name $cmd
		set cmd [concat | $cmd 2>/dev/null ]
		if { [catch {open $cmd r } fh] } {
			error $fh
		}
		$outtext insert end "$cmd_name\n"
		$outtext yview end 
		catch {cmd_output $fh}
		
		set env(GRASS_MESSAGE_FORMAT) $message_env
	}


###############################################################################

	proc runcmd {cmd} {
		global outtext
		global env
		set cmd_name $cmd
	    eval exec -- $cmd >@ stdout 2>@ stderr
		$outtext insert end "$cmd_name\n"
		$outtext yview end 		
	}

###############################################################################
proc term_panel {cmd} {
	global outtext
    global env
    eval exec -- xterm -name xterm-grass -e $env(GISBASE)/etc/grass-run.sh $cmd &
    set str $cmd
	$outtext insert end "$cmd\n"
	$outtext yview end

	update idletasks
}

###############################################################################
proc run {cmd args} {
    eval exec -- $cmd $args >@ stdout 2>@ stderr

}



###############################################################################
proc term {cmd args} {
    global env
    eval exec -- xterm -name xterm-grass -e $env(GISBASE)/etc/grass-run.sh $cmd $args &
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

proc Gm::color { color } {
    
    regexp -- {#(..)(..)(..)} $color x r g b

    set r [expr 0x$r ]
    set g [expr 0x$g ]
    set b [expr 0x$b ]

    return "$r:$g:$b"
}


###############################################################################

proc Gm::setenv { } {
	global env
	global gmpath
	
	set env(GRASS_PNGFILE) "mon0.ppm"
	set env(GRASS_TRANSPARENT) "TRUE"
	set env(GRASS_PNG_AUTO_WRITE) "TRUE"
	set env(GRASS_TRUECOLOR) "TRUE"

    if ![catch {open "|d.mon -L" r} input] {
        while {[gets $input line] >= 0} {
            if {[regexp -nocase "PNG.*not running" $line]} {
				run "d.mon start=PNG" 
            } 
        }
    }

}


###############################################################################


proc Gm::create { } {
    global gmpath
    global mainwindow
    global bgcolor
    global cmd
    global outtext
    global mon
    global tree_pane
    
    variable mainframe
    variable options
    variable tree
    variable prgtext
    variable prgindic
	
    set prgtext [G_msg "Loading GIS Manager"]
    set prgindic -1
    _create_intro
    update
    
        
    # eval "exec sleep 20"

    global env
	source $gmpath/menu.tcl

    set prgtext   "Creating MainFrame..."
    set mainframe [MainFrame .mainframe \
                       -menu $descmenu -background $bgcolor \
                       -textvariable Gm::status \
                       -progressvar  Gm::prgindic ]

    set mainwindow [$mainframe getframe]

	# check for currently active monitor    
	if ![catch {open "|d.mon -L" r} input] {
		while {[gets $input line] >= 0} {
			if {[regexp -nocase {.*(selected).*} $line]} {
				regexp -nocase {..} $line mon
			}              
		}
	}


    # toolbar 1 & 2 creation
    set tb1  [$mainframe addtoolbar]
    GmToolBar1::create $tb1
    set tb2  [$mainframe addtoolbar]
    GmToolBar2::create $tb2
    set pw1 [PanedWindow $mainwindow.pw1 -side top -pad 0 -width 6 -background $bgcolor]
    pack $pw1 -side top -expand no -fill x -anchor n 
    set pw2 [PanedWindow $mainwindow.pw2 -side left -pad 0 -width 10 -background $bgcolor ]   
    pack $pw2 -side left -expand yes -fill both -anchor n 
    
   
    # tree 
    set treemon [expr $mon + 1]
    set tree_pane  [$pw1 add  -minsize 50 -weight 1]
#    set tree($mon) [ GmTree::create $mon ]
#    GmTree::scrollview $treemon
    pack $tree_pane -side right -expand yes -fill both

    # options
    set options_pane  [$pw2 add -minsize 50 -weight 1]
    set options_sw [ScrolledWindow $options_pane.sw -relief raised -borderwidth 1]
    set options_sf [ScrollableFrame $options_sw.sf]
    $options_sf configure -height 145 -width 500
    $options_sw setwidget $options_sf
    set options [$options_sf getframe]
    pack $options_pane -expand yes -fill both 
    pack $options_sw $options_sf -fill both -expand yes
 
    # output
    set output_pane  [$pw2 add -minsize 50 -weight 2 ]
    pack $output_pane -expand yes -fill both 
    pack $pw2 -fill both -expand yes

    set output_sw [ScrolledWindow $output_pane.win -relief sunken -borderwidth 1]
	set outtext [text $output_sw.text -height 5 -width 30] 
	$output_sw setwidget $outtext
    pack $output_sw $outtext -fill both -expand yes
  

    set prgtext   [G_msg "Done"]

    set Gm::status [G_msg "Welcome to the GRASS GIS manager"]
    $mainframe showstatusbar status 

    pack $mainframe -fill both -expand yes
 
    set fon [font create -family Verdana -size 12 ]
    DynamicHelp::configure -font $fon -background yellow

	cmon::startmon
    update idletasks
}


###############################################################################

# start new display monitor and increment canvas monitor number
proc cmon::startmon { } {
	global mainwindow
	global mon
	variable tree

	Gm::increment_mon
	puts "mon in startmon $mon"
	cmon::create
	set tree($mon) [GmTree::create $mon]
	
}



# increment display number
proc Gm::increment_mon { } {
	global mon
	
	puts "mon before increment $mon"
	set mon [expr $mon + 1]
	puts "mon after incrementing $mon"
	
}	

###############################################################################

proc Gm::_create_intro { } {
    global gmpath
    global GRASSVERSION
    global location_name
    variable max_prgindic

    set top [toplevel .intro -relief raised -borderwidth 2]

    wm withdraw $top
    wm overrideredirect $top 1

    set ximg  [label $top.x -image [image create photo -file "$gmpath/intro.gif"] ]

    set frame [frame $ximg.f -background white]
    set lab1  [label $frame.lab1 -text "GRASS$GRASSVERSION GIS Manager - $location_name" \
                     -background white -foreground black -font {times 16}]
    set lab2  [label $frame.lab2 -textvariable Gm::prgtext -background white -font {times 12} -width 35]
    set prg   [ProgressBar $frame.prg -width 50 -height 15 -background white \
                   -variable Gm::prgindic -maximum $max_prgindic]
    pack $lab1 $prg $lab2 -side left
    place $frame -x 0 -y 0 -anchor nw
    pack $ximg
    BWidget::place $top 0 0 center
    wm deiconify $top
}

###############################################################################

# create new empty
proc Gm::new { } {
    variable tree
    variable options
    global new_root_node
    global mon
    
    $tree delete [$tree nodes root]
    destroy $options.fr

    catch {unset ::Gm::filename}
    GmPrint::init
    set new_root_node [GmGroup::create $tree "root"]
    $tree itemconfigure $new_root_node -text "UNTITLED"
    
    set ::Gm::filename Untitled.dmrc 
}

###############################################################################

#Ctrl-W to close file
proc Gm::FileClose { stay_alive} {
    variable tree
    variable options
 	global mon
 	
    $tree delete [$tree nodes root]
    destroy $options.fr

    if { $stay_alive == ""} {
    	catch {unset ::Gm::filename}
    }
}

###############################################################################

# add new group/layer to tree
proc Gm::add { type } {
	variable tree
    global new_root_node
    global mon

    if { [catch {match string {} $new_root_node}] } {
    set new_root_node root
    }
    
    puts "this is tree at gm add $tree($mon)"
    # selected node
    set parent_node [ lindex [$tree($mon) selection get] 0 ]
    if { $parent_node == "" } {
       set parent_node $new_root_node
    } 

    set parent_type [Gm::node_type $parent_node]
    if { $parent_type != "group" } {
        set parent_node [$tree($mon) parent $parent_node]
    }

    switch $type {
        group {
            GmGroup::create $tree $parent_node
        }
        raster {
            GmRaster::create $tree($mon) $parent_node
        }
        vector {
            GmVector::create $tree $parent_node
        }
        labels {
            GmLabels::create $tree $parent_node
        }
        cmd {
            GmCmd::create $tree $parent_node
        }
        gridline {
            GmGridline::create $tree $parent_node
        }
        rgbhis {
            GmRgbhis::create $tree $parent_node
        }
        legend {
            GmLegend::create $tree $parent_node
        }
        dframe {
            GmDframe::create $tree $parent_node
        }
        barscale {
            GmBarscale::create $tree $parent_node
        }
        chart {
            GmChart::create $tree $parent_node
        }
        thematic {
            GmThematic::create $tree $parent_node
        }
        fttext {
            GmFTtext::create $tree $parent_node
        }
        dtext {
            GmDtext::create $tree $parent_node
        }
    }
}

###############################################################################

# autoname layer when a map is selected
proc Gm::autoname { name } {
    variable tree
    variable node
    global mon
    
    set node [ lindex [$tree($mon) selection get] 0 ]
    GmTree::autoname $tree($mon) $node $name
}

###############################################################################
# selected node ( show options )
proc Gm::select { node } {
    variable tree
    variable options
	global mon
	
    set type [Gm::node_type $node]
    set id [Gm::node_id $node]

    # destroy old panel with options
    destroy $options.fr
 
    set opt [frame $options.fr ]
    pack $opt -fill both -expand yes

    switch $type {
        raster {
            GmRaster::options $id $opt
        }
        vector {
            GmVector::options $id $opt
        }
        labels {
            GmLabels::options $id $opt
        }
        cmd {
            GmCmd::options $id $opt
        }
        gridline {
            GmGridline::options $id $opt
        }
        rgbhis {
            GmRgbhis::options $id $opt
        }
        legend {
            GmLegend::options $id $opt
        }
        dframe {
            GmDframe::options $id $opt
        }
        barscale {
            GmBarscale::options $id $opt
        }
        chart {
            GmChart::options $id $opt
        }
        thematic {
            GmThematic::options $id $opt
        }
        fttext {
            GmFTtext::options $id $opt
        }
        dtext {
            GmDtext::options $id $opt
        }
    }
}

###############################################################################

# deselect ( hide options )
proc Gm::deselect { node } {
    variable tree
    variable options
	global mon
	
    destroy $options.fr
}

###############################################################################

# delete selected node
proc Gm::delete { } {
    variable tree
    variable options
    global mon

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } { return }

    $tree delete $sel
    destroy $options.fr
}


###############################################################################

# open monitor if no one is runnning
proc Gm::monitor { } {
    if ![catch {open "|d.mon -L" r} input] {
        while {[gets $input line] >= 0} {
            if {[regexp -nocase {(x.).*display *running} $line buffer monitor]} {
                return
            }
        }
    }
    runcmd d.mon start=x0"
    return
}

###############################################################################

#digitize
proc Gm::edit { } {
    variable tree
    variable options
    global mon

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } {
        set message "No map selected"
        tk_messageBox -type ok -icon warning -message $message
        return
    }

    set type [Gm::node_type $sel]

    switch $type {
        raster {
        term r.digit $sel
            return
        }
        labels {
            return
        }
        vector {
	    GmVector::WorkOnVector $sel
        }
        cmd {
            return
        }
        gridline {
            return
        }
        rgbhis {
            return
        }
        legend {
            return
        }
        dframe {
            return
        }
        barscale {
            return
        }
        chart {
            return
        }
        thematic {
            return
        }
        fttext {
            return
        }
        dtext {
            return
        }
    }

}

###############################################################################
proc message_dialog { msgtxt } {
	set msg [MessageDlg .msgdlg  \
		-title "The Message Dialog" \
		-message $msgtxt \
		-type yesno ]
if { $msg == 1 } {puts "no way" }
if { $msg == 0 } {puts "way to go"}

}


###############################################################################

# zoom
proc Gm::zoom { } {
    
    set cmd "d.zoom"
    term $cmd

}

###############################################################################

# zoom back
proc Gm::zoom_back { } {
    
    set cmd "d.zoom -r"
    term $cmd

}

###############################################################################

# pan
proc Gm::pan { } {
    
    set cmd "d.zoom -p"
    term $cmd 

}


###############################################################################

# measure
proc Gm::measure { } {
    
    set cmd "d.measure"
    term $cmd 

}

###############################################################################

# position
proc Gm::position { } {
    
    set cmd "d.where"
    term $cmd 

}

###############################################################################

# nviz
proc Gm::nviz { } {
global osxaqua
global HOSTTYPE
    
    set cmd "nviz"
	if { $HOSTTYPE == "macintosh" || $HOSTTYPE == "powermac" || $HOSTTYPE == "powerpc"} {
		if { $osxaqua == "1"} {
			spawn $cmd
		} else {
			term $cmd
		}
	} else {
		spawn $cmd
	}

}

###############################################################################

# fly
proc Gm::fly { } {
    
    set cmd "d.nviz"
    eval exec $cmd &

}

###############################################################################

# xganim
proc Gm::xganim { } {
    
    set cmd "xganim"
    eval exec $cmd &

}

###############################################################################

# help
proc Gm::help { } {
    
    set cmd "g.manual"
    term $cmd 

}

###############################################################################

# display node
proc Gm::display_node { node } {
    variable tree

    set type [Gm::node_type $node]

    switch $type {
        group {
            GmGroup::display $node
	}
	raster {
	    GmRaster::display $node
	}
	labels {
	    GmLabels::display $node
	}
	vector {
	    GmVector::display $node
	}
	cmd {
	    GmCmd::display $node
	}
	gridline {
	    GmGridline::display $node
	}
	rgbhis {
	    GmRgbhis::display $node
	}
	legend {
	    GmLegend::display $node
	}
	dframe {
	    GmDframe::display $node
	}
	barscale {
	    GmBarscale::display $node
	}
	chart {
	    GmChart::display $node
	}
	thematic {
	    GmThematic::display $node
	}
	fttext {
	    GmFTtext::display $node
	}
	dtext {
	    GmDtext::display $node
	}
    } 
}

###############################################################################

# display node
proc Gm::print_node { file node } {
    variable tree
    global raster_printed

    set type [Gm::node_type $node]

    switch $type {
        group {
            GmGroup::print $file $node
	}
	raster {
            if { ! $raster_printed } { 
	        GmRaster::print $file $node
                set raster_printed 1
            }
	}
	labels {
	    GmLabels::print $file $node
	}
	vector {
	    GmVector::print $file $node
	}
	cmd {
            puts "Command may not be printed to postscript file"
	}
	gridline {
            puts "not be printed to postscript file"
	}
	rgbhis {
            puts "not be printed to postscript file"
	}
	legend {
            puts "not be printed to postscript file"
	}
	dframe {
            puts "not be printed to postscript file"
	}
	barscale {
            puts "not be printed to postscript file"
	}
	chart {
            puts "not be printed to postscript file"
	}
	thematic {
            puts "not be printed to postscript file"
	}
	fttext {
            puts "not be printed to postscript file"
	}
	dtext {
            puts "not be printed to postscript file"
	}
    } 
}

###############################################################################

# query selected map
proc Gm::query { } {
    variable tree
    variable options
    global mon

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } { return }

    set type [Gm::node_type $sel]

    switch $type {
        raster {
            GmRaster::query $sel
        }
        labels {
            GmLabels::query $sel
        }
        vector {
            GmVector::query $sel
        }
        cmd {
            GmCmd::query $sel
        }
        gridline {
            return
        }
        rgbhis {
            GmRgbhis::query $sel
            #return
        }
        legend {
            return
        }
        dframe {
            return
        }
        barscale {
            return
        }
        chart {
            return
        }
        thematic {
            return
        }
        fttext {
            return
        }
        dtext {
            return
        }
    }
}

###############################################################################

# duplicate selected layer
proc Gm::duplicate { } {
    variable tree
    variable options
    variable id
    global new_root_node mon

    if { [catch {match string {} $new_root_node}] } {
    set new_root_node root
    }
    # selected node
    set parent_node [ lindex [$tree selection get] 0 ]
    if { $parent_node == "" } {
       set parent_node $new_root_node
    } 

    set parent_type [Gm::node_type $parent_node]
    if { $parent_type != "group" } {
        set parent_node [$tree parent $parent_node]
    }

    set sel [ lindex [$tree selection get] 0 ]
    if { $sel == "" } { return }
    
    set type [Gm::node_type $sel]
    set id [Gm::node_id $sel]

    switch $type {
        raster {
            GmRaster::duplicate $tree $parent_node $sel $id
        }
        labels {
            GmLabels::duplicate $tree $parent_node $sel $id
        }
        vector {
            GmVector::duplicate $tree $parent_node $sel $id
        }
        cmd {
            GmCmd::duplicate $tree $parent_node $sel $id
        }
        gridline {
            GmGridline::duplicate $tree $parent_node $sel $id
        }
        rgbhis {
            GmRgbhis::duplicate $tree $parent_node $sel $id
        }
        legend {
            GmLegend::duplicate $tree $parent_node $sel $id
        }
        dframe {
            GmDframe::duplicate $tree $parent_node $sel $id
        }
        barscale {
            GmBarscale::duplicate $tree $parent_node $sel $id
        }
        chart {
            GmChart::duplicate $tree $parent_node $sel $id
        }
        thematic {
            GmThematic::duplicate $tree $parent_node $sel $id
        }
        fttext {
            GmFTtext::duplicate $tree $parent_node $sel $id
        }
        dtext {
            GmDtext::duplicate $tree $parent_node $sel $id
        }
        group {
            GmGroup::duplicate $tree $parent_node $sel $id
        }
    }
}


###############################################################################

# save tree/options to file
proc Gm::save { spth } {
    global gisdbase location_name mapset
    global env mon
    variable rcfile
    variable tree

    set fpath $spth
    set rcfile [open $fpath w]

    GmPrint::save
    GmGroup::save $tree 0 "root"

    close $rcfile
}

###############################################################################

# save node to file
proc Gm::save_node { depth node } {
    variable rcfile
    variable tree
    global mon

    set type [Gm::node_type $node]
    set name [$tree itemcget $node -text]

    if { $type == "group" && $name == "UNTITLED" } {
    set name "File $::Gm::filename"
    }

    switch $type {
        group {
            Gm::rc_write $depth Group $name
            incr depth
            GmGroup::save $tree $depth $node
	}
	raster {
            Gm::rc_write $depth Raster $name
            incr depth
	    GmRaster::save $tree $depth $node
	}
	labels {
            Gm::rc_write $depth Labels $name
            incr depth
	    GmLabels::save $tree $depth $node
	}
	vector {
            Gm::rc_write $depth Vector $name
            incr depth
	    GmVector::save $tree $depth $node
	}
	cmd {
            Gm::rc_write $depth Cmd $name
            incr depth
	    GmCmd::save $tree $depth $node
	}
	gridline {
            Gm::rc_write $depth gridline $name
            incr depth
	    GmGridline::save $tree $depth $node
	}
	rgbhis {
            Gm::rc_write $depth rgbhis $name
            incr depth
	    GmRgbhis::save $tree $depth $node
	}
	legend {
            Gm::rc_write $depth legend $name
            incr depth
	    GmLegend::save $tree $depth $node
	}
	dframe {
            Gm::rc_write $depth dframe $name
            incr depth
	    GmDframe::save $tree $depth $node
	}
	barscale {
            Gm::rc_write $depth barscale $name
            incr depth
	    GmBarscale::save $tree $depth $node
	}
	chart {
            Gm::rc_write $depth chart $name
            incr depth
	    GmChart::save $tree $depth $node
	}
	thematic {
            Gm::rc_write $depth thematic $name
            incr depth
	    GmThematic::save $tree $depth $node
	}
	fttext {
            Gm::rc_write $depth fttext $name
            incr depth
	    GmFTtext::save $tree $depth $node
	}
	dtext {
            Gm::rc_write $depth dtext $name
            incr depth
	    GmDtext::save $tree $depth $node
	}
    } 
    set depth [expr $depth - 1]
    Gm::rc_write $depth End
    
}

###############################################################################

# load tree/options from file
proc Gm::load { lpth } {
    global gisdbase location_name mapset
    global env mon
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
	        GmPrint::set_option $key $val
            }
        } else {
            if { $key == "Print" } {
                 set print_section 1
	    } else {  
		# Tree of layers	
		switch $key {
		    Group {

			if { [regexp -- {^File (.+)} $val r leftover]  && ($leftover !=
			$::Gm::filename)} {
			    	set val "<-- $leftover"
			}

			set current_node [GmGroup::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
			set parent $current_node
		    }
		    Raster {
			set current_node [GmRaster::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    Labels {
			set current_node [GmLabels::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    Vector {
			set current_node [GmVector::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    Cmd {
			set current_node [GmCmd::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    gridline {
			set current_node [GmGridline::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    rgbhis {
			set current_node [GmRgbhis::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    legend {
			set current_node [GmLegend::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    dframe {
			set current_node [GmDframe::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    barscale {
			set current_node [GmBarscale::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    chart {
			set current_node [GmChart::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    thematic {
			set current_node [GmThematic::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    fttext {
			set current_node [GmFTtext::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    dtext {
			set current_node [GmDtext::create $tree $parent]
			$tree itemconfigure $current_node -text $val 
		    }
		    End {
			set type [Gm::node_type $current_node]
			if { $type == "group"  } {
			    set parent [$tree parent $parent]
			}
			set current_node [$tree parent $current_node]
		    }
		    default {
		      if {[catch {Gm::node_type $current_node}] } {
			tk_messageBox -type ok -message "Can't open $fpath - bad file format"
			break
		      } else {

			set type [Gm::node_type $current_node]
			switch $type {
			    group { 
				GmGroup::set_option $current_node $key $val
			    }
			    raster { 
				GmRaster::set_option $current_node $key $val
			    }
			    labels { 
				GmLabels::set_option $current_node $key $val
			    }
			    vector { 
				GmVector::set_option $current_node $key $val
			    }
			    cmd { 
				GmCmd::set_option $current_node $key $val
			    }
			    gridline { 
				GmGridline::set_option $current_node $key $val
			    }
			    rgbhis { 
				GmRgbhis::set_option $current_node $key $val
			    }
			    legend { 
				GmLegend::set_option $current_node $key $val
			    }
			    dframe { 
				GmDframe::set_option $current_node $key $val
			    }
			    barscale { 
				GmBarscale::set_option $current_node $key $val
			    }
			    chart { 
				GmChart::set_option $current_node $key $val
			    }
			    thematic { 
				GmThematic::set_option $current_node $key $val
			    }
			    fttext { 
				GmFTtext::set_option $current_node $key $val
			    }
			    dtext { 
				GmDtext::set_option $current_node $key $val
			    }
			}
		      }
		    }           
		}
	    }
	    incr row
	    set prg [expr $max_prgindic * $row / $nrows]
	    if { $prg > $max_prgindic } { set prg $max_prgindic }
	    set Gm::prgindic $prg
        }
    }
    close $rcfile
    set Gm::prgindic $max_prgindic
    set prgtext "Layers loaded"
}

###############################################################################

# write one row
proc Gm::rc_write { depth args } {
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

###############################################################################

# returns node type
proc Gm::node_type { node } {
    variable tree
    global mon

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
    if { [string match gridline* $node] } {
       return "gridline"
    }  
    if { [string match rgbhis* $node] } {
       return "rgbhis"
    }  
    if { [string match legend* $node] } {
       return "legend"
    }  
    if { [string match dframe* $node] } {
       return "dframe"
    }  
    if { [string match barscale* $node] } {
       return "barscale"
    }  
    if { [string match chart* $node] } {
       return "chart"
    }  
    if { [string match thematic* $node] } {
       return "thematic"
    }  
    if { [string match fttext* $node] } {
       return "fttext"
    }  
    if { [string match dtext* $node] } {
       return "dtext"
    }  
    
    return ""
}

###############################################################################

# returns node id
proc Gm::node_id { node } {
    variable tree
	global mon
	
    if { ![regexp {[^:]+:(.+)$} $node x id] } {
        return 0
    } else {
        return $id
    }
}

###############################################################################

# execute command
proc Gm::execute { cmd } {
    global env

    # warning: DBMI - be careful and test 'd.vect where=' after changes
    puts stdout $cmd
    
      ## This was old version - does not work, because $shell have not LD_LIBRARY_PATH to GRASS libs ? 
      #set shell $env(SHELL)
      #set cmd [ string map { \" \\\" \$ \\\$ } $cmd ]
      #eval "exec echo \"$cmd\" | $shell >@stdout 2>@stdout"

    eval "exec $cmd >@stdout 2>@stdout"
}

###############################################################################

# open print window
proc Gm::print { } {
    GmPrint::window
}

###############################################################################

#open dialog box
proc Gm::OpenFileBox {w} {
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
                ::Gm::filename_new] || \
                [string match {} $::Gm::filename_new]} return
	
	if {[catch {if { [ regexp -- {^Untitled.dmrc$} $::Gm::filename r]} {}}] } {
		set ::Gm::filename $::Gm::filename_new
	}
	
	Gm::load $::Gm::filename_new
		
};

###############################################################################

#save dialog box
proc Gm::SaveFileBox {w} {
    global mainwindow
    variable win

    set win $w
    if { $win == ""} {set win $mainwindow}
    catch {if { [ regexp -- {^Untitled.dmrc$} $::Gm::filename r]} {unset ::Gm::filename}}
    if {[catch {Gm::save $::Gm::filename}]} {
        set types {
            {{Adm Resource Files} {{.dm} {.dmrc}}}
            {{All Files} *}
        }
        if {[catch {tk_getSaveFile \
                -parent $win \
                -filetypes $types \
                -title {Save File}} \
                ::Gm::filename] || \
                [string match {} $::Gm::filename]} return
	
	Gm::save $::Gm::filename
	Gm::FileClose stay_alive
	Gm::load $::Gm::filename
    }
};

###############################################################################

proc Gm::cleanup { } {
	runcmd "g.mremove -f region=mon_* >/dev/null"
	eval exec "rm dispmon.png >/dev/null"
	destroy mon
}

###############################################################################

proc main {argc argv} {
    global auto_path
    global GRASSVERSION
    global location_name
    global keycontrol

    wm withdraw .
    wm title . [G_msg "GRASS$GRASSVERSION GIS Manager - $location_name"]

    bind . <$keycontrol-Key-o> {
	Gm::OpenFileBox {}
    }
    bind . <$keycontrol-Key-n> {
	Gm::new
    }
    bind . <$keycontrol-Key-s> {
	Gm::SaveFileBox {}
    }
    bind . <$keycontrol-Key-q> {
	GmPrint::clean;  exit
    }
    bind . <$keycontrol-Key-x> {
	Gm::delete
    }
    bind . <$keycontrol-Key-w> {
	Gm::FileClose {}
    }
    bind . <$keycontrol-Key-p> {
    Gm::print
    }


    Gm::create
#    Gm::setenv
    GmPrint::init
    GmPrint::init_tmpfiles
    BWidget::place . 0 0 at 700 300
    wm deiconify .
    raise .
    focus -force .
    destroy .intro
    
    if { $argc == "1"} { 
    	set ::Gm::filename $argv
	Gm::load $::Gm::filename
    }

}

bind . <Destroy> { if { "%W" == "."} { Gm::cleanup } } 
main $argc $argv
wm geom . [wm geom .]

