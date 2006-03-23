##########################################################################
#
# gm.tcl
#
# Primary tcltk script for GIS Manager: GUI for GRASS 6 
# Author: Michael Barton (Arizona State University)
# Based in part on Display Manager for GRASS 5.7 by Radim Blazek (ITC-IRST)
# and tcltkgrass for GRASS 5.7 by Michael Barton (Arizona State University)--
# with contributions by Glynn Clements, Markus Neteler, Lorenzo Moretti, 
# Florian Goessmann, and others
#
# March 2006
#
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
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
set iconpath $env(GISBASE)/etc/gui/icons

global iconpath
global gmpath

set keycontrol "Control"
set tmenu "1"
set keyctrl "Ctrl"
set execom "execute"
set msg 0
set mon 1
set moncount 1

if {[info exists env(HOSTTYPE)]} {
	set HOSTTYPE $env(HOSTTYPE)
} else {
	set HOSTTYPE ""
}

# set background color and help font
set bgcolor HoneyDew2
set helpfont [font create -family Verdana -size 12 ]

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
source $gmpath/gmtree.tcl
source $gmpath/gmtool1.tcl
source $gmpath/gmtool2.tcl
source $gmpath/group.tcl
source $gmpath/vector.tcl
source $gmpath/raster.tcl
source $gmpath/labels.tcl
source $gmpath/gridline.tcl
source $gmpath/rgbhis.tcl
source $gmpath/histogram.tcl
source $gmpath/rastnums.tcl
source $gmpath/rastarrows.tcl
source $gmpath/legend.tcl
source $gmpath/frames.tcl
source $gmpath/barscale.tcl
source $gmpath/chart.tcl
source $gmpath/thematic.tcl
source $gmpath/maptext.tcl
source $gmpath/mapprint.tcl
source $gmpath/mapcanvas.tcl


namespace eval Gm {
    variable mainframe
    variable status
    variable array tree # mon
    variable rcfile
	global array filename # mon

}


global topwin
global prgtext ""
global prgindic
global max_prgindic 
global helpfont

set max_prgindic 20


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
		global dtxt val
		while {![eof $fh]} {
			set str [gets $fh]
			if {[regexp -- {^GRASS_INFO_PERCENT: (.+)$} $str match val rest]} {
				puts "$match $val $rest"
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
				$dtxt insert end $str
				$dtxt yview end
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

proc Gm::xmon { type cmd } {
    global dmpath outtext
    global env
    
    set xmon 0
    set nextmon 1
    
    
	if { $xmon < 7 } {    
		if ![catch {open "|d.mon -L" r} input] {
			while {[gets $input line] >= 0} {
            	if {[regexp -nocase "$xmon.*not running" $line]} {
					runcmd "d.mon start=x$xmon"
					set nextmon [expr $xmon + 1]
					if { $type == "term" } {
						term_panel $cmd
					} else {
						run_panel $cmd
					}
            	} elseif {[regexp -nocase "$xmon.* running" $line]} {
					incr xmon 1
					runcmd "d.mon start=x$xmon"
					set nextmon [expr $xmon + 1]
					if { $type == "term" } {
						term_panel $cmd
					} else {
						run_panel $cmd
					}
            	}              
       		}
		    close $input
    	}
    }
    return
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
    global options
    global pgs
    global mainframe
    global fon
    global prgtext
    global prgindic
    global keycontrol
    global helpfont
    
    variable mainframe
    variable tree
   
    set prgtext "Loading GIS Manager"
    set prgindic -1
    _create_intro
    update
    
    global env
	source $gmpath/gmmenu.tcl
	
    set prgtext   "Creating MainFrame..."
    
    set mainframe [MainFrame .mainframe \
                       -menu $descmenu -background $bgcolor \
                       -textvariable Gm::status \
                       -progressvar  Gm::prgindic ]

    set mainwindow [$mainframe getframe]

    # toolbar 1 & 2 creation
    set tb1  [$mainframe addtoolbar]
    GmToolBar1::create $tb1
    set tb2  [$mainframe addtoolbar]
    GmToolBar2::create $tb2
    set pw1 [PanedWindow $mainwindow.pw1 -side left -pad 0 -width 10 \
    	-background $bgcolor]    
   
    # tree 
    set treemon [expr $mon + 1]
    set tree_pane  [$pw1 add  -minsize 50 -weight 1]
	set pgs [PagesManager $tree_pane.pgs]

	
	pack $pgs -expand yes -fill both
    pack $tree_pane -side right -expand yes -fill both


    # options
    set options_pane  [$pw1 add -minsize 50 -weight 1]
    set options_sw [ScrolledWindow $options_pane.sw -relief flat -borderwidth 1]
    set options_sf [ScrollableFrame $options_sw.sf]
    $options_sf configure -height 145 -width 460
    $options_sw setwidget $options_sf
    set options [$options_sf getframe]
    pack $options_pane -expand yes -fill both 
    pack $options_sw $options_sf -fill both -expand yes
 
    # command console 
    set output_pane  [$pw1 add -minsize 50 -weight 2 ]
    set output_frame [frame $output_pane.fr -bg $bgcolor]
    set output_bbox [ButtonBox $output_frame.bb -bg $bgcolor \
    	-padx 0 -pady 0 -homogeneous 0 ]
    
    pack $output_frame -expand yes -fill both 
    pack $output_pane -expand yes -fill both 

    set output_sw [ScrolledWindow $output_frame.win -relief sunken -borderwidth 1]
	set outtext [text $output_sw.text -height 5 -width 30 -bg #ffffff] 
	$output_sw setwidget $outtext
	
	$output_bbox add -text "run" -command "Gm::run_txt $outtext"  -bg #dddddd \
		-highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 -width 8 \
        -helptext [G_msg "run command at cursor"] -highlightbackground $bgcolor \
        -pady 3
	$output_bbox add -text "clear" -command "Gm::clear_txt $outtext" -bg #dddddd \
		-highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 -width 8 \
        -helptext [G_msg "Clear output"] -highlightbackground $bgcolor \
        -pady 3
	$output_bbox add -text "save" -command "Gm::save_txt $outtext"  -bg #dddddd \
		-highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 -width 8 \
        -helptext [G_msg "Save output to file"] -highlightbackground $bgcolor \
        -pady 3
	$output_bbox add -text "open output window" -command "Gm::create_disptxt $mon"  \
		-bg #dddddd -highlightthickness 0 -takefocus 0 -relief raised \
		-borderwidth 1 -width 20 \
        -helptext [G_msg "Save output to file"] -highlightbackground $bgcolor \
        -pady 3
	
    pack $output_sw $outtext -fill both -expand yes
    pack $pw1 -side top -expand yes -fill both -anchor n 
	pack $output_bbox -expand no -fill none -anchor s
  
	bind $outtext <$keycontrol-c> "tk_textCopy %W"
	bind $outtext <$keycontrol-v> "tk_textPaste %W"
	bind $outtext <$keycontrol-x> "tk_textCut %W"

	# finish up
    set prgtext "Done"

    set Gm::status [G_msg "Welcome GRASS GIS"]
    $mainframe showstatusbar status 

    pack $mainframe -fill both -expand yes
 
    DynamicHelp::configure -font $helpfont -background yellow

	Gm::startmon
	Gm::create_disptxt $mon
	
	bind .mainframe <Destroy> { 
		set destroywin %W
		Gm::cleanup $destroywin
	} 
		
	

}


###############################################################################

# start new display monitor and increment canvas monitor number
proc Gm::startmon { } {
	global mainwindow
	global mon
	global moncount
	variable tree

	set mon $moncount
	incr moncount 1

	#create initial display canvas and layer tree
	MapCanvas::create
	GmTree::create $mon
	
	wm title .mapcan($mon) [G_msg "Map Display $mon"]
	wm withdraw .mapcan($mon)
	wm deiconify .mapcan($mon)
}


###############################################################################

proc Gm::_create_intro { } {
    global gmpath
    global GRASSVERSION
    global location_name
    global max_prgindic
    global prg

    set top [toplevel .intro -relief raised -borderwidth 2]

    wm withdraw $top
    wm overrideredirect $top 1

    set ximg  [label $top.x -image [image create photo -file "$gmpath/intro.gif"] ]

    set frame [frame $ximg.f -background white]
    set lab1  [label $frame.lab1 -text "GRASS$GRASSVERSION GIS Manager - $location_name" \
                     -background white -foreground black -font {times 14}]
    set lab2  [label $frame.lab2 -textvariable Gm::prgtext -background white \
    	-font {times 12}]
    set prg   [ProgressBar $frame.prg -width 50 -height 15 -background white \
                   -variable Gm::prgindic -maximum $max_prgindic]
    pack $lab1 $prg -side left -fill both -expand yes
    pack $lab2 -side right -expand yes
    place $frame -x 0 -y 0 -anchor nw
    pack $ximg
    BWidget::place $top 0 0 center
    wm deiconify $top
}

###############################################################################

# create output window
proc Gm::create_disptxt { mon } {
	global douttitle
	global bgcolor
	global dtxt
	global keycontrol
	variable mainframe
	variable can

	if { [info exists dout] } {return}

	set douttitle "output"
    set dout [toplevel .dispout]
    wm title .dispout [G_msg $douttitle]


    wm withdraw $dout
    #wm overrideredirect $txt 1

	# output text window
	set doutmf [MainFrame .dispout.fr -bg $bgcolor ]
	set doutfr [$doutmf getframe]
    set dout_sw [ScrolledWindow $doutfr.sw -relief sunken -borderwidth 1]
	set dtxt [text $dout_sw.txt -height 20 -width 40 -bg #ffffff] 

    $dout_sw setwidget $dtxt

	# control buttons
	set dout_tb [$doutmf addtoolbar]
	set dbb [ButtonBox $dout_tb.bb -orient horizontal -background $bgcolor]
	$dbb add -text "clear" -command "Gm::clear_txt $dtxt" -bg #dddddd \
		-highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Clear output"]  -highlightbackground $bgcolor
	$dbb add -text "save" -command "Gm::save_txt $dtxt"  -bg #dddddd \
		-highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Save output to file"]  -highlightbackground $bgcolor

	pack $doutmf -expand yes -fill both
	pack $doutfr -expand yes -fill both -padx 0 -pady 0
	pack $dout_sw -fill both -expand yes
	pack $dtxt -fill both -expand yes
	pack $dbb -fill both -expand no
	
	bind $dtxt <$keycontrol-c> "tk_textCopy %W"
	bind $dtxt <$keycontrol-v> "tk_textPaste %W"
	bind $dtxt <$keycontrol-x> "tk_textCut %W"
		
	BWidget::place $dout 0 0 at 800 150
    wm deiconify $dout
}

###############################################################################
# run selected text (pass to GRASS command line interpreter)
proc Gm::run_txt { txt } {
	global env

	set runtxt [$txt get "insert linestart" "insert lineend"]
	if { $runtxt == "" } {
		set runtxt [$txt get "end linestart" "end lineend"]
	}
	
	run_panel $runtxt
	return
}


# clear output window
proc Gm::clear_txt { txt } {
	global dtxt
	
	$txt delete 1.0 end
}

# save text in output window
proc Gm::save_txt { txt } {
	global env
	global dtxt

	if ![catch {$dtxt get sel.first}] {
		set svtxt [$dtxt get sel.first sel.last]
	} else {
		set svtxt [$dtxt get 1.0 end]
	} 
	
	set types {
    {{TXT} {.txt}}
	}

	if { [info exists HOME] } {
		set dir $env(HOME)
		set path [tk_getSaveFile -initialdir $dir -filetypes $types \
			-defaultextension ".txt"]
	} else {
		set path [tk_getSaveFile -filetypes $types \
			-defaultextension ".txt"]
	}

	if { $path == "" } { return }

	set txtfile [open $path w]
	puts $txtfile $svtxt
	close $txtfile
	return
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

#open dialog box
proc Gm::OpenFileBox { } {
    global mainwindow
    global filename    
    global mon
    
    set types {
            {{Map Resource File} {{.dm} {.dmrc} {.grc}}}
            {{All Files} *}
    }

	set filename_new [tk_getOpenFile -parent $mainwindow -filetypes $types \
		-title {Open File} ]
	if { $filename_new == "" } { return}
	set filename($mon) $filename_new	
	GmTree::load $filename($mon)
		
};

###############################################################################

#save dialog box
proc Gm::SaveFileBox { } {
    global mainwindow
    global filename
    global mon

    catch {
   	if {[ regexp -- {^Untitled_?.grc$} $filename($mon) r]} {
    		set filename($mon) ""
    	}
    }
    
    if { $filename($mon) != "" } {
    	GmTree::save $filename($mon)
    } else {
        set types {
            {{Map Resource File} {{.grc}}}
            {{DM Resource File} {{.dm} {.dmrc}}}
            {{All Files} *}
		}
    	set filename($mon) [tk_getSaveFile -parent $mainwindow -filetypes $types \
    		-title {Save File} -defaultextension .grc] 
    	if { $filename($mon) == "" } { return}
    	GmTree::save $filename($mon)    	
    }
};

###############################################################################

proc Gm::cleanup { destroywin } {
	global mapfile
	global mon
	global tmpdir
	global mappid

	# stop gism PNG driver if it is still running due to error
	if ![catch {open "|d.mon -L" r} input] {
		while {[gets $input line] >= 0} {
			if {[regexp "^gism.*       running" $line]} {
				open "|d.mon stop=gism"
				break
			}
		}
		close $input
	}

	# delete all map display ppm files
	cd $tmpdir
	set deletefile $mappid
	append deletefile ".*"
	foreach file [glob -nocomplain $deletefile] {
		file delete $file
	}
	
	unset mon

	# delete temporary region files for map displays
	runcmd "g.mremove -f region=mon_*"
}

###############################################################################

proc main {argc argv} {
    global auto_path
    global GRASSVERSION
    global location_name
    global keycontrol
    global mainframe
    global filename
    global mon

    wm withdraw .
    wm title . [G_msg "GRASS$GRASSVERSION GIS Manager - $location_name"]

    bind . <$keycontrol-Key-o> {
	Gm::OpenFileBox
    }
    bind . <$keycontrol-Key-n> {
	GmTree::new
    }
    bind . <$keycontrol-Key-s> {
	Gm::SaveFileBox
    }
    bind . <$keycontrol-Key-q> {
    	exit
   	}
    bind . <$keycontrol-Key-w> {
	GmTree::FileClose {}
    }

    Gm::create
    BWidget::place . 0 0 at 400 100

    wm deiconify .
    raise .mainframe
    focus -force .
    destroy .intro
    if { $argc == "1"} { 
    	set filename($mon) $argv
		GmTree::load $filename($mon)
    }
}

bind . <Destroy> { 
	if { "%W" == "."} { Gm::cleanup }
} 

main $argc $argv
wm geom . [wm geom .]

