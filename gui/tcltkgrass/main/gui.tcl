###############################################################################
# Name:		gui.tcl
# Author:	Jacques Bouchard (bouchard@onera.fr)
#    additions:	Markus Neteler (neteler@geog.uni-hannover.de)
#    updated for GRASS 5.7 by Michael Barton (Arizona State University) and 
#    Glynn Clements
# $Id$
# Description:	Simple interface builder for TCLTKGRASS
#
# procedure: "menu_build" to create a menubar calling the module GUIs
#
# Usage:	menu_build 1 PATH DESCRIPTION
#
#	1:		Sorry, you have to put this constant
#			due to recursion used by menu_build
#
#	PATH:		path of the parent widget of the menubar
#
#	DESCRIPTION:	description of the menubar
#
#	DESCRIPTION=	{LABEL ACTION [-separator] [...]}
#
#	-separator:	a horizontal dividing line is displayed in the menu
#	LABEL:		name of the menu item
#	ACTION:		either a command or a nested DESCRIPTION
#
#	Well, it's rather difficult to explain, but very easy to understand
#	with an example: have a look at menu.tcl
#
###############################################################################

source $env(GISBASE)/etc/gui.tcl

###############################################################################
proc execute {cmd} {
    global dlg path

    set code [exec -- $cmd --tcltk]

    set path .dialog$dlg
    toplevel $path
    eval $code
}


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
    eval exec -- xterm -e $cmd $args &
}


###############################################################################
proc set_menu_font {} {
    global main_menu
    fontsel {Menu font} main_menu(font)
    setfont .main_menu $main_menu(font)
    resize_menu
}

###############################################################################

proc menu_build {initial path description} {
    global env main_menu balloonHelp

    if {[llength $description] == 0} {
        error "Incorrect menu description (empty item)"
    }

    set j 0

    foreach {label comment desc} $description {
        if {[string match -separator $label]} {
            if {$initial} {
                error {'-separator' usage forbidden in main menu}
            } else {
                $path add separator
            }
	    continue
	}

	if {[llength $desc] == 1} {
	    set cmd [lindex $desc 0]
	    $path add command -label $label -command $cmd \
		-state [expr {$cmd == "" ? "disabled" : "normal"}]
	} else {
	    if {$initial} {
		set menupath $path.mb$j.m
		menubutton $path.mb$j -text $label -menu $menupath
		menu $menupath
		pack $path.mb$j -side left -expand yes
		bindtags $path.mb$j [list $path.mb$j Menubutton balloon . all]
		set balloonHelp($path.mb$j) $comment
	    } else {
		set menupath $path.m$j
		$path add cascade -label $label -menu $menupath
		menu $menupath -tearoff 0
	    }
	    menu_build 0 $menupath $desc
	}
	incr j
    }

    if {$initial} {
	setfont $path $main_menu(font)
	menu $path.popup -tearoff 0
	$path.popup add command -label "Automatic size" -command resize_menu
	bind $path <ButtonPress-3> {
	    puts %W
	    tk_popup [winfo toplevel %W].popup %X %Y 5
	}
    }
}

###############################################################################

proc create_separator {path num color height} {
    upvar $num n
    frame $path.frame$n -background $color -height $height
    grid $path.frame$n -columnspan 2 -sticky we
    incr n
}

###############################################################################

proc fontsel {title font} {
    global fontselarray dialog_font

    scan $font {%[^(]} name
    global $name

    set path .fontsel
    toplevel $path
    wm title $path $title

    if [catch {font actual [subst $$font]} message] {
        set message [font actual helvetica]
    }
    array set fontselarray $message

    label  $path.lab1 -text "Family:"
    entry  $path.ent1 -textvariable fontselarray(-family)
    button $path.but1 -text Pick -command {
        set font [list_select {} {Font family} {Families} [font families]]
        if {[llength $font] > 0} {set fontselarray(-family) $font}
    }

    grid $path.lab1 $path.ent1 $path.but1 -sticky w

    label $path.lab2 -text "Size:"
    entry $path.ent2 -textvariable fontselarray(-size)
    grid $path.lab2 $path.ent2 -sticky w

    label $path.lab3 -text "Weight:"
    checkbutton $path.check1 -text normal -offvalue "" -onvalue normal \
        -variable fontselarray(-weight)
    grid $path.lab3 $path.check1 -sticky w
    checkbutton $path.check2 -text bold   -offvalue "" -onvalue bold \
        -variable fontselarray(-weight)
    grid $path.check2 -column 1 -sticky w

    label $path.lab4 -text "Slant:"
    checkbutton $path.check3 -text roman  -offvalue "" -onvalue roman \
        -variable fontselarray(-slant)
    grid $path.lab4 $path.check3 -sticky w
    checkbutton $path.check4 -text italic -offvalue "" -onvalue italic \
        -variable fontselarray(-slant)
    grid $path.check4 -column 1 -sticky w

    frame $path.buttons
    grid $path.buttons -columnspan 3 -sticky ewsn
    button $path.buttons.ok -text OK -command "eval set $font [list {[eval list [list $fontselarray(-family)] $fontselarray(-size) $fontselarray(-weight) $fontselarray(-slant)]}]; destroy $path"
    button $path.buttons.quit -text Quit -command "destroy $path"
    pack $path.buttons.ok $path.buttons.quit -side left -expand yes

    setfont $path $dialog_font

    grab set $path
    tkwait window $path
}

###############################################################################

proc list_select {geometry title subtitle list} {
    global list_select_name dialog_font

    set list_select_name ""

    toplevel .list
    catch {
        wm geometry .list $geometry
    }
    wm title .list $title

    label .list.label -text $subtitle
    pack .list.label -side top

    frame .list.buttons
    pack .list.buttons -side bottom -fill x
    button .list.buttons.ok -text OK -command {
        set i [.list.frame.listbox curselection]
        if {[string length $i] > 0} {
            set list_select_name [.list.frame.listbox get $i]
            destroy .list
        }
    }
    button .list.buttons.cancel -text Cancel -command {
        set list_select_name ""
        destroy .list
    }
    pack .list.buttons.ok .list.buttons.cancel -side left -expand yes

    scrolly_widget listbox .list.frame -setgrid yes -width 0
    pack .list.frame -side top -fill both -expand yes
    bind .list.frame.listbox <Double-ButtonPress-1> {.list.buttons.ok invoke}

    foreach i $list {.list.frame.listbox insert end $i}

    setfont .list $dialog_font

    grab set .list
    tkwait window .list

    return $list_select_name
}

###############################################################################
# changed by Andreas Lange (andreas.lange@rhein-main.de)

proc help {title textopts tagopts message} {
    global help_font

    toplevel .help
    wm title .help $title

    bind .help <Return> {destroy .help}

    button .help.ok -text OK -command "destroy .help"
    pack .help.ok -side bottom

    eval scrollxy_widget text .help.frame -setgrid yes -wrap none $textopts
    pack .help.frame -side top -fill both -expand yes
    setfont .help.frame $help_font
    .help.frame.text insert end $message texttag
    eval .help.frame.text tag configure texttag $tagopts

    .help.frame.text configure -state disabled
    focus .help.frame.text
    
    grab .help
    tkwait window .help
}

###############################################################################

proc scrollx_widget {widget path args} {
    frame $path
    eval $widget $path.$widget $args {-xscrollcommand [list $path.xscroll set]}
    scrollbar $path.xscroll -width 10 -orient horizontal \
        -command [list $path.$widget xview]
    grid $path.$widget -sticky news
    grid $path.xscroll -sticky news
    grid rowconfigure    $path 0 -weight 1
    grid columnconfigure $path 0 -weight 1
    return $path.$widget
}

###############################################################################

proc scrolly_widget {widget path args} {
    frame $path
    eval $widget $path.$widget $args {-yscrollcommand [list $path.yscroll set]}
    scrollbar $path.yscroll -width 10 -orient vertical \
        -command [list $path.$widget yview]
    grid $path.$widget $path.yscroll -sticky news
    grid rowconfigure    $path 0 -weight 1
    grid columnconfigure $path 0 -weight 1
    return $path.$widget
}

###############################################################################

proc scrollxy_widget {widget path args} {
    frame $path
    eval $widget $path.$widget $args { \
        -xscrollcommand [list $path.xscroll set] \
        -yscrollcommand [list $path.yscroll set] \
    }
    scrollbar $path.xscroll -width 10 -orient horizontal \
        -command [list $path.$widget xview]
    scrollbar $path.yscroll -width 10 -orient vertical \
        -command [list $path.$widget yview]
    grid $path.$widget $path.yscroll -sticky news
    grid $path.xscroll -sticky news
    grid rowconfigure    $path 0 -weight 1
    grid columnconfigure $path 0 -weight 1
    return $path.$widget
}

###############################################################################

proc children {path} {
    set list {}
    foreach child [winfo children $path] {
        eval lappend list $child [children $child]
    }
    return $list
}

###############################################################################

proc setfont {path font} {
    if {[llength $font] > 0} {
        foreach child [children $path] {
            catch {$child configure -font $font}
        }
    }
}

###############################################################################

proc resize_menu {} {
    wm geometry . ""
}

###############################################################################
# modified by Michael Barton (Arizona State University) for GRASS 5.7

proc quit {} {exit}

###############################################################################
# added by Andreas Lange (andreas.lange@rhein-main.de)

proc set_list {path file {elemn 1} {sep " "} } {
    global env

    set list ""

    # this is ugly, but portable to win
    if { [regexp -- {filelist} $file] } {
	set gispath [file join $env(GISBASE) $path "*"]
	set files [glob $gispath]
	foreach file $files {
	    lappend list [file tail $file]
	}
	return $list
    }

    set gispath [file join $env(GISBASE) $path]
    set filename [file join $gispath $file]
    if { ! [file exists $filename] } {
	return
    }
    if { [catch {set fd [open $filename r]}] } {
	return
    }

    foreach line [lsort -dictionary [split [read $fd] \n]] {
	if { [regexp -- {^\#} $line] || [regexp -- {^\ } $line] || [regexp -- {^;} $line]} {
	    continue
	}
	set i 0
	foreach element [split $line $sep] {
	    incr i
	    if { $i == $elemn } {
		lappend list $element
		continue
	    }
	}
    }
    close $fd
    return $list
}

###############################################################################
# added by Andreas Lange (andreas.lange@rhein-main.de)
#
# proc set_array {arrayname path file arrayn elemn {sep " "} } {
#  return 1
# }

###############################################################################

source $env(TCLTKGRASSBASE)/main/balloon.tcl

BalloonInit

array set Featuredir {
	mapset		{$env(GISDBASE)/$env(LOCATION_NAME)		.	"Available mapsets:"}
	Mapset		{$env(GISDBASE)/$env(LOCATION_NAME)		(^[^P]|[^T]$)	"Available mapsets:"}
	module		{"$env(GISBASE)/bin $env(GISBASE)/scripts"	.	"Binaries and shells:"}
	postscript	{$env(GISBASE)/etc/paint/ps.devices		.	"Device:"}
	paint		{$env(GISBASE)/etc/paint/driver			.	"Device:"}
}

#
# format:
# main_element:alias:description:menu text
#    support_element:description
#

set fonts [set_list fonts filelist 1]
set main_menu(window_geometry) "+0+0"
set main_menu(font) ""
set result_font courier
set help_font courier

if ![catch {open ~/.tcltkgrass r} stream] {
    while {[gets $stream line] >= 0} {
        if {[llength $line] != 2 || \
            [set i [scan [lindex $line 0] {%[^(](%[^)]) %s} array index buffer]] != 1 && \
            $i != 2} {
            tk_messageBox -type ok -message "$line: wrong line"
        } else {
            global $array
            set [lindex $line 0] [lindex $line 1]
            if {$i == 1} {
            } elseif {$array == "env"} {
            } elseif {$array == "xdriver"} {
                 if {[scan $index {x%d,%s %s} monitor prop buffer] == 2} {
                     if {$prop == "window_state" && [llength [lindex $line 1]] > 0} {
                         lappend active_xdrivers x$monitor
                     }
                 }
            } elseif {$array == "xdriver_defaults"} {
            } elseif {$array == "quit_window"} {
            } elseif {$array == "window_management"} {
            } elseif {$array == "main_menu"} {
            } elseif {$array == "group"} {
            } elseif {$array == "subgroup"} {
	    } elseif {$array == "html"} {
            } else {
            }
        }
    }
    close $stream
}

wm title . "TCLTKGRASS 4.0 (2004) - Location: $env(LOCATION_NAME) "

wm geometry . $main_menu(window_geometry)
catch {
    if {$main_menu(window_state) == "iconic"} {
        wm iconify .
    }
}

label .loading -text {Loading ...}
pack .loading -expand yes

toplevel .w_g
wm geometry .w_g 50x50+0+0
update idletasks
scan [exec xwininfo -id [winfo id .w_g] | grep -i Corners] {%*s %d%d} dx dy
destroy .w_g
update idletasks
set window_management(dx) $dx
set window_management(dy) $dy
set window_management(lx) [expr [winfo screenwidth  .] - ($dx + $dx)]
set window_management(ly) [expr [winfo screenheight .] - ($dx + $dy)]
raise .
destroy .loading
