###############################################################################
# Name:		gui.tcl
# Author:	Jacques Bouchard (bouchard@onera.fr)
#    additions:	Markus Neteler (neteler@geog.uni-hannover.de)
# $Id$
# Description:	Simple interface builder for TCLTKGRASS
#
#	2 procedures:
#
#		"menu_build" to create a menubar calling the module GUIs
#
#		"interface_build" to create a GUI for each module
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
# Usage:	interface_build LIST
#
#	LIST=	{COMMAND TERM DESCRIPTION ITEM ...}
#
#	TERM=	0 | 1
#
#	ITEM=	{entry VARIABLE DESCRIPTION SCROLL BUTTON}		|
#		{scale VARIABLE DESCRIPTION FROM_VALUE TO_VALUE}	|
#		{checkbox VARIABLE DESCRIPTION OFF_VALUE ON_VALUE}	|
#		{separator COLOR HEIGHT}
#
#	BUTTON=	"" | color | Color | 3Dcolor | font | distance | area | spheroid |
#		monitor | file | File | xy | xyz | mapset | module | paint |
#		postscript | FEATURE | method
#
#		file: to get the name of an existing file (for reading)
#		File: to get the name of a file (for writing)
#		xy: to pick x,y coordinates on the active monitor window
#		xyz.map: to pick x,y coordinates on the active monitor window
#		         + z value for the raster map whose name is in variable 'map'.
#		module: man-page for GRASS modules (binaries and shells)
#
#	FEATURE=arc | raster | vector | vector_att | vector_cats |
#		dlg | dlg_ascii | group | icon | label | sites | region
#
#	SCROLL=	0 | 1
#
#	COMMAND:	First name of the command to run
#
#	TERM:		1 if terminal input must be used
#
#	DESCRIPTION:	Description of the command, to write by the widget
#
#	SCROLL:		A scroll bar is added to the widget if SCROLL=1
#
#	BUTTON:		A button is added at the end of the widget if BUTTON!=""
#			The content of BUTTON is written on the button
#			The user is asked to select an item in a list
#			If the BUTTON begins with '+', this character is
#			not written on the button, and the result of the
#			selection is *added* to VARIABLE, after a comma.
#
#	FROM_VALUE:	Minimal value of the scale
#	TO_VALUE:	Maximal value of the scale
#
#	OFF_VALUE:	Value of VARIABLE when the checkbox is deselected
#	ON_VALUE:	Value of VARIABLE when the checkbox is selected
#
#	VARIABLE:	Name of the variable to which the widget value is
#				assigned
#			The string VARIABLE=VALUE is included in the text of
#				the command to run if VALUE!=""
#				(else no string is included)
#			If VARIABLE begins with "-", just the string VALUE
#				is included in the text of the command to run.
#			If VARIABLE is "<", the string VALUE is the name of
#				the file to use as standard input
###############################################################################

proc menu_build {initial path description} {
    global env main_menu balloonHelp

    if {[llength $description] == 0} {
        puts stderr "Incorrect menu description (empty item)"
        exit 1
    }

    set i 0
    set j 0
    foreach element $description {
        if {[string compare $element -separator] == 0} {
            if {$initial} {
                puts stderr {'-separator' usage forbidden in main menu}
                exit 1
            } else {
                $path add separator
            }
        } elseif {$i == 0} {
            set i 1
            set label $element
        } elseif {$i == 1} {
            set i 2
            set comment $element
        } else {
            set i 0
            if {[llength $element] == 1 && [regexp {^\$[^ ]+$} $element]} {
                set element [uplevel #0 subst $element]
            }
            if {[llength $element] == 1} {
                set cmd [lindex $element 0]
                if {$initial} {
                    button $path.b$j -text $label -relief flat -command $cmd
                    pack $path.b$j -side left -expand yes
                    if {[llength $cmd] == 0} {
                        $path.b$j configure -state disabled
                    } else {
                        bindtags $path.b$j [list $path.b$j Button balloon . all]
                        set balloonHelp($path.b$j) $comment
                    }
                } else {
                    if {[llength $cmd] == 0} {
                        $path add command -label $label -command $cmd \
                                          -state disabled
                    } else {
                        $path add command -label $label -command $cmd
                    }
                }
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
                menu_build 0 $menupath $element
            }
            incr j
        }
    }
    if {$i != 0} {
        puts stderr "Incorrect menu description (item \"$element\" alone)"
        exit 1
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

proc interface_build {description} {
    global module_list terminal_input module_font

    set command [lindex $description 0]
    regsub -all {[. =,-]} $command {_} array
    global $array

    llast module_list $array

    set path .$array
    if {[catch {toplevel $path}]} {
        wm withdraw $path
        wm deiconify $path
        return
    }
    wm title $path $command
    if [catch {eval wm geometry $path $${array}(window_geometry)}] {
        wm geometry $path +10+100
    }

    set terminal_input [lindex $description 1]

    set num 0

    set variable ${array}(description)
    set $variable [lindex $description 2]
    [create_entry $path num $variable "" 1 ""] \
        configure -state disabled -fg blue -relief solid

    set ${array}(option_list) ""

    foreach element [lrange $description 3 end] {
        switch -- [lindex $element 0] {
            entry {
                lappendu ${array}(option_list) [lindex $element 1]
                eval create_entry $path num \
                    ${array}([lindex $element 1]) [lrange $element 2 4]
            }
            scale {
                lappendu ${array}(option_list) [lindex $element 1]
                eval create_scale $path num \
                    ${array}([lindex $element 1]) [lrange $element 2 5]
            }
            checkbox {
                lappendu ${array}(option_list) [lindex $element 1]
                eval create_check $path num \
                    ${array}([lindex $element 1]) [lrange $element 2 4]
            }
            separator {
                eval create_separator $path num [lrange $element 1 2]
            }
            default {
                puts stderr "$element: wrong element!"
                exit 1
            }
        }
    }

    set ${array}(command_prefix) $command
    set variable ${array}(command_line)
    set $variable $command
    [create_entry $path num $variable "" 1 ${array}(command_line)] \
        configure -state disabled -fg blue -relief solid

    grid columnconfigure $path 0 -weight 1
    for {set i 0} {$i < $num} {incr i} {
        grid rowconfigure $path $i -weight 1
    }

    trace variable $array w command_set

    menu $path.popup -tearoff 0
    $path.popup add command -label "Automatic size" \
        -command "resize $array"
    $path.popup add command -label Reinitialize \
        -command "reinit_module $array"
    $path.popup add command -label Raise \
        -command "llast module_list $array; raise $path"
    $path.popup add command -label Lower \
        -command "lfirst module_list $array; lower $path"
    $path.popup add separator
    $path.popup add command -label Quit -command "destroy $path"

    bind $path <ButtonPress-3> {
        tk_popup [winfo toplevel %W].popup %X %Y 5
    }
    bind $path <Circulate> {
        puts Circulate
        if {"%p" == "PlaceOnTop"} {
            llast module_list [string trimleft %W .]
        } elseif {"%p" == "PlaceOnBottom"} {
            lfirst module_list [string trimleft %W .]
        }
    }
    bind $path <Configure> {
        if [regexp {^\.([^.]+)$} %W buffer array] {
            set ${array}(window_geometry) [wm geometry %W]
        }
    }
    bind $path <Map> {
        if [regexp {^\.([^.]+)$} %W buffer array] {
            set ${array}(window_state) normal
        }
    }
    bind $path <Unmap> {
        if [regexp {^\.([^.]+)$} %W buffer array] {
            set ${array}(window_state) iconic
        }
    }
    bind $path <Destroy> {
        if [regexp {^\.([^.]+)$} %W buffer array] {
            set ${array}(window_state) ""
        }
    }

    setfont $path $module_font

    tkwait visibility $path
}

###############################################################################

proc create_entry {path num variable description scroll button} {
    upvar $num n
    global balloonHelp terminal_input

    if {[string length $description] > 0} {
        label $path.label$n -anchor w -padx 2 -text $description
        grid $path.label$n - -sticky we
        incr n
    }

    if {$scroll} {set widget "scrollx_widget entry" } else {set widget entry}
    set entry [eval $widget $path.entry$n -textvariable $variable]
    if {[string length $button] > 0} {
        if {[string match {*(*)} $button]} {
            button $path.button$n -fg blue -activeforeground blue -text Run \
                -padx 1 -pady 0
            bind $path.button$n <ButtonPress-1> \
                "execute $button $path.button$n 1 $terminal_input"
            bind $path.button$n <ButtonPress-2> \
                "execute $button $path.button$n 2 $terminal_input"
            bind $path.button$n <ButtonPress-3> \
                "execute $button $path.button$n 3 $terminal_input"
            bindtags $path.button$n [list $path.button$n Button balloon . all]
            set balloonHelp($path.button$n) \
                "Left button:\trun command\nMiddle button:\tprint man page\nRight button:\tprint usage"
        } else {
            regexp {[^()]+} $variable array
            if {$button == "group"} {
                global group
                set group($array) $variable
            } elseif {$button == "subgroup"} {
                global subgroup
                set subgroup($array) $variable
            }
            regexp {\+*([^.]*)} $button exp butt
            button $path.button$n -text $butt \
                -padx 1 -pady 0 -command "feature_get $button $variable"
            if {$button == "Quit"} {
                $path.button$n configure -fg blue -activeforeground blue
            }
        }
        grid $path.entry$n $path.button$n -sticky we
    } else {
        grid $path.entry$n - -sticky we
    }
    incr n

    return $entry
}

###############################################################################

proc create_check {path num variable description off on} {
    upvar $num n
    checkbutton $path.check$n -relief flat -anchor w \
        -text $description \
        -variable $variable -offvalue $off -onvalue $on
    grid $path.check$n -columnspan 2 -sticky we
    incr n
}

###############################################################################

proc create_scale {path num variable description from to incr} {
    upvar $num n
    scale $path.scale$n -width 10 -orient horizontal \
        -label $description \
        -from $from -to $to \
        -variable $variable
    if {$incr != 1} {
        $path.scale$n configure -command "scale_adjust $variable $from $incr"
    }
    grid $path.scale$n -columnspan 2 -sticky we
    incr n
}

###############################################################################

proc create_separator {path num color height} {
    upvar $num n
    frame $path.frame$n -background $color -height $height
    grid $path.frame$n -columnspan 2 -sticky we
    incr n
}

###############################################################################

proc scale_adjust {var x0 dx x} {
    regexp -- {^[^(]+} $var v
    global $v
    set $var [expr $x0 + round(($x - $x0) / $dx) * $dx]
}

###############################################################################

proc feature_get {feature variable} {
    regexp {[^()]+} $variable array
    global $array env group subgroup
    global colors Colors 3Dcolors fonts distance_units area_units spheroids
    global monitors featuredir Featuredir methods

    regexp -- {[+-].*} [wm geometry .$array] geometry
    set var ""

    regexp -- {(\+*)([^.]*)\.*(.*)} $feature exp add feature arg

    switch -exact -- $feature {
        Quit {
            destroy .$array
        }
        color {
            set var [list_select $geometry {} Color $colors]
        }
        Color {
            set var [list_select $geometry {} Color $Colors]
        }
        3Dcolor {
            set var [list_select $geometry {} Color $3Dcolors]
        }
        font {
            set var [list_select $geometry {} Font $fonts]
        }
        distance {
            set var [lindex \
                [list_select $geometry {} {Distance unit} $distance_units] 0]
        }
        area {
            set var [lindex \
                [list_select $geometry {} {Area unit} $area_units] 0]
        }
        spheroid {
            set var [list_select $geometry {} {Spheroid} $spheroids]
        }
        monitor {
            set var [list_select $geometry {} {Monitor} $monitors]
        }
        method {
            set var [list_select $geometry {} {Method} $methods]
        }
        file {
            set var [tk_getOpenFile -initialdir .]
        }
        File {
            set var [tk_getSaveFile -initialdir .]
        }
        xy {
            set var ""
            if [catch {
                set res [exec d.where -1 |& cat]
                if {[scan $res {%*[^0-9]%f%f} x y] == 2} {
                    set var "$x,$y"
                }
            } message] {
                tk_messageBox -type ok -message $message
            }
        }
        xyz {
            set var ""
            if [catch {
                eval set raster $${array}($arg)
                exec d.erase
                eval exec d.rast -o map=$raster
                set res [exec d.what.rast map=$raster -1t |& cat]
                if {[scan $res {%*[^0-9]%f:%f:%*d%*[^:]:%f:} x y z] == 3} {
                    set var "$x,$y,$z"
                }
            } message] {
                tk_messageBox -type ok -message $message
            }
        }
        mapset -
        Mapset -
        module -
        postscript -
        paint {
            set list ""
            foreach dir [lindex $Featuredir($feature) 0] {
                foreach file [eval glob -nocomplain $dir/*] {
                    set filetail [file tail $file]
                    if {([file isfile $file] || [file isdirectory $file]) && [regexp [lindex $Featuredir($feature) 1] $filetail]} {
                        lappend list $filetail
                    }
                }
            }
            set var [list_select $geometry $feature [lindex $Featuredir($feature) 2] \
                     [lsort $list]]
        }
        default {
            if {[info exists featuredir($feature)]} {
                set pwd [pwd]
		set inpath 1
                set list ""
		cd $env(GISDBASE)/$env(LOCATION_NAME)
                foreach dir [concat [exec g.mapsets -p] . [glob *]] {
		    if {[string compare $dir .] == 0} {
		        set inpath 0
			continue
		    }
		    if [info exists dirstat($dir)] continue
		    set dirstat($dir) 1
                    if {[catch {eval eval cd $env(GISDBASE)/$env(LOCATION_NAME)/$dir/$featuredir($feature)}]} {
                        if {0 && $dir == $env(MAPSET)} {
                            tk_messageBox -message "$feature directory\n'[subst [subst $featuredir($feature)]]'\nnon-existent or unusable" -type ok
                        }
                    } elseif {[catch {glob *} names]} {
                    } elseif {$inpath} {
                        if {$dir == $env(MAPSET)} {
                            eval lappend list [lsort $names]
                        } else {
                            foreach name [lsort $names] {
                                lappend list "$name ($dir)"
                            }
                        }
                    } else {
                        foreach name [lsort $names] {
                            lappend list $name@$dir
                        }
                    }
                }
                cd $pwd
		if {[llength $list] > 0} {
                    set var [lindex [list_select $geometry $feature {} $list] 0]
                }
            }
        }
    }

    if {[string length $var] > 0} {
        if {[string length $add] > 0 && [eval string length $$variable] > 0} {
            append $variable , $var
        } else {
            set $variable $var
        }
    }
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

proc setdisplay {} {
    global env xdriver_defaults dialog_font

    set path .set_display
    toplevel $path
    wm title $path {Display dimensions}


    label $path.label1 -anchor w -text "XDriver default LEFT position"
    entry $path.entry1 -textvariable xdriver_defaults(left)
    label $path.label2 -anchor w -text "XDriver default TOP position"
    entry $path.entry2 -textvariable xdriver_defaults(top)
    label $path.label3 -anchor w -text "XDriver default WIDTH"
    entry $path.entry3 -textvariable xdriver_defaults(width)
    label $path.label4 -anchor w -text "XDriver default HEIGHT"
    entry $path.entry4 -textvariable xdriver_defaults(height)
    label $path.label5 -anchor w -text "CELL driver WIDTH"
    entry $path.entry5 -textvariable env(GRASS_WIDTH)
    label $path.label6 -anchor w -text "CELL driver HEIGHT"
    entry $path.entry6 -textvariable env(GRASS_HEIGHT)
    pack $path.label1 $path.entry1 \
         $path.label2 $path.entry2 \
         $path.label3 $path.entry3 \
         $path.label4 $path.entry4 \
         $path.label5 $path.entry5 \
         $path.label6 $path.entry6 -side top -expand yes -fill x

    button $path.quit -text OK -command "destroy $path"
    pack $path.quit -side bottom -expand yes -pady 1m

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

proc command_set {array element op} {
    global $array

    set cmd [list [subst $${array}(command_prefix)]]
    foreach i [subst $${array}(option_list)] {
        eval set j $${array}($i)
        if {[string length $j] > 0} {
            switch -glob -- $i {
                -*      {append cmd " $j"}
                <       {append cmd " < $j"}
                >       {append cmd " > $j"}
                default {append cmd " $i=$j"}
            }
        }
    }
    set ${array}(command_line) $cmd
}

###############################################################################

proc run {args} {
    global env
    eval exec xterm -title [lindex $args 0] -e $env(TCLTKGRASSBASE)/main/pause $args
}

###############################################################################

proc execute {command path button terminal} {
    upvar $command cmd
    global env pipe nbackspace balloonHelp command_state show_output

    set root [lindex $cmd 0]

    switch [$path cget -text] {
        Run {
            if {$button == 1} {
                set name [eval concat $cmd]
                set see [expr {[lindex $cmd 0] == "g.manual"} ? 0 : -1]
            } elseif {$button == 2} {
                set name "g.manual [lindex $root 0]"
                set see 0
            } else {
                set name "[lindex $root 0] help"
                set see 0
            }
            if $terminal {
                set name [list | xterm -title [list $root I/O] \
                          -e $env(TCLTKGRASSBASE)/main/pause $name]
            } else {
                if ![regexp -- < $cmd] {append name " < /dev/null"}
#               set name "| $name |& cat"
                set name "| $name 2>@ stdout"
            }
            if [catch {open $name r} message] {
                tk_messageBox -type ok -message $message
            } else {
                set pipe($root) $message
                set nbackspace 0
                set command_state($path) "Running ..."
                set show_output($path) 1
                if {$see == 0} {
                    catch {
                        set root [lindex $cmd 0]
                        regsub -all {[. =,-]} $root {_} array
                        global $array
                        scan [.$array.print.frame.text index end] %d see
                        incr see -2
                    }
                }
                fileevent $pipe($root) readable \
                          [list reader $pipe($root) $path $cmd $see]
                fconfigure $pipe($root) -blocking 0
                $path configure -text Stop -fg red -activeforeground red
                set balloonHelp($path) "Right button to KILL"
                update idletasks
            }
        }
        Stop {
            if {$button == 3} {
                kill -9 [pid $pipe($root)]
                set command_state($path) ""
            } else {
                kill -STOP [pid $pipe($root)]
                $path configure -text Cont -fg red -activeforeground red
                set command_state($path) "Stopped ..."
                set show_output($path) 1
            }
        }
        Cont {
            if {$button == 3} {
                kill -9 [pid $pipe($root)]
                set command_state($path) ""
            } else {
                kill -CONT [pid $pipe($root)]
                $path configure -text Stop -fg red -activeforeground red
                set command_state($path) "Continued ..."
                set show_output($path) 1
            }
        }
    }
}

###############################################################################

proc reader {stream path cmd see} {
    global nbackspace balloonHelp command_state show_output result_font

    set root [lindex $cmd 0]
    regsub -all {[. =,-]} $root {_} array
    global $array

    if [eof $stream] {
#       if [catch {close $stream} message] {puts $message}
        catch {close $stream}
        $path configure -text Run -fg blue -activeforeground blue
        set balloonHelp($path) \
            "Left button:\trun command\nMiddle button:\tprint man page\nRight button:\tprint usage"
        hideBalloonHelp $path
        if [string length $command_state($path)] {
            set command_state($path) "Finished."
        } else {
            set command_state($path) "Terminated."
        }
        set name .${array}_ouput_window
        if [winfo exists $name] {
            wm deiconify $name
            raise $name
            update idletasks
        }
    } else {
        set text [read $stream 1000]
#       gets $stream text
#       append text "\n"
        while {[string compare $text ""]} {
            set name .$array.print
            if [winfo exists $name] {
                if $show_output($path) {
                    wm deiconify $name
                    raise $name
                    update idletasks
                }
            } else {
                toplevel $name
                wm title $name "$root output"
                if [catch {eval wm geometry $name $${array}(result_geometry)}] {
                    wm geometry $name +10+100
                }
                label $name.label -textvariable command_state($path) -fg blue
                pack $name.label
                frame $name.buttons
                pack $name.buttons -side bottom -fill x
                button $name.buttons.quit -text Quit -command "destroy $name"
                button $name.buttons.save -text Save \
                    -command "savelist $name.frame.text"
                pack $name.buttons.quit $name.buttons.save \
                    -side left -expand yes
                scrollxy_widget text $name.frame -setgrid yes -wrap none \
                                                 -width 40 -height 10
                pack $name.frame -side top -fill both -expand yes
                setfont $name $result_font
                bind $name <Configure> {
                    if [regexp {^\.([^.]+).print$} %W buffer array] {
                        set ${array}(result_geometry) [wm geometry %W]
                    }
                }
                bind $name <Map> {
                    if [regexp {^\.([^.]+).print$} %W buffer array] {
                        set ${array}(result_state) normal
                    }
                }
                bind $name <Unmap> {
                    if [regexp {^\.([^.]+).print$} %W buffer array] {
                        set ${array}(result_state) iconic
                    }
                }
                bind $name <Destroy> {
                    if [regexp {^\.([^.]+).print$} %W buffer array] {
                        set ${array}(result_state) ""
                    }
                }
            }
            set show_output($path) 0
            if {$nbackspace > 0} {
                $name.frame.text delete "end - [expr $nbackspace + 1] chars" end
                set nbackspace 0
            }
            if {! [regexp -indices -- \b+ $text indices]} {
                $name.frame.text insert end $text
                if {$see < 0 } {
                    $name.frame.text see end
                } else {
                    $name.frame.text yview $see
                }
                update idletasks
                break
            }
            set i [lindex $indices 0]
            if {$i > 0} {
                $name.frame.text insert end [string range $text 0 [expr $i - 1]]
                if {$see < 0 } {
                    $name.frame.text see end
                } else {
                    $name.frame.text yview $see
                }
                update idletasks
            }
            set j [lindex $indices 1]
            set nbackspace [expr $j - $i + 1]
            set text [string range $text [expr $j + 1] end]
        }
    }
}

###############################################################################

proc help {title textopts tagopts message} {
    toplevel .help
    wm title .help $title

    button .help.ok -text OK -command "destroy .help"
    pack .help.ok -side bottom

    eval scrollxy_widget text .help.frame -setgrid yes -wrap none $textopts
    pack .help.frame -side top -fill both -expand yes
    .help.frame.text insert end $message texttag
    eval .help.frame.text tag configure texttag $tagopts

    grab .help
    tkwait window .help
}

###############################################################################

proc kill {signal listpid} {
    set stream [open "|ps -l 2> /dev/null" r]
    set n 0
    while {[gets $stream line] >= 0} {
        incr n
        if {$n == 1} {
            set ppid [lsearch -exact $line PPID]
            set pid  [lsearch -exact $line PID]
            if {$ppid < 0 || $pid < 0} {
                tk_messageBox -type ok -message \
                    {'ps -l' doesn't behave as expected
                     -> STOP/CONT/KILL function can't be used}
                close $stream
                return
            }
        } else {
            lappend child([lindex $line $ppid]) [lindex $line $pid]
        }
    }
    close $stream

    set list ""
    foreach i $listpid {
        lappend list $i
        catch {eval lappend list $child($i)}
    }
    catch {eval exec kill $signal $list}
}

###############################################################################

proc savelist {path} {
    global env

    set file [tk_getSaveFile -initialdir .]
    if {[string length $file] == 0} return

    if [catch {open $file w} out] {
        tk_messageBox -type ok -message $out
    } else {
        puts -nonewline $out [$path get 1.0 end]
        close $out
    }
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

proc lappendu {list element} {
    upvar $list l
    if {! [info exists l] || [lsearch -exact $l $element] < 0} {
        lappend l $element
    }
}

###############################################################################

proc llast {list element} {
    upvar $list l
    if {[info exists l] && [set i [lsearch -exact $l $element]] >= 0} {
        set l [lreplace $l $i $i]
    }
    lappend l $element
}

###############################################################################

proc lfirst {list element} {
    upvar $list l
    if {[info exists l]} {
        set i [lsearch -exact $l $element]
        if {$i >= 0} {
            set l [concat $element [lreplace $l $i $i]]
        } else {
            set l [concat $element $l]
        }
    } else {
       lappend l $element
    }
}

###############################################################################

proc tcltkgrass_save {path} {
    global env module_list module_font result_font dialog_font main_menu
    global xdriver_defaults quit_window
    global xdriver active_xdrivers window_management
    global group subgroup

    if [catch {open ~/.tcltkgrass w} stream] return

    writeparam $stream env(GRASS_WIDTH) env(GRASS_HEIGHT) \
                       xdriver_defaults(width) xdriver_defaults(height) \
                       xdriver_defaults(left)  xdriver_defaults(top) \
                       quit_window(save)       quit_window(stopX)

    foreach element [lsort [array names xdriver]] {
        if [regexp {window_state} $element] {
            set xdriver($element) ""
        }
    }

    set active_xdrivers [search_xdrivers]

    if [regexp -nocase {selected monitor:  *(x[0-9]+)} [exec d.mon -p] \
               buffer monitor] {
        set xdriver(selected) $monitor
    } else {
        catch {unset xdriver(selected)}
    }

    foreach element [lsort [array names xdriver]] {
        writeparam $stream xdriver($element)
    }

    writeparam $stream module_font result_font dialog_font main_menu(font)

    puts $stream "main_menu(window_geometry) [wm geometry $path]"
    puts $stream "main_menu(window_state) [wm state $path]"

    foreach module [concat $module_list group subgroup] {
        global $module
        foreach element [lsort [array names $module]] {
            if {$element != "option_list"} {
                writeparam $stream ${module}($element)
            }
        }
    }

    close $stream
}

###############################################################################

proc search_xdrivers {} {
    global window_management xdriver

    set xdriver_list ""

    if ![catch {open "|xlsclients -l" r} input] {
        while {[gets $input line] >= 0} {
            if {[regexp -nocase {^ *Window ([^:]+)} $line buffer window]} {
            } elseif {[regexp -nocase {^ *Name:.*GRASS.*Monitor: *(.*)} $line buffer monitor]} {
               lappend xdriver_list $monitor
               set geometry [lindex [exec xwininfo -id $window | grep -i geometry] 1]
               scan $geometry {%dx%d%[+-]%d%[+-]%d} dx dy sx x sy y
               if {$sx == "-"} {
                   set x [expr $window_management(lx) - $x - $dx]
               }
               if {$sy == "-"} {
                   set y [expr $window_management(ly) - $y - $dy]
               }
               set xdriver($monitor,window_geometry) [format {%dx%d+%d+%d} $dx $dy $x $y]
               if [catch {exec xwininfo -id $window | grep -i IsViewable}] {
                   set xdriver($monitor,window_state) iconic
               } else {
                   set xdriver($monitor,window_state) normal
               }
            }
        }
        close $input
    }

    return [lsort $xdriver_list]
}

###############################################################################

proc writeparam {stream args} {
    foreach name $args {
        upvar $name value
        puts $stream [list $name $value]
    }
}

###############################################################################

proc start_monitor {monitor} {
    global env xdriver xdriver_defaults window_management

    set geometry [format {%dx%d+%d+%d} $xdriver_defaults(width) \
                                       $xdriver_defaults(height) \
                                       $xdriver_defaults(left) \
                                       $xdriver_defaults(top)]
    catch {
        set geometry $xdriver($monitor,window_geometry)
    }

    scan $geometry {%dx%d+%d+%d} width height left top

    set env(XDRIVER_WIDTH)  $width
    set env(XDRIVER_HEIGHT) $height
    set env(XDRIVER_LEFT)   $left
    set env(XDRIVER_TOP)    $top

    exec xterm -geometry 40x5 -e d.mon start=$monitor
}

###############################################################################

proc start_monitors {} {
    global active_xdrivers xdriver

    foreach name [lsort -decreasing $active_xdrivers] {
        start_monitor $name
    }
    catch {exec d.mon select=$xdriver(selected)}
}

###############################################################################

proc stop_monitor {monitor} {
    if {[lsearch -exact [search_xdrivers] $monitor] >= 0} {
        exec d.mon start=$monitor >& /dev/null
        exec d.mon stop=$monitor >& /dev/null
    }
}

###############################################################################

proc stop_monitors {} {
    foreach monitor [search_xdrivers] {
        exec d.mon start=$monitor >& /dev/null
        exec d.mon stop=$monitor >& /dev/null
    }
}

###############################################################################

proc start_module {name} {
    global env module_list
    upvar #0 $name module

    if [catch {
        regsub -all { } $module(command_prefix) {_} cmd
        set window_state $module(window_state)
        if {[llength $window_state] > 0} {
            source $env(TCLTKGRASSBASE)/module/$cmd
            if {$window_state == "iconic"} {wm iconify .$name}
        } elseif {! [file readable $env(TCLTKGRASSBASE)/module/$cmd]} {
            error "File $env(TCLTKGRASSBASE)/module/$cmd is not readable"
        }
    }] {
        catch {unset module}
    } else {
        llast module_list $name
    }
}

###############################################################################

proc start_modules {} {
    global module_list

    set list $module_list
    set module_list ""
    foreach module $list {
        start_module $module
    }
}

###############################################################################

proc reinit_module {name} {
    catch {
        upvar #0 $name module
        set command_prefix $module(command_prefix)
        set window_state $module(window_state)
        regexp {[+-].*} $module(window_geometry) window_geometry
        destroy .$name
        unset module
        set module(command_prefix) $command_prefix
        set module(window_state) $window_state
        set module(window_geometry) $window_geometry
        start_module $name
    }
}

###############################################################################

proc reinit_modules {} {
    global module_list

    set list $module_list
    set module_list ""
    foreach name $list {
        reinit_module $name
    }
}

###############################################################################

proc map_modules {} {
    global module_list

    foreach name $module_list {
        catch {
            upvar #0 $name module
            set path .$name
            if {[wm state $path] == "withdrawn"} {
                switch -- $module(window_state) {
                    iconic {wm iconify $path}
                    normal {wm deiconify $path; raise $path}
                }
            }
            set path .$name.print
            if {[wm state $path] == "withdrawn"} {
                switch -- $module(result_state) {
                    iconic {wm iconify $path}
                    normal {wm deiconify $path; raise $path}
                }
            }
        }
    }
}

###############################################################################

proc unmap_modules {} {
    global module_list

    foreach name $module_list {
        catch {
            foreach suffix {"" .print} {
                set script [bind .${name}${suffix} <Unmap>]
                bind .${name}${suffix} <Unmap> {}
                wm withdraw .${name}${suffix}
                bind .${name}${suffix} <Unmap> $script
            }
        }
    }
}

###############################################################################

proc resize_menu {} {
    wm geometry . ""
}

###############################################################################

proc resize {modules} {
    foreach module $modules {
        catch {wm geometry .$module ""}
    }
}

###############################################################################

proc quit {} {
    global dialog_font
    toplevel .quit_window 
    grab set .quit_window
    frame .quit_window.radio
    frame .quit_window.answer
    pack .quit_window.radio -fill y -expand yes
    pack .quit_window.answer -fill both -expand yes
    checkbutton .quit_window.radio.b1 -text {save config} \
                -variable quit_window(save)
    checkbutton .quit_window.radio.b2 -text {stop all X monitors} \
                -variable quit_window(stopX)
    pack .quit_window.radio.b1 .quit_window.radio.b2 -expand yes -anchor w
    button .quit_window.answer.b1 -text OK -command {
        if {$quit_window(save)}  {tcltkgrass_save .}
        if {$quit_window(stopX)} {stop_monitors}
        exit
    }
    button .quit_window.answer.b2 -text CANCEL -command {destroy .quit_window}
    pack .quit_window.answer.b1 .quit_window.answer.b2 -side left -expand yes
    setfont .quit_window $dialog_font
}

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

array set featuredir {
	arc		arc
	raster		cell
	vector		dig
	vector_att	dig_att
	vector_cats	dig_cats
	vector_ascii	dig_ascii
	dlg		dlg
	dlg_ascii	dlg_ascii
	group		group
	subgroup	group/$$group($array)/subgroup
	signature	group/$$group($array)/subgroup/$$subgroup($array)/sig
	signature2      group/$$group($array)/subgroup/$$subgroup($array)/sigset
	icon		icons
	Icon		ps_icons
	label		paint/labels
	sites		site_lists
	region		windows
}

set colors {
	red
	magenta
	orange
	yellow
	green
	blue
	aqua
	indigo
	white
	gray
	grey
	black
	brown
}

set Colors "$colors none"

set 3Dcolors "color $colors"

set fonts {
	cyrilc
	gothgbt
	gothgrt
	gothitt
	greekc
	greekcs
	greekp
	greeks
	italicc
	italiccs
	italict
	romanc
	romancs
	romand
	romans
	romant
	scriptc
	scripts
}

set distance_units {
	{mi (miles)}
	{f  (feet)}
	{me (meters)}
	{k  (kilometers)}
	{a  (acres)}
	{h  (hectares)}
	{c  (counts)}
}

set area_units {
	{mi (cover measured in square miles)}
	{me (cover measured in square meters)}
	{k  (cover measured in square kilometers)}
	{a  (cover measured in acres)}
	{h  (cover measured in hectares)}
	{c  (the number of cells in the area of interest)}
	{p  (the percent cover, excluding no data areas)}
}

set spheroids {
	GRS67
	IAU76
	MERIT
	SEasia
	airy
	australian
	bessel
	clark66
	clark80
	everest
	grs80
	hayford
	hough
	international
	krassovsky
	mercury
	modif_airy
	modif_everest
	modif_merc
	new_internatnl
	walbeck
	wgs66
	wgs72
	wgs84
}

set monitors {
	x0
	x1
	x2
	x3
	x4
	x5
	x6
	CELL
}

set methods {
	average
	avedev
	distribution
	kurtosis
	mode
	median
	min
	max
	skewness
	stddev
	sum
	variance
}

set env(GRASS_WIDTH)    801
set env(GRASS_HEIGHT)   601

set xdriver_defaults(width)  600
set xdriver_defaults(height) 600
set xdriver_defaults(left)    10
set xdriver_defaults(top)     60

set quit_window(save)  0
set quit_window(stopX) 0

set main_menu(window_geometry) "+0+0"
set main_menu(font) ""
set module_font ""
set result_font courier
set dialog_font ""

set module_list ""
set active_xdrivers ""

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
            } else {
                llast module_list $array
            }
        }
    }
    close $stream
}

wm title . "TCLTKGRASS 3.0 (2000) - Location: $env(LOCATION_NAME) "

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

start_monitors
start_modules

raise .
destroy .loading
