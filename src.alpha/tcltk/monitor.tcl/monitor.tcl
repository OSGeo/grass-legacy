#!wish -f


#lappend auto_path $env(DPG_TCL_LIB_PATH)

###############################################################################
#LIBRARY ROUTINE  (would normally be auto-loaded)
#
# Proc UNIQUE 
#
#   Usage:  unique NAME
#
#    returns a name which is guaranteed to be unique (when compared to other
#     names unique returns) with $name as its prefix. Basically
#     it appends 1-N to the end of the string.  This is good up to
#     the max int value at which point it will wrap around and potentially
#     fail.
#
#
#  EXTENSIONS:
#
#     None
#
#
#  RETURNS:
#     Normal return value
#     Result is a unique string
#
#
#  GLOBALS:
#     unique_a()
#     
#  PROCS:
#     unique
#     



set unique_a(start) 1

proc unique {name} {
    global unique_a

    switch [catch {set num [set unique_a($name)]}] {
	0 {			; # Found a match
	    incr num
	    set unique_a($name) $num
	    return $name$num
	  }
	1 {			; # no match  new name
	    set num 1
	    set unique_a($name) $num
	    return $name$num
	  }
    }

    return -code error "unknown error in unique"
}
###############################################################################


wm minsize . 1 1 

# Global settings
set D_color white
set D_thick 1
set File_list {}
set cur_zoom 256
set prev_zoom 256


# Set resources
option add *Button.font fixed
option add *Frame.BorderWidth 3
option add *Canvas.Background black

pack [frame .base] -fill both -expand y

proc make_Pulldown {} {

  pack [frame .base.menu -relief raised -borderwidth 2 -width 200 -height 30] \
      -side top -fill both
    
  pack propagate .base.menu 0

  menubutton .base.menu.file -text "File" -menu .base.menu.file.m -underline 0
  menubutton .base.menu.edit -text "Edit" -menu .base.menu.edit.m -underline 1

  pack .base.menu.file .base.menu.edit -side left

  menu .base.menu.file.m

    .base.menu.file.m add command -label "New"   -underline 0 -state disabled
    .base.menu.file.m add command -label "Open"  -underline 0 \
	-command {
	    set a [busy exec xgbrowser -vector -initialMapset1 PERMANENT]
	    if {$a != {}} { disp_file $a {} $D_color $D_thick } 
	}
    .base.menu.file.m add command -label "Close   " -underline 0 -state disabled
    .base.menu.file.m add separator
    .base.menu.file.m add command -label "Save"  -underline 0 -state disabled
    .base.menu.file.m add command -label "Print" -underline 0 \
	-command { 
	    busy_on
	    set fp [open "|lpr " w] 
	    set a [.base.b.c postscript]
	    puts $fp $fp
	    close $fp
	    busy_off
	}
    .base.menu.file.m add separator
    .base.menu.file.m add command -label "Exit"  -command "exit" -underline 1


  menu .base.menu.edit.m

    .base.menu.edit.m add cascade -label "Update Atts"  -underline 7 \
	-menu .base.menu.edit.m.attrib -state disabled
    .base.menu.edit.m add cascade -label "Remove"        -underline 0 \
	-menu .base.menu.edit.m.delete -state disabled

  menu .base.menu.edit.m.attrib
  menu .base.menu.edit.m.delete


  # Tie menu buttons together and create bindings for auto-traversal
  tk_menuBar .base.menu .base.menu.file .base.menu.edit 
  tk_bindForTraversal .base.menu.file .base.menu.edit 
}

proc busy {args} {
    .base config -cursor watch
    update
    set a [eval $args]
    .base config -cursor ""
    update
    return $a
}

proc busy_on {} { .base config -cursor watch ; update }
proc busy_off {} { update ; .base config -cursor "" ; update }

#
# As files are added or deleted, update the menus to agree w/ File_list
# also disenable commands if there are no files loaded
proc update_edit_menus {} {
    global File_list

    .base.menu.edit.m.attrib delete 0 last
    .base.menu.edit.m.delete delete 0 last

    if {[llength $File_list] > 0} {
	.base.menu.edit.m enable 0
	.base.menu.edit.m enable 1

	foreach i $File_list {
	    .base.menu.edit.m.attrib add command -label $i \
	        -command "update_file $i"
	    .base.menu.edit.m.delete add command -label $i \
		-command "delete_file $i"
	}
    } else {
#	.base.menu.edit.m disable 0
#	.base.menu.edit.m disable 1
	.base.menu.edit.m delete 0
	.base.menu.edit.m delete 1
    }
}

#
#  Update select file from current color and width attributes
proc update_file {file} {
    global D_thick D_color

    .base.b.c itemconfigure $file -fill $D_color -width $D_thick
}

#
#  delete selected file from display
proc delete_file {file} {
    global D_color File_list

    .base.b.c delete $file

    # DELETE FROM LIST
    set a [lsearch $File_list $file]
    set File_list [lreplace $File_list $a $a]

    update_edit_menus
}



frame .base.b

set theCanvas .base.b.c
canvas .base.b.c -relief sunken -borderwidth 3 \
    -scrollregion {-25 -25 281 281} -xscrollcommand ".base.hscroll set" \
    -yscrollcommand ".base.vscroll set"
scrollbar .base.vscroll -relief sunken -command "$theCanvas yview"
scrollbar .base.hscroll -orient horiz -relief sunken -command "$theCanvas xview"

make_Pulldown
pack .base.vscroll -side right -fill y
pack .base.hscroll -side bottom -fill x
pack .base.b -fill both -expand y -side left
pack .base.b.c -fill both -expand y


##################################################################
# Ignore this stuff, it is used for another demo

proc itemStartDrag {c x y} {
    global lastX lastY
    set lastX [$c canvasx $x]
    set lastY [$c canvasy $y]
}

proc itemDrag {c x y {i current}} {
    global lastX lastY
    set x [$c canvasx $x]
    set y [$c canvasy $y]
    $c move $i [expr $x-$lastX] [expr $y-$lastY]
    set lastX $x
    set lastY $y
}



# f is window
# field is canvas tag for window
# theCanvas is canvas
proc setBindings {F Field} {
    upvar #0 $F f $Field field
    global theCanvas

    bind $f <B3-Motion> "itemDrag $theCanvas \
                                    \[expr %x+\[winfo x $f]]\
                                    \[expr %y+\[winfo y $f]] $field";
    bind $f <3> "itemStartDrag $theCanvas \[expr %x+\[winfo x $f]]\
                                    \[expr %y+\[winfo y $f]]";


}



bind $theCanvas <1> "set lastx \[expr %x+\[winfo x $theCanvas]]; \
			set lasty \[expr %y+\[winfo y $theCanvas]]";
bind $theCanvas <2> "$theCanvas create line \$lastx \$lasty \
	    \[expr %x+\[winfo x $theCanvas]] \[expr %y+\[winfo y $theCanvas]]\
		-capstyle round  -width 4 ";


proc add_widget {type options} {
    set path  [unique .base.b.wid]
    set tag  [unique tag_wid]
    set win  [unique win_wid]
    global $win $tag

    eval "$type $path $options"
    set $tag [.base.b.c create window 50 50 -window $path]
    set $win $path

    setBindings $win $tag

    return $path
}

bind $theCanvas <Double-Button-1> "set tag \[$theCanvas find closest \
    \[expr %x+\[winfo x $theCanvas]] \[expr %y+\[winfo y $theCanvas]]] ; \
    $theCanvas delete \$tag ; puts \$tag"

#add_widget button "-text foo"

##################################################################


proc disp_file {file {tag {}} {color white} {width 1}} {
    global first_time File_list cur_zoom x_orig y_orig

    busy_on

    set fd [open "|v.out $file" "r"]

    if {$tag == {}} {set tag $file}

    while { "-1" != [gets $fd buf] } {
	eval .base.b.c create line $buf -tags $tag -fill $color -width $width 
    }
    .base.b.c scale $tag 0 0 $cur_zoom $cur_zoom

    # Note have a problem if a file is already displayed
    lappend File_list $file
    update_edit_menus

    busy_off

    return $file
}


set x_orig 0
set y_orig 0

pack [frame .base.f] -before .base.hscroll  -side bottom

pack [button .base.f.thick -bitmap @thick1.xbm -command set_thick] \
    -side left -padx 10 -expand y -anchor w
pack [button .base.f.color -bitmap @back.xbm -fg $D_color -command set_color ] \
    -padx 10 -side left -expand y -anchor w
pack [button .base.f.in -text In -command zoom_in ] -side left
pack [button .base.f.out -text Out -command zoom_out ] -side left

pack [frame .base.f.f -relief sunken -borderwidth 3] -padx 10
pack [button .base.f.f.high -text Highlight: \
    -command {highlight [.base.f.f.tag get]}] -side left
pack [entry .base.f.f.tag -width 20 -borderwidth 3 -relief sunken ] -side right
bind .base.f.f.tag <Return> {highlight [.base.f.f.tag get]}

proc highlight {tag} {
    .base.b.c itemconfigure $tag -fill red
}

proc zoom_detail {scale} {
    global cur_zoom 


###########X
    set xtmp [.base.hscroll get]
puts stderr "#########"
puts stderr "X1 $xtmp"
    set xtotal [lindex $xtmp 0]
    set xsize  [lindex $xtmp 1]
    set xstart [lindex $xtmp 2]
    set xend   [lindex $xtmp 3]

    
    set xcent [expr ($xstart + $xend) / 2]
    set xcent_rat [expr $xcent. / $xtotal.]


###########Y
    set ytmp [.base.vscroll get]
puts stderr "Y1 $ytmp"
    set ytotal [lindex $ytmp 0]
    set ysize  [lindex $ytmp 1]
    set ystart [lindex $ytmp 2]
    set yend   [lindex $ytmp 3]

    set ycent [expr ($ystart + $yend) / 2]
    set ycent_rat [expr $ycent. / $ytotal.]


##########X
    set new_xtotal [expr int($scale * $xtotal)]
    set new_ytotal [expr int($scale * $ytotal)]
    set new_xsize  $xsize
    set new_ysize  $ysize

    set new_xcent [expr int($xcent_rat * $new_xtotal)]
    set new_xstart [expr $new_xcent - ($new_xsize / 2)]

    set new_ycent [expr int($ycent_rat * $new_ytotal)]
    set new_ystart [expr $new_ycent - ($new_ysize / 2)]

set diff [expr int(.10 * $cur_zoom)]
set end [expr $diff + $cur_zoom]
##########
    .base.b.c config -scrollregion "-$diff -$diff $end $end"
##########
    .base.b.c xview $new_xstart
    .base.b.c yview $new_ystart
}

proc zoom_in {} {
    global x_orig y_orig cur_zoom

    busy_on

    .base.b.c scale all $x_orig $y_orig 2 2 
    set cur_zoom [expr $cur_zoom * 2]

    zoom_detail 2.

    busy_off
}


proc zoom_out {} {
    global x_orig y_orig cur_zoom

    busy_on

    .base.b.c scale all $x_orig $y_orig .5 .5
    set cur_zoom [expr $cur_zoom / 2]

    zoom_detail .5

    busy_off
}



###############################################################################
#
#  Select line thickness
#


proc set_thick {} {
    global D_thick

    incr D_thick

    if {$D_thick > 6} {
        set D_thick 1
    }

    set bitmap @thick$D_thick.xbm

#    switch $D_thick {
#        1 { set bitmap @thick1.xbm }
#        2 { set bitmap @thick2.xbm }
#        3 { set bitmap @thick3.xbm }
#        4 { set bitmap @thick4.xbm }
#        5 { set bitmap @thick5.xbm }
#        6 { set bitmap @thick6.xbm }
#    }


    .base.f.thick config -bitmap $bitmap
}
###############################################################################

###############################################################################
#
#  Select line color
#


proc set_color {} {
    global D_color

    switch $D_color {
        white  { set D_color cyan }
        cyan    { set D_color blue }
        blue   { set D_color yellow }
        yellow { set D_color green }
        green  { set D_color magenta }
        magenta  { set D_color white }
    }

    .base.f.color config -activeforeground $D_color -fg $D_color
}
###############################################################################

update_edit_menus
