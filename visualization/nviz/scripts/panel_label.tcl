# 4/4/95
# M. Astley
# USACERL, blah blah blah

##########################################################################
# 
# Panel to facilitate label placement for finishing images produced
# by nviz.
#
##########################################################################

# Changes
#

# Panel specific globals
global Nv_

# Font Type: Times, Helvetica, Courier
set Nv_(labelFontType) times

# Font Weight: Italic, Bold
set Nv_(labelFontWeight1) 0
set Nv_(labelFontWeight2) 0

# Font Point Size: varies
set Nv_(labelFontSize) 12
set Nv_(labelFontColor) #FF0000

# Legend section
set Nv_(catval) 1
set Nv_(leg_on) 0
set Nv_(catlabel) 0
set Nv_(leg_invert) 0
set Nv_(leg_userange) 0
set Nv_(leg_discat) 0
set Nv_(leg_uselist) 0
set Nv_(cat_list) [list]
set Nv_(cat_list_select) 0

# Label Sites section
set Nv_(labvalues) 1
set Nv_(lablabels) 1
set Nv_(labinbox) 1

global clr

##########################################################################

proc mklabelPanel { BASE } {
    global Nv_ 

    set panel [St_create {window name size priority} $BASE "Label" 2 5]
    frame $BASE -relief groove -borderwidth 2
    Nv_mkPanelname $BASE "Label Panel"
    
    ##########################################################################
    # This section contains widgets for setting font type, size and color
    frame $BASE.text_char -relief groove -bd 5
    set rbase $BASE.text_char

    frame $rbase.font_size -relief raised
    label $rbase.font_size.label -text "Font Size:"
    entry $rbase.font_size.entry -relief sunken -width 4 \
	-textvariable Nv_(labelFontSize)
    button $rbase.font_size.color -text "Color" \
	-bg $Nv_(labelFontColor) -width 8 \
	-command "change_label_color $rbase.font_size.color"
    pack $rbase.font_size.label $rbase.font_size.entry \
	$rbase.font_size.color -side left \
	-padx 2 -anchor n -expand no 

    frame $rbase.font_type -relief raised
    label $rbase.font_type.lab -text "Font: "
    radiobutton $rbase.font_type.times -text "Times-Roman" \
	-value times -variable Nv_(labelFontType) \
        -anchor w
    radiobutton $rbase.font_type.helv  -text "Helvetica" \
	-value helvetica -variable Nv_(labelFontType) \
        -anchor w
    radiobutton $rbase.font_type.cour  -text "Courier" \
	-value courier -variable Nv_(labelFontType) \
        -anchor w 
    pack $rbase.font_type.lab $rbase.font_type.times \
        $rbase.font_type.helv \
	$rbase.font_type.cour -side left -padx 2 \
	-expand no

    frame $rbase.font_weight -relief raised
    label $rbase.font_weight.lab -text "Font Weight: "
    checkbutton $rbase.font_weight.italic -text "Italic" \
	-variable Nv_(labelFontWeight1) -anchor w
    checkbutton $rbase.font_weight.bold   -text "Bold" \
	-variable Nv_(labelFontWeight2) -anchor w
    pack $rbase.font_weight.lab $rbase.font_weight.italic \
        $rbase.font_weight.bold -padx 2 \
	-side left -expand no -anchor w

    pack $rbase.font_type $rbase.font_weight -side top \
	-expand yes -padx 2 -pady 2 -anchor n
    pack $rbase.font_size -side top -expand yes -fill x \
	-before $rbase.font_type -anchor n -padx 2 -pady 2

    ##########################################################################
    # This section contains widgets for specifying the label text and a button
    # which actually places the label
    frame $BASE.text_place -relief groove -bd 5
    set rbase $BASE.text_place

    frame $rbase.buttons -relief raised
    button $rbase.buttons.place -text "Place Label" -command "place_label"
    button $rbase.buttons.undo -text "Undo" -command "delete_list label 1"
    button $rbase.buttons.undo_all -text "Undo All" -command "delete_list label 0"
    pack $rbase.buttons.place $rbase.buttons.undo $rbase.buttons.undo_all -side left \
	-padx 2 -expand no

    entry $rbase.text -relief sunken -width 30 -textvariable Nv_(label_text)
    label $rbase.label -text "Label Text"
    pack $rbase.buttons -side top -expand yes\
	-fill x -padx 2 -pady 2 -anchor n
    pack $rbase.text $rbase.label -side top -expand no \
	-padx 2 -pady 2 -anchor n


    ##########################################################################
    # Separator
    Nv_makeSeparator $BASE.sep1

    ##########################################################################
    # This section contains widgets for specifying a legend
    frame $BASE.legends -relief groove -bd 5
    set rbase $BASE.legends

    # Legend button, invert checkbutton and category checkbuttons
    frame $rbase.leg
   
   button $rbase.leg.legend -text "Place Legend" -command "place_legend"
   checkbutton $rbase.leg.c1 -text "On" \
        -variable Nv_(leg_on) -onvalue 1 -offvalue 0 \
	-command "delete_list legend 0"
   pack $rbase.leg.legend $rbase.leg.c1 \
	-fill x -side left -expand no -padx 2 -pady 1

   frame $rbase.leg2
    checkbutton $rbase.leg2.invert -text "Invert" -anchor w \
	-variable Nv_(leg_invert) -onvalue 1 -offvalue 0
    checkbutton $rbase.leg2.values -text "Category Values" \
	-anchor w -variable Nv_(catval) -onvalue 1 -offvalue 0
    checkbutton $rbase.leg2.labels -text "Category Labels" \
	-anchor w -variable Nv_(catlabel) -onvalue 1 -offvalue 0
    pack $rbase.leg2.invert $rbase.leg2.values $rbase.leg2.labels\
	-fill x -side left -expand no -padx 2 -pady 1

    # Use-range portion of panel
    frame $rbase.ranges -relief sunken
    checkbutton $rbase.ranges.useit -text "Use Range" -anchor w \
	-variable Nv_(leg_userange) -onvalue 1 -offvalue 0

    entry $rbase.ranges.entry_low -relief sunken -width 8 \
        -textvariable Nv_(leg_lorange)
    label $rbase.ranges.label_low -text "Low:" \
	-width 5 -anchor e

    entry $rbase.ranges.entry_hi  -relief sunken -width 8 \
         -textvariable Nv_(leg_hirange)
    label $rbase.ranges.label_hi -text "Hi:" \
	-width 5 -anchor e

    pack $rbase.ranges.useit \
        $rbase.ranges.label_low $rbase.ranges.entry_low \
        $rbase.ranges.label_hi $rbase.ranges.entry_hi \
        -side left -fill x -expand no

    # Return bindings for "use range" entries
    bind $rbase.ranges.entry_low <Return> "$rbase.ranges.useit select"
    bind $rbase.ranges.entry_hi <Return> "$rbase.ranges.useit select"

    frame $rbase.leg3
    # Discrete categories and use-list portion
    checkbutton $rbase.leg3.disc_cat -text "Discrete Categories" \
	-anchor w -width 18 -variable Nv_(leg_discat) \
	-onvalue 1 -offvalue 0
    
    # Some special handling for the "Use List" entry
    checkbutton $rbase.leg3.cb -text "Use List" \
	-anchor w -variable Nv_(leg_uselist) \
	-onvalue 1 -offvalue 0 -command "make_cat_list $rbase.leg3.curr.m" \
        -state disabled
    menubutton $rbase.leg3.curr -text "Current List" \
	-menu $rbase.leg3.curr.m -relief raised \
        -state disabled
    menu $rbase.leg3.curr.m -disabledforeground black

    pack $rbase.leg3.disc_cat $rbase.leg3.cb $rbase.leg3.curr \
         -side left -padx 2 -expand no

    $rbase.leg3.curr.m add command -label "None" -state disabled

    # Pack all portions
    pack $rbase.leg $rbase.leg2 \
         $rbase.ranges $rbase.leg3 -side top \
         -fill x -pady 4 -expand yes

    ##########################################################################
    # Separator
    Nv_makeSeparator $BASE.sep2

    ##########################################################################
    # This section contains widgets for specifying label sites
    frame $BASE.lab_sites -relief groove -bd 5
    set rbase $BASE.lab_sites

    frame $rbase.top
    button $rbase.top.lab_sites -text "  Label Sites  "
    pack $rbase.top.lab_sites \
 	-side left -expand no -padx 2 -pady 1

    frame $rbase.bottom
    checkbutton $rbase.bottom.in_box -text "In Box" -anchor w \
	-variable Nv_(labinbox) -onvalue 1 -offvalue 0
    checkbutton $rbase.bottom.values -text "Values" -anchor w \
	-variable Nv_(labvalues) -onvalue 1 -offvalue 0
    checkbutton $rbase.bottom.labels -text "Labels" -anchor w \
	-variable Nv_(lablabels) -onvalue 1 -offvalue 0
    pack $rbase.bottom.in_box $rbase.bottom.values $rbase.bottom.labels \
	-side left -fill x -expand no -padx 2 -pady 1

    pack $rbase.top $rbase.bottom -side top -expand yes \
	-fill x -padx 2 -pady 1

    ##########################################################################
    # Pack all frames
    pack $BASE.text_char \
	$BASE.text_place \
	$BASE.sep1 \
	$BASE.legends \
	$BASE.sep2 \
	$BASE.lab_sites \
	-padx 2 -pady 2 -fill x -expand no

    return $panel
}

# Simple routine to change the color of fonts
proc change_label_color { me } {
global Nv_

    set clr [lindex [$me configure -bg] 4]
    set clr [mkColorPopup .colorpop LabelColor $clr 1]
    set Nv_(labelFontColor) $clr
    $me configure -bg $clr
}

# Routine to popup a list selector for selecting a discrete list of values
proc make_cat_list {MENU} {
    global Nv_

    # Check to see if we are turning this check button on
    if {$Nv_(leg_uselist) == 0} return

    # Reinitalize list values
    set Nv_(cat_list) [list]
    set Nv_(cat_list_select) 0
    $MENU delete 0 last

    # Create the "individual" subpanel
    set BASE ".cat_list"
    set pname $BASE
    toplevel $pname -relief raised -bd 3
    list_type1 $pname.list 3c 3c
    $pname.list.t configure -text "Category Values"
    entry $pname.level -relief sunken -width 10
    bind $pname.level <Return> "make_cat_list_add $BASE"
    button $pname.addb -text "Add"    -command "make_cat_list_add $BASE"
    button $pname.delb -text "Delete" -command "make_cat_list_delete $BASE"
    button $pname.done -text "Done"   -command "set Nv_(cat_list_select) 1"
    pack $pname.list $pname.level $pname.addb $pname.delb $pname.done\
	-fill x -padx 2 -pady 2

    tkwait variable Nv_(cat_list_select)
    for {set i 0} {$i < [$pname.list.l size]} {incr i} {
	set temp [$pname.list.l get $i]
	lappend Nv_(cat_list) $temp
	$MENU add command -label "$temp" -state disabled
    }

    if {[llength $Nv_(cat_list)]==0} {
	$MENU add command -label None -state disabled
    }

    destroy $BASE
}

# Two quick routines to add or delete isosurface levels for
# selecting them individually
proc make_cat_list_add { BASE } {
    # For this routine we just use the value stored in the
    # entry widget
    # Get the value from the entry widget
    set level [$BASE.level get]

    # Now just append it to the list
    $BASE.list.l insert end $level
}

proc make_cat_list_delete { BASE } {
    # For this procedure we require that the user has selected
    # a range of values in the list which we delete
    # Get the range of selections
    set range [$BASE.list.l curselection]
    
    # Now delete the entries
    foreach i $range {
	$BASE.list.l delete $i
    }
}

# Routine to do_legend
proc do_legend {W x y flag } {
    global Nv_
    global x1 y1 x2 y2

if {$flag == 1} {
#pick first corner
set y [expr $Nv_(height) - $y]

#set first corner of box
      set x1 $x
      set y1 $y

  } else {
set y [expr $Nv_(height) - $y]
#set last corner of box and reset binding
      #Get name of current map 
      set name [Nget_current surf]
      if { [lindex [Nsurf$name get_att color] 0] == "const"} {
	puts "Colortable constant -- no legend available"
#reset everything
        bind $W <Button-1> {}
        bind $W <Button-3> {}
        unset x1
        unset y1
        update
	return
      } 

      set name [lindex [Nsurf$name get_att color] 1]

      set range_low -9999
      set range_high -9999
      if {$Nv_(leg_userange)} {
         set range_low $Nv_(leg_lorange)
         set range_high $Nv_(leg_hirange)
          if { $range_low == ""} {set range_low -9999}
          if { $range_high == ""} {set range_high -9999}
      }
#make sure corner 1 is picked
      if {[info exists x1]} {

      if {$x1 > $x} {
	set x2 $x1
	set x1 $x
      } else {
	set x2 $x
      }
      if {$y1 > $y} {
        set y2 $y1
        set y1 $y
      } else {
        set y2 $y
      }
#get font description
	if {$Nv_(labelFontWeight2) == 1} {
		set weight "bold"
	} else {
		set weight "medium"
	}
	if {$Nv_(labelFontWeight1) == 1} {
		set slant "i"
	} else {
		set slant "r"
	}	
	set font "*-$Nv_(labelFontType)-$weight-$slant-normal--$Nv_(labelFontSize)-*-*-*-*-*-*-*"

#Ndraw_legend Args -- filename use_vals use_labels invert use_range 
# low_range high_range discrete colors corner_coords
	
      Ndraw_legend $name $font $Nv_(labelFontSize) $Nv_(catval) $Nv_(catlabel) $Nv_(leg_invert) $Nv_(leg_discat) \
	$Nv_(leg_userange) $range_low $range_high $x1 $x2 $y1 $y2

#reset bindings
      bind $W <Button-1> {}
      bind $W <Button-3> {}
      unset x1
      unset x2
      unset y1
      unset y2
      update
      }
}

}

#Routine to delete display list
proc delete_list { list flag } {
    global Nv_

if {$list == "legend" && $Nv_(leg_on) == 0} {
	Ndelete_list legend 0
}

if {$list == "label"} {
	if {$flag == 1} {
		Ndelete_list label 1
	} else {
		Ndelete_list label 0
	}
}




}

# Routine to place legend
proc place_legend { } {
    global Nv_
    global x1 y1 x2 y2

#do bindings
bind $Nv_(TOP).canvas <Button-1> {do_legend %W %x %y 1 }
bind $Nv_(TOP).canvas <Button-3> {do_legend %W %x %y 2 }

#set checkbutton to on
set Nv_(leg_on) 1

puts "Select Legend Corners in Window ..."
puts "Corner 1 = left button      Corner 2 = right button"
update

##Tried binding to draw rectangle outline but unsupported with togl ??
#bind $Nv_(TOP).canvas <Motion> {do_legend %W %x %y 3}
}


# Routines to allow user to place a label
proc place_label { } {
    global Nv_
    # We bind the canvas area so that the user can click to place the
    # label.  After the click is processed we unbind the canvas area

bind $Nv_(TOP).canvas <Button-1> {place_label_cb %x %y }

}


proc place_label_cb { sx sy } {
    global Nv_ 

set sy [expr $Nv_(height) - $sy]

#get font description
	if {$Nv_(labelFontWeight2) == 1} {
		set weight "bold"
	} else {
		set weight "medium"
	}
	if {$Nv_(labelFontWeight1) == 1} {
		set slant "i"
	} else {
		set slant "r"
	}	
	set font "*-$Nv_(labelFontType)-$weight-$slant-normal--$Nv_(labelFontSize)-*-*-*-*-*-*-*"

        set clr $Nv_(labelFontColor)

	Nplace_label $Nv_(label_text) $font $Nv_(labelFontSize) $clr $sx $sy



#remove binding
bind $Nv_(TOP).canvas <Button-1> {}
}





