# Symbology table
proc color2rgb { color } {
    regexp -- {#(..)(..)(..)} $color x r g b
    set r [expr 0x$r ]
    set g [expr 0x$g ]
    set b [expr 0x$b ]
    return [list $r $g $b]
}

proc set_color { col color } {
    set clr [ color2rgb $color] 
    c_set_color $col [lindex $clr 0]  [lindex $clr 1]  [lindex $clr 2]
}

proc set_on { code on } {
    c_set_on $code $on
}

# create settings window
proc settings {} {
    global symb, snap_mode, snap_screen, snap_map
    set clw 30

    if { [winfo exists .settings] } {
        puts "Settings already opened"
        wm deiconify .settings
        raise .settings
        return
    } 
    set stt [toplevel .settings]
 
    set nb [NoteBook $stt.nb]  
    
    # --- Symbology ---
    set symbf [$nb insert end colors -text "Symbology"]

    # Background
    set row [ frame $symbf.row1 ]
    Label $row.a -anchor w -width $clw -text "Background"  
    SelectColor $row.b -type menubutton -variable symb(background,color) \
                -command { set_color background $symb(background,color) }
    pack $row.a -side left; pack $row.b -side right; pack $row -side top -fill x -expand yes

    # Highlight
    set row [ frame $symbf.row2 ]
    Label $row.a -anchor w -width $clw -text "Highlight"  
    SelectColor $row.b -type menubutton -variable symb(highlight,color) \
                -command { set_color highlight $symb(highlight,color) }
    pack $row.a -side left; pack $row.b -side right; pack $row -side top -fill x -expand yes

    # Point
    set row [ frame $symbf.row3 ]
    Label $row.a -anchor w -width $clw -text "Point"  
    checkbutton $row.b -variable symb(point,on) -height 1 -padx 0 -width 0 \
                -command { set_on point $symb(point,on) }
    SelectColor $row.c -type menubutton -variable symb(point,color) \
                -command { set_color point $symb(point,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    # Line
    set row [ frame $symbf.row4 ]
    Label $row.a -anchor w -width $clw -text "Line"  
    checkbutton $row.b -variable symb(line,on) -height 1 -padx 0 -width 0 \
                -command { set_on line $symb(line,on) }
    SelectColor $row.c -type menubutton -variable symb(line,color) \
                -command { set_color line $symb(line,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    # Boundary
    set row [ frame $symbf.row5 ]
    Label $row.a -anchor w -width $clw -text "Boundary (no area)"  
    checkbutton $row.b -variable symb(boundary_0,on) -height 1 -padx 0 -width 0 \
                -command { set_on boundary_0 $symb(boundary_0,on) }
    SelectColor $row.c -type menubutton -variable symb(boundary_0,color) \
                -command { set_color boundary_0 $symb(boundary_0,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    set row [ frame $symbf.row6 ]
    Label $row.a -anchor w -width $clw -text "Boundary (1 area)"  
    checkbutton $row.b -variable symb(boundary_1,on) -height 1 -padx 0 -width 0 \
                -command { set_on boundary_1 $symb(boundary_1,on) }
    SelectColor $row.c -type menubutton -variable symb(boundary_1,color) \
                -command { set_color boundary_1 $symb(boundary_1,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    set row [ frame $symbf.row7 ]
    Label $row.a -anchor w -width $clw -text "Boundary (2 areas)"  
    checkbutton $row.b -variable symb(boundary_2,on) -height 1 -padx 0 -width 0 \
                -command { set_on boundary_2 $symb(boundary_2,on) }
    SelectColor $row.c -type menubutton -variable symb(boundary_2,color) \
                -command { set_color boundary_2 $symb(boundary_2,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    # Centroid
    set row [ frame $symbf.row8 ]
    Label $row.a -anchor w -width $clw -text "Centroid (in area)"  
    checkbutton $row.b -variable symb(centroid_in,on) -height 1 -padx 0 -width 0 \
                -command { set_on centroid_in $symb(centroid_in,on) }
    SelectColor $row.c -type menubutton -variable symb(centroid_in,color) \
                -command { set_color centroid_in $symb(centroid_in,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    set row [ frame $symbf.row9 ]
    Label $row.a -anchor w -width $clw -text "Centroid (outside area)"  
    checkbutton $row.b -variable symb(centroid_out,on) -height 1 -padx 0 -width 0 \
                -command { set_on centroid_out $symb(centroid_out,on) }
    SelectColor $row.c -type menubutton -variable symb(centroid_out,color) \
                -command { set_color centroid_out $symb(centroid_out,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    set row [ frame $symbf.row10 ]
    Label $row.a -anchor w -width $clw -text "Centroid (duplicate in area)"  
    checkbutton $row.b -variable symb(centroid_dupl,on) -height 1 -padx 0 -width 0 \
                -command { set_on centroid_dupl $symb(centroid_dupl,on) }
    SelectColor $row.c -type menubutton -variable symb(centroid_dupl,color) \
                -command { set_color centroid_dupl $symb(centroid_dupl,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    # Node
    set row [ frame $symbf.row11 ]
    Label $row.a -anchor w -width $clw -text "Node (1 line)"  
    checkbutton $row.b -variable symb(node_1,on) -height 1 -padx 0 -width 0 \
                -command { set_on node_1 $symb(node_1,on) }
    SelectColor $row.c -type menubutton -variable symb(node_1,color) \
                -command { set_color node_1 $symb(node_1,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    set row [ frame $symbf.row12 ]
    Label $row.a -anchor w -width $clw -text "Node (2 lines)"  
    checkbutton $row.b -variable symb(node_2,on) -height 1 -padx 0 -width 0 \
                -command { set_on node_2 $symb(node_2,on) }
    SelectColor $row.c -type menubutton -variable symb(node_2,color) \
                -command { set_color node_2 $symb(node_2,color) }
    pack $row.a -side left; pack $row.c $row.b -side right; pack $row -side top -fill x -expand yes

    # --- Settings ---
    set setf [$nb insert end settings -text "Settings"]

    # Snapping threshold
    set row [ frame $setf.row1 ]

    Label $row.a -anchor w -text "Snapping threshold in screen pixels"  
    radiobutton $row.b -variable snap_mode -value 0 -height 1 -padx 0 -width 0 \
                -command { c_set_snap snap_mode $snap_mode }
    Entry $row.c -width 10 -text "" -textvariable snap_screen -command { c_set_snap snap_screen $snap_screen } 
    pack $row.a -side left; pack $row.c $row.b -side right; 
    pack $row -side top -fill x -expand no -anchor n 
    
    set row [ frame $setf.row2 ]
    Label $row.a -anchor w -text "Snapping threshold in map units"  
    radiobutton $row.b -variable snap_mode -value 1 -height 1 -padx 0 -width 0 \
                -command { c_set_snap snap_mode $snap_mode }
    Entry $row.c -width 10 -text "" -textvariable snap_map -command { c_set_snap snap_map $snap_map }
    pack $row.a -side left; pack $row.c $row.b -side right; 
    pack $row -side top -fill x -expand no -anchor n 

    # -- pack notebook --
    pack $nb -fill both -expand yes -padx 1 -pady 1
    $nb raise [$nb page 0]

    # --- Bar ---
    #Button $stt.exit -text "Test"  -command { puts "bg color = $symb(background,color)" }
    #pack $stt.exit -side left -anchor w

    tkwait visibility $stt
}


