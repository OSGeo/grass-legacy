proc dismiss t {
    destroy $t
}

proc scrollEnter canvas {
    global oldFill
    set id [$canvas find withtag current]
    if {[lsearch [$canvas gettags current] text] >= 0} {
	set id [expr {$id-1}]
    }
    set oldFill [lindex [$canvas itemconfig $id -fill] 4]
    if {[winfo depth $canvas] > 1} {
	$canvas itemconfigure $id -fill SeaGreen1
    } else {
	$canvas itemconfigure $id -fill black
	$canvas itemconfigure [expr {$id+1}] -fill white
    }
}

proc scrollLeave canvas {
    global oldFill
    set id [$canvas find withtag current]
    if {[lsearch [$canvas gettags current] text] >= 0} {
	set id [expr {$id-1}]
    }
    $canvas itemconfigure $id -fill $oldFill
    $canvas itemconfigure [expr {$id+1}] -fill black
}

proc scrollButton canvas {
    global oldFill
    set id [$canvas find withtag current]
    if {[lsearch [$canvas gettags current] text] < 0} {
	set id [expr {$id+1}]
    }
    puts stdout "You buttoned at [lindex [$canvas itemconf $id -text] 4]"
}


proc positionWindow w {
    wm geometry $w +300+300
}

proc nextRaster {raster maptype} {
    G_incr_void_ptr $raster [G_raster_size $maptype]
}

proc getRasterValue { raster maptype } {
    if { $maptype == CELL } {
	G_get_raster_value_c $raster
    } else {
	if { $maptype == FLOAT } {
	    G_get_raster_value_f $raster
	} else {
	    G_get_raster_value_d $raster
	}
    }
}

array set widgetFont {
    main   {Helvetica 12}
    bold   {Helvetica 12 bold}
    title  {Helvetica 18 bold}
    status {Helvetica 10}
    vars   {Helvetica 14}
}

set widgetDemo 1
set font $widgetFont(main)

proc showmap {mapname scale} {
    global font

    # Locate the map and open it
    if { [catch {set themapset [G_find_cell $mapname ""]} res] } {
	okbox "Couldn't locate $mapname: $res"
	exit 1
    }
    set dbmap [G_gisdbase]/[G_location]/$themapset/CELL/$mapname
    okbox "Our map: $dbmap"

    set cellfd [G_open_cell_old $mapname $themapset]

    # The GRASS window parameters derived from the current region
    set our_region [new_Cell_head]
    if { [catch {G_get_window $our_region} res] } {
	okbox "G_get_window exception: $res"
    }
    #    set ewr [Cell_head_ew_res_get $our_region]
    #    set zone [Cell_head_zone_get $our_region]
    #    set cols [Cell_head_cols_get $our_region]
    #    set rows [Cell_head_rows_get $our_region]
    
    set cols [G_window_cols]
    set rows [G_window_rows]
    set maptype [G_raster_map_type $mapname $themapset]

    # Set up the display
    set tl .grassmap
    toplevel $tl 
    frame $tl.img
    frame $tl.button
    set c $tl.img.c
    canvas $c -width [expr {$cols+0}] -height [expr {$rows+0}]
    pack $tl.img.c

    label $tl.button.label -text "$dbmap: rows $rows,  cols $cols, type $maptype" 
    pack $tl.button.label -side left
    button $tl.button.dismiss -text "Dismiss" -command "dismiss $tl" -width 10 
    pack $tl.button.dismiss -side right
    pack $tl.img $tl.button -side top -fill x   
    bind $tl <Key-q> "dismiss $tl"
    bind $tl <Key-Escape> "dismiss $tl"

    # Set up a row buffer
    set rowBuffer [G_allocate_raster_buf $maptype]
    set rastptr $rowBuffer

    # Process and close the raster
    for {set row 0} {$row < $rows} {incr row} {
	if { [catch {G_get_raster_row $cellfd $rowBuffer $row $maptype} res] } {
	    okbox "G_get_raster_row exception: $res"
	    exit 1
	}	    
	for {set col 0} {$col < $cols} {incr col} {
	    set rastptr [nextRaster $rastptr $maptype]
	    $c create rectangle $col $row $col $row -fill $rastptr
	}
    }

    G_close_cell $cellfd

}
