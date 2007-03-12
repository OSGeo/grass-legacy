
if {[array get env GISBASE] == ""} {
    puts stderr "You must be in GRASS GIS to run this program."
    exit 1
}

if {$tcl_platform(platform) == "windows"} {
	set stderr NUL:
} else {
	set stderr @stderr
}

set outmap $env(GIS_OPT_OUTPUT)
set inmap $env(GIS_OPT_INPUT)
set width $env(GIS_OPT_WIDTH)
set height $env(GIS_OPT_HEIGHT)
set size $env(GIS_OPT_SIZE)

proc load_map {map} {
	global wind rows cols values changed colors
	global stderr

	set infile [open "|r.out.ascii --q input=$map 2>$stderr" r]
	regexp {^north: *([0-9]+)$} [gets $infile] dummy wind(N)
	regexp {^south: *([0-9]+)$} [gets $infile] dummy wind(S)
	regexp {^east: *([0-9]+)$}  [gets $infile] dummy wind(E)
	regexp {^west: *([0-9]+)$}  [gets $infile] dummy wind(W)
	regexp {^rows: *([0-9]+)$}  [gets $infile] dummy rows
	regexp {^cols: *([0-9]+)$}  [gets $infile] dummy cols

	for {set row 0} {$row < $rows} {incr row} {
		gets $infile line
		set col 0
		foreach elem $line {
			set values($row,$col) $elem
			set changed($row,$col) 0
			incr col
		}
	}

	close $infile

	set tempbase [exec g.tempfile pid=[pid]]
	set tempfile $tempbase.ppm
	exec r.out.ppm --q input=$map output=$tempfile 2>$stderr

	image create photo colorimg -file $tempfile

	file delete $tempbase
	file delete $tempfile

	for {set row 0} {$row < $rows} {incr row} {
		for {set col 0} {$col < $cols} {incr col} {
			set val $values($row,$col)
			if {[array get colors $val] != ""} continue
			set pix [colorimg get $col $row]
			set r [lindex $pix 0]
			set g [lindex $pix 1]
			set b [lindex $pix 2]
			set color [format "#%02x%02x%02x" $r $g $b]
			set colors($val) $color
		}
	}

	image delete colorimg
}

proc save_map {map unchanged} {
	global wind rows cols values changed inmap
	global stderr

	set outfile [open "|r.in.ascii --q --o input=- output=$map 2>$stderr" w]

	puts $outfile "north: $wind(N)"
	puts $outfile "south: $wind(S)"
	puts $outfile "east: $wind(E)"
	puts $outfile "west: $wind(W)"
	puts $outfile "rows: $rows"
	puts $outfile "cols: $cols"

	for {set row 0} {$row < $rows} {incr row} {
		for {set col 0} {$col < $cols} {incr col} {
			if {$col > 0} {
				puts -nonewline $outfile " "
			}
			if {$unchanged || $changed($row,$col)} {
				puts -nonewline $outfile "$values($row,$col)"
			} else {
				puts -nonewline $outfile "*"
			}
		}
		puts $outfile ""
	}

	close $outfile

	exec r.colors --q $map rast=$inmap 2>$stderr
}

proc get_color {val} {
	global colors

	if {[array get colors $val] == ""} {
		return "#ffffff"
	} else {
		return $colors($val)
	}
}

proc brush_update {} {
	global brush colors

	if {$brush == "*"} {
		.tools.color configure -bitmap gray12 -foreground black
	} else {
		.tools.color configure -bitmap gray75 -foreground [get_color $brush]
	}
}

proc current_cell {} {
	global canvas

	set row ""
	set col ""

	set tags [$canvas itemcget current -tags]

	foreach tag $tags {
		if {[regexp {row-([0-9]+)} $tag dummy r]} {set row $r}
		if {[regexp {col-([0-9]+)} $tag dummy c]} {set col $c}
	}

	return [list $row $col]
}

proc cell_enter {} {
	global status
	global wind rows cols values

	set pos [current_cell]
	set row [lindex $pos 0]
	set col [lindex $pos 1]

	if {$row == "" || $col == ""} return

	set status(row) $row
	set status(col) $col
	set status(x) [expr {$wind(E) + ($col + 0.5) * ($wind(E) - $wind(W)) / $cols}]
	set status(y) [expr {$wind(N) - ($row + 0.5) * ($wind(N) - $wind(S)) / $rows}]
	set status(value) $values($row,$col)
}

proc cell_leave {} {
	global status

	set status(row) ""
	set status(col) ""
	set status(x) ""
	set status(y) ""
	set status(value) ""
}

proc cell_get {} {
	global brush values colors

	set pos [current_cell]
	set row [lindex $pos 0]
	set col [lindex $pos 1]

	set brush $values($row,$col)

	brush_update
}

proc cell_set {} {
	global canvas brush values changed colors

	set pos [current_cell]
	set row [lindex $pos 0]
	set col [lindex $pos 1]
	set val $brush

	set values($row,$col) $val
	set changed($row,$col) 1

	set cell [$canvas find withtag "(cell&&row-$row&&col-$col)"]

	if {$val == "*"} {
		set fill black
		set stipple gray12 
	} else {
		set fill [get_color $val]
		set stipple ""
	}

	$canvas itemconfigure $cell -outline white -fill $fill -stipple $stipple
}

proc make_canvas {} {
	global canvas rows cols values colors
	global size width height

	set cx [expr $width  / $cols]
	set cy [expr $height / $rows]

	if {$cx < $size} {set cx $size}
	if {$cy < $size} {set cy $size}

	set canvas .canvas

	set w [expr $cols * $cx]
	set h [expr $rows * $cy]

	canvas $canvas -width $width -height $height -scrollregion [list 0 0 $w $h] \
	    -xscrollcommand {.xscroll set} -yscrollcommand {.yscroll set}

	scrollbar .xscroll -orient horizontal -command {$canvas xview}
	scrollbar .yscroll -orient vertical   -command {$canvas yview}

	for {set row 0} {$row < $rows} {incr row} {
		for {set col 0} {$col < $cols} {incr col} {
			set x0 [expr $col * $cx + 1]
			set x1 [expr $x0 + $cx - 1]
			set y0 [expr $row * $cy + 1]
			set y1 [expr $y0 + $cy - 1]
			if {$values($row,$col) == "*"} {
				set color black
				set stipple gray12
			} else {
				set color $colors($values($row,$col))
				set stipple ""
			}

			$canvas create polygon $x0 $y0 $x1 $y0 $x1 $y1 $x0 $y1 \
			    -fill $color -stipple $stipple \
			    -outline black -activeoutline red \
			    -tags [list cell row-$row col-$col]
		}
	}

	$canvas bind cell <Any-Enter> { cell_enter }
	$canvas bind cell <Any-Leave> { cell_leave }

	$canvas bind cell <Button-1> { cell_set }
	$canvas bind cell <Button-3> { cell_get }

	bind $canvas <Any-Leave> { cell_leave }
}

proc make_ui {} {
	global canvas

	wm title . "d.rast.edit"

	menu .menu -tearoff 0
	menu .menu.file -tearoff 0
	.menu add cascade -label "File" -menu .menu.file -underline 0
	.menu.file add command -label "Save" -underline 0 -command {save_map $outmap 1}
	.menu.file add command -label "Save Changes" -underline 5 -command {save_map $outmap 0}
	.menu.file add command -label "Exit" -underline 1 -command {destroy .}

	. configure -menu .menu

	frame .status
	label .status.row_l -text "Row:"
	entry .status.row -textvariable status(row) -width 6
	label .status.col_l -text "Col:"
	entry .status.col -textvariable status(col) -width 6
	label .status.x_l -text "X:"
	entry .status.x -textvariable status(x) -width 10
	label .status.y_l -text "Y:"
	entry .status.y -textvariable status(y) -width 10
	label .status.value_l -text "Value:"
	entry .status.value -textvariable status(value) -width 10

	pack \
	    .status.row_l .status.row \
	    .status.col_l .status.col \
	    .status.x_l .status.x \
	    .status.y_l .status.y \
	    .status.value_l .status.value \
	    -side left

	frame .tools
	label .tools.value_l -text "Value:"
	entry .tools.value -textvariable brush
	label .tools.color_l -text "Color:"
	label .tools.color -bitmap gray12 -foreground black

	pack \
	    .tools.value_l .tools.value \
	    .tools.color_l .tools.color \
	    -side left

	bind .tools.value <KeyPress-Return> brush_update

	grid $canvas .yscroll -sticky nsew
	grid .xscroll -sticky nsew
	grid .status  -sticky nsew
	grid .tools  -sticky nsew

	grid rowconfigure    . 0 -weight 1
	grid columnconfigure . 0 -weight 1
}

set status(row) ""
set status(col) ""
set status(x) ""
set status(y) ""
set status(value) ""

set brush "*"

load_map $inmap
make_canvas
make_ui

