
namespace eval DmVector {
    variable array opt # vector options
    variable count 1
}


proc DmVector::legend { id } {
    variable opt
   
    set lh $DmTree::legend_height
    set lw $DmTree::legend_width
    set mar 2
    set leg $opt($id,_legend)

    $leg delete all

    # point 
    set xc [expr $lw / 6 + 1 ]
    set yc [expr $lh / 2 ]
    set size $opt($id,size)
   
    set maxpsize  [expr $lw / 3 - 2 ]
    if { $size > $maxpsize } { set size $maxpsize }
    set x1 [expr $xc - $size / 2 ]
    set x2 [expr $xc + $size / 2 + 1 ]
    set y1  [expr $yc - $size / 2 ]
    set y2  [expr $yc + $size / 2 + 1 ]

    if { $opt($id,type_point) || $opt($id,type_centroid) } {
        $leg create line $x1 $yc $x2 $yc -fill $opt($id,color)
	$leg create line $xc $y1 $xc $y2 -fill $opt($id,color)
    }
    # line    
    if { $opt($id,type_line) || $opt($id,type_boundary) || $opt($id,type_face) } {
	set x1 [expr $lw / 3 + $mar ]
	set x2 [expr 2 * $lw / 3 - $mar ]
	set y1 [expr $lh - $mar ]
	set y2 [expr $mar ]
        $leg create line $x1 $y1 $x2 $y2 -fill $opt($id,color)
    }
    # area    
    if { $opt($id,type_area) } {
	set x1 [expr 2 * $lw / 3 + $mar ]
	set x2 [expr $lw - $mar ]
	set y1 [expr $mar ]
	set y2 [expr $lh - $mar ]
	$leg create rectangle $x1 $y1 $x2 $y2 -outline $opt($id,color) \
                              -fill $opt($id,fcolor)
    }
}

proc DmVector::create { tree parent } {
    variable opt
    variable count

    set node "vector:$count"

    set frm [ frame .vectoricon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable DmVector::opt($count,_check) \
                           -height 1 -padx 0 -width 0]
    set can [ canvas $frm.c -width $DmTree::legend_width \
                     -height $DmTree::legend_height -borderwidth 0 \
                     -highlightbackground gray ]
    set opt($count,_legend) $can
    pack $check $can -side left

    $tree insert end $parent $node \
	-text      "vector $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,_check) 1 

    set opt($count,map) "" 
    set opt($count,display_shape) 1 
    set opt($count,display_cat) 0
    set opt($count,display_topo) 0 
    set opt($count,display_dir) 0 
    set opt($count,display_attr) 0
    set opt($count,type_point) 1 
    set opt($count,type_line) 1
    set opt($count,type_boundary) 1
    set opt($count,type_centroid) 1
    set opt($count,type_area) 1
    set opt($count,type_face) 0 

    set opt($count,color) \#000000
    set opt($count,sqlcolor) 0
    set opt($count,rdmcolor) 0
    set opt($count,fcolor) \#AAAAAA 
    set opt($count,lcolor) \#000000
    set opt($count,_use_color) 1
    set opt($count,_use_fcolor) 1

    set opt($count,symdir) "basic"
    set opt($count,icon) "basic/x"
    set opt($count,size) 5 

    set opt($count,field) 1 
    set opt($count,lfield) 1 
    set opt($count,cat) "" 
    set opt($count,where) "" 
    set opt($count,_use_where) 1

    set opt($count,attribute) "" 
    set opt($count,xref) "left"
    set opt($count,yref) "center"
    set opt($count,lsize) 8

    set opt($count,minreg) "" 
    set opt($count,maxreg) "" 

    set opt($count,_query_text) 1 
    set opt($count,_query_edit) 0 

    set opt($count,_width) 1

    DmVector::legend $count

    incr count
    return $node
}

proc DmVector::set_option { node key value } {
    variable opt
 
    set id [Dm::node_id $node]
    set opt($id,$key) $value

    DmVector::legend $id
}

proc DmVector::select_map { id } {
    set m [GSelect vector]
    if { $m != "" } { 
        set DmVector::opt($id,map) $m 
    }
}

# select symbols from directories
proc DmVector::select_symbol { id } {
    variable opt
    set i [GSelect symbol]
    if { $i != "" } {
        set DmVector::opt($id,icon) $i
    }
}

# display vector options
proc DmVector::options { id frm } {
    variable opt

    # vector name
    set row [ frame $frm.name ]
    Button $row.a -text [G_msg "Vector name:"] \
           -command "DmVector::select_map $id"
    Entry $row.b -width 40 -text "$opt($id,map)" \
          -textvariable DmVector::opt($id,map)
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # display
    set row [ frame $frm.disp ]
    Label $row.a -text [G_msg "Display:"]
    checkbutton $row.b -text [G_msg "shape"] -variable DmVector::opt($id,display_shape) \
                -command "DmVector::legend $id"
    checkbutton $row.c -text [G_msg "category"] -variable DmVector::opt($id,display_cat) \
                -command "DmVector::legend $id"
    checkbutton $row.d -text [G_msg "topology"] -variable DmVector::opt($id,display_topo) \
                -command "DmVector::legend $id"
    checkbutton $row.e -text [G_msg "direction"] -variable DmVector::opt($id,display_dir) \
                -command "DmVector::legend $id"
    checkbutton $row.f -text [G_msg "label"] -variable DmVector::opt($id,display_attr) \
                -command "DmVector::legend $id"
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # type
    set row [ frame $frm.type ]
    Label $row.a -text [G_msg "Type:"]
    checkbutton $row.b -text [G_msg "point"] -variable DmVector::opt($id,type_point) \
                -command "DmVector::legend $id"
    checkbutton $row.c -text [G_msg "line"] -variable DmVector::opt($id,type_line) \
                -command "DmVector::legend $id"
    checkbutton $row.d -text [G_msg "boundary"] -variable DmVector::opt($id,type_boundary) \
                -command "DmVector::legend $id"
    checkbutton $row.e -text [G_msg "centroid"] -variable DmVector::opt($id,type_centroid)\
                -command "DmVector::legend $id"
    checkbutton $row.f -text [G_msg "area"] -variable DmVector::opt($id,type_area) \
                -command "DmVector::legend $id"
    checkbutton $row.g -text [G_msg "face"] -variable DmVector::opt($id,type_face) \
                -command "DmVector::legend $id"
    pack $row.a $row.b $row.c $row.d $row.e $row.f $row.g -side left
    pack $row -side top -fill both -expand yes

    # point icon / size
    set row [ frame $frm.icon ]
 #  ComboBox $row.a -label [G_msg "Symbol:"] \
 #        -width 20  -textvariable DmVector::opt($id,icon) \
 #        -values {"basic/cross" "basic/circle" "basic/box" "basic/diamond"} \
 #        -modifycmd "DmVector::legend $id"
    
 #   ComboBox $row.e -label [G_msg "Symbol collection:"] \
 #       -width 10 -textvariable DmVector::opt($id,symdir)  \
 #	 -values {"basic" "demo"} \
 #	 -modifycmd "DmVector::legend $id"
  
    Button $row.a -text [G_msg "Symbol:"] \
	-command "DmVector::select_symbol $id"
    Entry $row.d -width 15 -text "$opt($id,icon)" \
	-textvariable DmVector::opt($id,icon)
    pack $row.a $row.d -side left
    pack $row -side top -fill both -expand yes

    Label $row.b -text "Size:" 
    SpinBox $row.c -range {1 50 1} -textvariable DmVector::opt($id,size) \
                   -width 2 -helptext "Icon size" -modifycmd "DmVector::legend $id"
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # color
    set row [ frame $frm.color ]
    Label $row.a -text [G_msg "Line color:"] 
    SelectColor $row.b -type menubutton -variable DmVector::opt($id,color) \
                -command "DmVector::legend $id"
    checkbutton $row.c -text [G_msg "show lines"] -variable DmVector::opt($id,_use_color) \
                -command "DmVector::legend $id"
    Label $row.d -text [G_msg " Fill color:"] 
    SelectColor $row.e -type menubutton -variable DmVector::opt($id,fcolor) \
                -command "DmVector::legend $id"
    checkbutton $row.f -text [G_msg "fill areas"] -variable DmVector::opt($id,_use_fcolor) \
                -command "DmVector::legend $id"
 
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # multi fill colors
    set row [ frame $frm.multicolor ]
    checkbutton $row.a -text [G_msg "Random fill colors for each cat value"] -variable DmVector::opt($id,rdmcolor) \
                -command "DmVector::legend $id"
    checkbutton $row.b -text [G_msg " GRASSRGB column for fill color"] -variable DmVector::opt($id,sqlcolor) \
                -command "DmVector::legend $id"
 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # field
    set row [ frame $frm.field ]
    Label $row.a -text [G_msg "Label color:"] 
    SelectColor $row.b -type menubutton -variable DmVector::opt($id,lcolor) \
                -command "DmVector::legend $id"
    Label $row.c -text [G_msg " Label size:"] 
    SpinBox $row.d -range {1 50 1} -textvariable DmVector::opt($id,lsize) \
                   -width 2 -helptext [G_msg "Label size"] -modifycmd "DmVector::legend $id"
    ComboBox $row.e -label [G_msg " Label xpos"] \
                    -width 6  -textvariable DmVector::opt($id,xref) \
                    -values {"left" "center" "right"} \
                    -modifycmd "DmVector::legend $id"
    ComboBox $row.f -label [G_msg " Label ypos"] \
                    -width 6  -textvariable DmVector::opt($id,yref) \
                    -values {"top" "center" "bottom"} \
                    -modifycmd "DmVector::legend $id"
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # label attribute column
    set row [ frame $frm.attribute ]
    LabelEntry $row.a -label [G_msg "Layer for labels"] -textvariable DmVector::opt($id,lfield) -width 5
    LabelEntry $row.b -label [G_msg " Attribute col for labels"] -textvariable DmVector::opt($id,attribute) -width 24
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # category
    set row [ frame $frm.cat ]
    LabelEntry $row.a -label [G_msg "Layer for query"] -textvariable DmVector::opt($id,field) -width 5
    LabelEntry $row.b -label [G_msg " Category query"] -textvariable DmVector::opt($id,cat) \
               -width 30
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # where
    set row [ frame $frm.where ]
    LabelEntry $row.a -label [G_msg "SQL query"] -textvariable DmVector::opt($id,where) \
               -width 42
    checkbutton $row.b -text [G_msg " use query"] -variable DmVector::opt($id,_use_where) \
                -command "DmVector::legend $id"
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # query
    set row [ frame $frm.query ]
    checkbutton $row.a -text [G_msg "Print query output as text in terminal"] \
                -variable DmVector::opt($id,_query_text) 
    checkbutton $row.b -text [G_msg "Edit attributes"] \
                -variable DmVector::opt($id,_query_edit) 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # display only in limited region size range
    set row [ frame $frm.region ]
    Label $row.a -text [G_msg "Display constraints:"]
    LabelEntry $row.b -label "Min" -textvariable DmVector::opt($id,minreg) -width 8
    LabelEntry $row.c -label "Max" -textvariable DmVector::opt($id,maxreg) -width 8
    Label $row.d -text [G_msg "region size"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # Width
    set row [ frame $frm.print ]
    Label $row.a -text [G_msg "Width (print):"] 
    SpinBox $row.b -range {1 100 1} -textvariable DmVector::opt($id,_width) \
                   -width 2 -helptext [G_msg "Line width used for printing"] 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc DmVector::save { tree depth node } {
    variable opt
    
    set id [Dm::node_id $node]


    foreach key { _check map display_shape display_cat display_topo display_dir display_attr
                  type_point type_line type_boundary type_centroid type_area type_face
                  color _use_color fcolor _use_fcolor lcolor rdmcolor sqlcolor icon size field lfield attribute
                  xref yref lsize cat where _query_text _query_edit _use_where minreg maxreg _width } {
        Dm::rc_write $depth "$key $opt($id,$key)"

    } 
}

proc DmVector::color { color } {
    
    regexp -- {#(..)(..)(..)} $color x r g b

    set r [expr 0x$r ]
    set g [expr 0x$g ]
    set b [expr 0x$b ]

    return "$r:$g:$b"
}

proc DmVector::display { node } {
    variable opt
    
    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    if { !$opt($id,display_shape) && !$opt($id,display_cat) &&
         !$opt($id,display_topo)  && !$opt($id,display_dir) &&
         !$opt($id,display_attr) } { return } 

    if { !$opt($id,type_point) && !$opt($id,type_line) &&
         !$opt($id,type_boundary)  && !$opt($id,type_centroid) && 
         !$opt($id,type_area) && !$opt($id,type_face) } { return } 

    set cmd "d.vect map=$opt($id,map)"

    # color
    if { $opt($id,rdmcolor) } { append cmd " -c" }
    if { $opt($id,sqlcolor) } { append cmd " -a" }
    set color [DmVector::color $opt($id,color)]
    set fcolor [DmVector::color $opt($id,fcolor)]
    set lcolor [DmVector::color $opt($id,lcolor)]

    if { $opt($id,_use_color) } { append cmd " color=$color" } { append cmd " color=none" }
    append cmd " lcolor=$lcolor" 

    if { $opt($id,_use_fcolor) } { append cmd " fcolor=$fcolor" } { append cmd " fcolor=none" }

    # display
    set dlist [list]
    foreach d { shape cat topo dir } {
       if { $opt($id,display_$d) } { lappend dlist $d }
    }
    if { $opt($id,display_attr) && $opt($id,attribute) != "" } { lappend dlist attr }
    
    set display [join $dlist , ]
    append cmd " display=$display"

    # type
    set tlist [list]
    foreach t { point line boundary centroid area face } {
       if { $opt($id,type_$t) } { lappend tlist $t }
    }
    set type [join $tlist , ]
    append cmd " type=$type"

    append cmd " icon=$opt($id,icon) size=$opt($id,size)" 

    if { $opt($id,field) != "" } { 
        append cmd " layer=$opt($id,field)" 
    } 
    if { $opt($id,attribute) != "" && $opt($id,display_attr) } { 
        append cmd " {att=$opt($id,attribute)}" 
    } 
    append cmd " lsize=$opt($id,lsize)" 
    
    append cmd " xref=$opt($id,xref) yref=$opt($id,yref)"

    if { $opt($id,lfield) != "" } { 
        append cmd " llayer=$opt($id,lfield)" 
    } 
    if { $opt($id,cat) != "" } { 
        append cmd " cat=$opt($id,cat)" 
    } 
    if { $opt($id,where) != "" && $opt($id,_use_where) } { 
        append cmd " {where=$opt($id,where)}" 
    } 
    if { $opt($id,minreg) != "" } { 
        append cmd " minreg=$opt($id,minreg)" 
    } 
    if { $opt($id,maxreg) != "" } { 
        append cmd " maxreg=$opt($id,maxreg)" 
    } 

    run $cmd
}

proc DmVector::print { file node } {
    variable opt

    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! $opt($id,_check) } { return } 
    if { $opt($id,map) == "" } { return } 

    if { $opt($id,display_cat) || $opt($id,display_topo) || 
         $opt($id,display_dir) || $opt($id,display_attr) 
    } { puts "At least one of selected display options for vector is not supported for PS"  }

    if { ! $opt($id,display_shape) } { return } 

    set color [DmVector::color $opt($id,color)]
    set fcolor [DmVector::color $opt($id,fcolor)]

    # Points
    if { $opt($id,type_point) || $opt($id,type_centroid) } {
        puts $file "vpoints $opt($id,map)"

        set str "  type"
        if { $opt($id,type_point) } { append str " point" }
        if { $opt($id,type_centroid) } { append str " centroid" }
        puts $file $str

	if { $opt($id,field) != "" } { puts $file "  layer $opt($id,field)" }
	if { $opt($id,cat) != "" }   { puts $file "  cats $opt($id,cat)" }
	if { $opt($id,where) != "" } { puts $file "  where $opt($id,where)" } 

	    if { $opt($id,_use_color) } { 
	    puts $file "  color $color"
        } else { 
	    puts $file "  color none"
        }

	#puts $file "width $opt($id,ps_width)"

        if { $opt($id,_use_fcolor) } { 
	    puts $file "  fcolor $fcolor"
        } else { 
	    puts $file "  fcolor none"
        }

        puts $file "  symbol $opt($id,icon)"
        puts $file "  size $opt($id,size)"

       # confuses ps.map:
       # puts $file "  xref $opt($id,xref)"
       # puts $file "  yref $opt($id,yref)"

	puts $file "end"
    } 

    # Lines
    if { $opt($id,type_line) || $opt($id,type_boundary) } {
        puts $file "vlines $opt($id,map)"

        set str "  type"
        if { $opt($id,type_line) } { append str " line" }
        if { $opt($id,type_boundary) } { append str " boundary" }
        puts $file $str

	if { $opt($id,field) != "" } { puts $file "  layer $opt($id,field)" }
	if { $opt($id,cat) != "" }   { puts $file "  cats $opt($id,cat)" }
	if { $opt($id,where) != "" } { puts $file "  where $opt($id,where)" } 

        if { $opt($id,_use_color) } { 
	    puts $file "  color $color"
        } else { 
	    puts $file "  color none"
        }

	puts $file "width $opt($id,_width)"

	puts $file "  hcolor NONE"

	puts $file "end"
    } 

    # Areas
    if { $opt($id,type_area) } {
        puts $file "vareas $opt($id,map)"

	if { $opt($id,field) != "" } { puts $file "  layer $opt($id,field)" }
	if { $opt($id,cat) != "" }   { puts $file "  cats $opt($id,cat)" }
	if { $opt($id,where) != "" } { puts $file "  where $opt($id,where)" } 

        if { $opt($id,_use_color) } { 
	    puts $file "  color $color"
        } else { 
	    puts $file "  color none"
        }

	puts $file "width $opt($id,_width)"

        if { $opt($id,_use_fcolor) } { 
	    puts $file "  fcolor $fcolor"
        } else { 
	    puts $file "  fcolor none"
        }

	puts $file "end"
    } 
}

proc DmVector::query { node } {
    variable opt
    
    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    if { !$opt($id,display_shape) && !$opt($id,display_cat) &&
         !$opt($id,display_topo)  && !$opt($id,display_dir) &&
         !$opt($id,display_attr) } { return } 

    if { !$opt($id,type_point) && !$opt($id,type_line) &&
         !$opt($id,type_boundary)  && !$opt($id,type_centroid) && 
         !$opt($id,type_area) && !$opt($id,type_face) } { return } 

    set cmd "d.what.vect -f map=$opt($id,map)"
    if { $opt($id,_query_text) && !$opt($id,_query_edit) } { 
        append cmd " -x" 
    } 
    if { $opt($id,_query_edit) } { 
        append cmd " -e" 
    } 

    if { $opt($id,_query_text) && !$opt($id,_query_edit) } {
        term $cmd
    } else {
        spawn $cmd
    }
}

proc DmVector::WorkOnVector { node } {
    variable opt
    
    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    if { !$opt($id,display_shape) && !$opt($id,display_cat) &&
         !$opt($id,display_topo)  && !$opt($id,display_dir) &&
         !$opt($id,display_attr) } { return } 

    if { !$opt($id,type_point) && !$opt($id,type_line) &&
         !$opt($id,type_boundary)  && !$opt($id,type_centroid) && 
         !$opt($id,type_area) && !$opt($id,type_face) } { return } 

    set cmd "v.digit -n map=$opt($id,map)"
    Dm::monitor
    spawn $cmd
}
