
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
	if { $opt($id,icon) == "cross" } {
	    $leg create line $x1 $yc $x2 $yc -fill $opt($id,color)
	    $leg create line $xc $y1 $xc $y2 -fill $opt($id,color)
	} 
        if { $opt($id,icon) == "box" } {
	    $leg create rectangle $x1 $y1 $x2 $y2 -outline $opt($id,color)
	}
    }
    # line    
    if { $opt($id,type_line) || $opt($id,type_boundary) } {
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
    set opt($count,display_attr) 1
    set opt($count,type_point) 1 
    set opt($count,type_line) 1 
    set opt($count,type_boundary) 1 
    set opt($count,type_centroid) 1 
    set opt($count,type_area) 0 

    set opt($count,color) \#ffffff
    set opt($count,fcolor) \#ffffff 
    set opt($count,lcolor) \#ffffff

    set opt($count,icon) "cross"
    set opt($count,size) 5 

    set opt($count,field) 1 
    set opt($count,lfield) 1 
    set opt($count,cat) "" 
    set opt($count,where) "" 

    set opt($count,attribute) "" 

    set opt($count,_query_attr) 0 

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

# display vector options
proc DmVector::options { id frm } {
    variable opt

    # vector name
    set row [ frame $frm.name ]
    Button $row.a -text "Vector name:" \
           -command "DmVector::select_map $id"
    Entry $row.b -width 40 -text "$opt($id,map)" \
          -textvariable DmVector::opt($id,map)
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # display
    set row [ frame $frm.disp ]
    Label $row.a -text "Display:"
    checkbutton $row.b -text "shape" -variable DmVector::opt($id,display_shape) \
                -command "DmVector::legend $id"
    checkbutton $row.c -text "category" -variable DmVector::opt($id,display_cat) \
                -command "DmVector::legend $id"
    checkbutton $row.d -text "topology" -variable DmVector::opt($id,display_topo) \
                -command "DmVector::legend $id"
    checkbutton $row.e -text "direction" -variable DmVector::opt($id,display_dir) \
                -command "DmVector::legend $id"
    checkbutton $row.f -text "attribute" -variable DmVector::opt($id,display_attr) \
                -command "DmVector::legend $id"
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # type
    set row [ frame $frm.type ]
    Label $row.a -text "Type:"
    checkbutton $row.b -text "point" -variable DmVector::opt($id,type_point) \
                -command "DmVector::legend $id"
    checkbutton $row.c -text "line" -variable DmVector::opt($id,type_line) \
                -command "DmVector::legend $id"
    checkbutton $row.d -text "boundary" -variable DmVector::opt($id,type_boundary) \
                -command "DmVector::legend $id"
    checkbutton $row.e -text "centroid" -variable DmVector::opt($id,type_centroid)\
                -command "DmVector::legend $id"
    checkbutton $row.f -text "area" -variable DmVector::opt($id,type_area) \
                -command "DmVector::legend $id"
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # color
    set row [ frame $frm.color ]
    Label $row.a -text "Line color:" 
    SelectColor $row.b -type menubutton -variable DmVector::opt($id,color) \
                -command "DmVector::legend $id"
    Label $row.c -text "Fill color:" 
    SelectColor $row.d -type menubutton -variable DmVector::opt($id,fcolor) \
                -command "DmVector::legend $id"
    Label $row.e -text "Label color:" 
    SelectColor $row.f -type menubutton -variable DmVector::opt($id,lcolor) \
                -command "DmVector::legend $id"

    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # point icon / size
    set row [ frame $frm.icon ]
    ComboBox $row.a -label "Icon:" \
                    -width 5  -textvariable DmVector::opt($id,icon) \
                    -values {"cross" "box"} -modifycmd "DmVector::legend $id"
    Label $row.b -text "Size:" 
    SpinBox $row.c -range {1 50 1} -textvariable DmVector::opt($id,size) \
                   -width 2 -helptext "Icon size" -modifycmd "DmVector::legend $id"
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # field
    set row [ frame $frm.field ]
    LabelEntry $row.a -label "Field" -textvariable DmVector::opt($id,field) -width 5
    LabelEntry $row.b -label "Label Field" -textvariable DmVector::opt($id,lfield) -width 5
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # attribute column
    set row [ frame $frm.attribute ]
    LabelEntry $row.a -label "Attribute col" -textvariable DmVector::opt($id,attribute) -width 40
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # category
    set row [ frame $frm.cat ]
    LabelEntry $row.a -label "Category" -textvariable DmVector::opt($id,cat) \
               -width 40
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # where
    set row [ frame $frm.where ]
    LabelEntry $row.a -label "SQL query" -textvariable DmVector::opt($id,where) \
               -width 40
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # query
    set row [ frame $frm.query ]
    checkbutton $row.a -text "Query with attributes" \
                -variable DmVector::opt($id,_query_attr) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

}

proc DmVector::save { tree depth node } {
    variable opt
    
    set id [Dm::node_id $node]


    foreach key { _check map display_shape display_cat display_topo display_dir display_attr
                  type_point type_line type_boundary type_centroid type_area 
                  color fcolor lcolor icon size field lfield attribute cat where 
                  _query_attr } {
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
         !$opt($id,type_area) } { return } 

    set cmd "d.vect map=$opt($id,map)"

    # color
    set color [DmVector::color $opt($id,color)]
    set fcolor [DmVector::color $opt($id,fcolor)]
    set lcolor [DmVector::color $opt($id,lcolor)]
    append cmd " color=$color fcolor=$fcolor lcolor=$lcolor" 

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
    foreach t { point line boundary centroid area } {
       if { $opt($id,type_$t) } { lappend tlist $t }
    }
    set type [join $tlist , ]
    append cmd " type=$type"

    append cmd " icon=$opt($id,icon) size=$opt($id,size)" 

    if { $opt($id,field) != "" } { 
        append cmd " field=$opt($id,field)" 
    } 
    if { $opt($id,attribute) != "" } { 
        append cmd " att=$opt($id,attribute)" 
    } 
    if { $opt($id,lfield) != "" } { 
        append cmd " lfield=$opt($id,lfield)" 
    } 
    if { $opt($id,cat) != "" } { 
        append cmd " cat=$opt($id,cat)" 
    } 
    if { $opt($id,where) != "" } { 
        append cmd " where=\"$opt($id,where)\"" 
    } 

    Dm::execute $cmd
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
         !$opt($id,type_area) } { return } 

    set cmd "d.what.vect map=$opt($id,map)"
    if { $opt($id,_query_attr) } { 
        append cmd " -a" 
    } 

    Dm::execute $cmd
}
