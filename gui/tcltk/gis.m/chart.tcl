###############################################################
# chart.tcl - vector chart layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmChart {
    variable array opt # chart options
    variable count 1
}


proc GmChart::create { tree parent } {
    variable opt
    variable count
    global gmpath

    set node "chart:$count"

    set frm [ frame .charticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmChart::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo chartico -file "$gmpath/chart.gif"
    set ico [label $frm.ico -image chartico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "chart $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,_check) 1 
    
    set opt($count,map) "" 
    set opt($count,type_point) 1 
    set opt($count,type_line) 1
    set opt($count,type_boundary) 1
    set opt($count,type_centroid) 1
    set opt($count,type_area) 0
    set opt($count,layer) 1 
    set opt($count,ctype) "pie" 
    set opt($count,columns) "" 
    set opt($count,sizecol) "" 
    set opt($count,csize) 40 
    set opt($count,cscale) 1 
    set opt($count,ocolor) "black" 
    set opt($count,fcolors) "" 
    
    incr count
    return $node
}

proc GmChart::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmChart::select_map { id } {
    variable tree
    variable node
    set m [GSelect vector]
    if { $m != "" } { 
        set GmChart::opt($id,map) $m
        GmTree::autonamel "chart for $m"
    }
}

proc GmChart::show_columns { id } {
	variable opt
	global bgcolor
	set mapname $opt($id,map)
	set layernum $opt($id,layer)
	set cmd "v.info -c map=$mapname layer=$layernum"
	run_panel $cmd
}

proc GmChart::show_data { id } {
	variable opt
	global bgcolor
	set mapname $opt($id,map)
	set layer $opt($id,layer)
	set vdb [open "|v.db.connect map=$mapname layer=$layer -g" r]
	set vectdb [read $vdb]
	close $vdb
	set vdblist [split $vectdb " "]
	set tbl [lindex $vdblist 1]
	set db [lindex $vdblist 3]
	set drv [lindex $vdblist 4]
	puts "table=$tbl database=$db driver=$drv"
	set cmd "db.select table=$tbl database=$db driver=$drv"
	run_panel $cmd
}

# chart options
proc GmChart::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Display pie and bar charts of attribute values at vector object locations" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # vector name
    set row [ frame $frm.map ]
    Label $row.a -text [G_msg "Vector map to chart:"]
    Button $row.b -image [image create photo -file "$gmpath/vector.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "vector map to chart"] \
		-command "GmChart::select_map $id"
    Entry $row.c -width 30 -text " $opt($id,map)" \
          -textvariable GmChart::opt($id,map) \
          -background white
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.vect.chart" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # vector type
    set row [ frame $frm.type ]
    Label $row.a -text [G_msg "Vector type:"]
    checkbutton $row.b -text [G_msg "points"] -variable GmChart::opt($id,type_point)
    checkbutton $row.c -text [G_msg "lines"] -variable GmChart::opt($id,type_line)
    checkbutton $row.d -text [G_msg "boundaries"] -variable GmChart::opt($id,type_boundary)
    checkbutton $row.e -text [G_msg "centroids"] -variable GmChart::opt($id,type_centroid)
    checkbutton $row.f -text [G_msg "areas"] -variable GmChart::opt($id,type_area)
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # attributes1 and data
    set row [ frame $frm.attr1 ]
    Label $row.a -text "Attributes to chart: attribute layer"
    LabelEntry $row.b -textvariable GmChart::opt($id,layer) -width 5 \
		-entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # show data and columns
    set row [ frame $frm.showcolumns ]
    Label $row.a -text [G_msg "     show attribute columns"] 
    Button $row.b -text [G_msg "columns"] \
		-image [image create photo -file "$gmpath/columns.gif"] \
		-command "GmChart::show_columns $id" \
		-background $bgcolor \
		-helptext [G_msg "Show columns"]
    Label $row.c -text [G_msg "   show attribute data"] 
    Button $row.d -text [G_msg "data"] \
		-image [image create photo -file "$gmpath/columns.gif"] \
		-command "GmChart::show_data $id" \
		-background $bgcolor \
		-helptext [G_msg "Show data"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # attributes2
    set row [ frame $frm.attr2 ]
    Label $row.a -text "     columns to chart (col1,col2,...)  "
    LabelEntry $row.b -textvariable GmChart::opt($id,columns) -width 30 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # attributes3
    set row [ frame $frm.attr3 ]
    Label $row.a -text "     colors for columns (clr1,clr2,...)"
    LabelEntry $row.b -textvariable GmChart::opt($id,fcolors) -width 30 \
            -entrybg white -padx 2
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # attributes4
    set row [ frame $frm.attr4 ]
    Label $row.a -text "     column for variable chart size"
    LabelEntry $row.b -textvariable GmChart::opt($id,sizecol) -width 12 \
            -entrybg white -padx 9
    Label $row.c -text "   scale factor"
    LabelEntry $row.d -textvariable GmChart::opt($id,cscale) -width 4 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # chart options1
    set row [ frame $frm.chopt1 ]
    Label $row.a -text [G_msg "Chart type:"] 
    ComboBox $row.b -padx 2 -width 4 -textvariable GmChart::opt($id,ctype) \
                    -values {"pie" "bar"} -entrybg white
    Label $row.c -text "       fixed chart size (if size column not used)"
    LabelEntry $row.d -textvariable GmChart::opt($id,csize) -width 4 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # chart options2
    set row [ frame $frm.chopt2 ]
    Label $row.a -text [G_msg "     chart outline color:"] 
    ComboBox $row.b -padx 0 -width 10 -textvariable GmChart::opt($id,ocolor) \
                    -values {"none" "white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
                    -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmChart::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check map layer ctype columns sizecol csize cscale ocolor fcolors \
             type_point type_line type_boundary type_centroid type_area } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}


proc GmChart::display { node } {
    variable opt
    variable tree
    global mon
    set line ""
    set input ""
    global gmpath
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]


    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 
    if { $opt($id,columns) == "" } { return }
    if { !$opt($id,type_point) && !$opt($id,type_line) &&
         !$opt($id,type_boundary)  && !$opt($id,type_centroid) && 
         !$opt($id,type_area) } { return } 

    # combine vector types         
    set type ""
    if { $opt($id,type_point) } { append type "point" }
    if { $opt($id,type_line) && "$type" != "" } { 
        append type ",line"
    } elseif { $opt($id,type_line) && "$type" == "" } {
        append type "line"}
    if { $opt($id,type_boundary) && "$type" != "" } { 
        append type ",boundary" 
    } elseif { $opt($id,type_boundary) && "$type" == "" } {
        append type "boundary"}
    if { $opt($id,type_centroid) && "$type" != "" } { 
        append type ",centroid" 
    } elseif { $opt($id,type_centroid) && "$type" == "" } {
        append type "centroid"}
    if { $opt($id,type_area) && "$type" != "" } { 
        append type ",area" 
    } elseif { $opt($id,type_area) && "$type" == "" } {
        append type "area"}

    #create d.vect.chart command
    set cmd "d.vect.chart map=$opt($id,map) type=$type \
            layer=$opt($id,layer) columns=$opt($id,columns) \
            ctype=$opt($id,ctype) ocolor=$opt($id,ocolor) "
            
    # sizecol
    if { $opt($id,sizecol) != "" } { 
        append cmd " sizecol=$opt($id,sizecol)"
    }

    # csize
    if { $opt($id,csize) != "" } { 
        append cmd " size=$opt($id,csize)"
    }

    # cscale
    if { $opt($id,cscale) != "" } { 
        append cmd " scale=$opt($id,cscale)"
    }

    # fcolors
    if { $opt($id,fcolors) != "" } { 
        append cmd " colors=$opt($id,fcolors)"
    }

        run_panel $cmd

}


proc GmChart::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "chart:$count"

    set frm [ frame .charticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmChart::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo chartico -file "$gmpath/legend.gif"
    set ico [label $frm.ico -image chartico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,map) == ""} {
    	$tree insert end $parent $node \
		-text      "chart $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "chart for $opt($id,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)

    set opt($count,map) "$opt($id,map)" 
    set opt($count,type_point) "$opt($id,type_point)" 
    set opt($count,type_line)  "$opt($id,type_line)"
    set opt($count,type_boundary)  "$opt($id,type_boundary)"
    set opt($count,type_centroid)  "$opt($id,type_centroid)"
    set opt($count,type_area)  "$opt($id,type_area)"
    set opt($count,layer)  "$opt($id,layer)"
    set opt($count,ctype)  "$opt($id,ctype)" 
    set opt($count,columns)  "$opt($id,columns)" 
    set opt($count,sizecol)  "$opt($id,sizecol)" 
    set opt($count,csize)  "$opt($id,csize)" 
    set opt($count,cscale)  "$opt($id,cscale)"
    set opt($count,color) "$opt($id,oolor)" 
    set opt($count,color) "$opt($id,folors)" 

    incr count
    return $node
}
