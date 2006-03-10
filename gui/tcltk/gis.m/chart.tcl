##########################################################################
# chart.tcl - vector chart layer options file for GRASS GIS Manager
# March 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmChart {
    variable array opt # chart options
    variable count 1
    variable array lfile # vector
    variable array lfilemask # vector
    variable optlist
}


proc GmChart::create { tree parent } {
    variable opt
    variable count
    variable optlist
    variable lfile
    variable lfilemask
    global gmpath

    set node "chart:$count"

    set frm [ frame .charticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmChart::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo chartico -file "$gmpath/chart.gif"
    set ico [label $frm.ico -image chartico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
	#insert new layer
	if {[$tree selection get] != "" } {
		set sellayer [$tree index [$tree selection get]]
    } else { 
    	set sellayer "end" 
    }

    $tree insert $sellayer $parent $node \
	-text  "chart $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,1,_check) 1 
    
    set opt($count,1,map) "" 
	set opt($count,1,opacity) 1.0
    set opt($count,1,type_point) 1 
    set opt($count,1,type_line) 1
    set opt($count,1,type_boundary) 1
    set opt($count,1,type_centroid) 1
    set opt($count,1,type_area) 0
    set opt($count,1,layer) 1 
    set opt($count,1,ctype) "pie" 
    set opt($count,1,columns) "" 
    set opt($count,1,sizecol) "" 
    set opt($count,1,csize) 40 
    set opt($count,1,cscale) 1 
    set opt($count,1,ocolor) "black" 
    set opt($count,1,fcolors) "" 
    set opt($count,1,mod) 1
    
	set optlist { _check map layer ctype columns sizecol csize cscale ocolor fcolors \
             type_point type_line type_boundary type_centroid type_area opacity} 

    foreach key $optlist {
		set opt($count,0,$key) $opt($count,1,$key)
    } 
    
	# create files in tmp diretory for layer output
	set mappid [pid]
	set lfile($count) [eval exec "g.tempfile pid=$mappid"]
	set lfilemask($count) $lfile($count)
	append lfile($count) ".ppm"
	append lfilemask($count) ".pgm"

	incr count
    return $node
}

proc GmChart::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,1,$key) $value
}

proc GmChart::select_map { id } {
    variable tree
    variable node
    set m [GSelect vector]
    if { $m != "" } { 
        set GmChart::opt($id,1,map) $m
        GmTree::autonamel "chart for $m"
    }
}

proc GmChart::show_columns { id } {
	variable opt
	global bgcolor
	set mapname $opt($id,1,map)
	set layernum $opt($id,1,layer)
	set cmd "v.info -c map=$mapname layer=$layernum"
	run_panel $cmd
}

proc GmChart::show_data { id } {
	variable opt
	global bgcolor
	set mapname $opt($id,1,map)
	set layer $opt($id,1,layer)
	if ![catch {open "|v.db.connect map=$mapname layer=$layer -g" r} vdb] {
		set vectdb [read $vdb]
		catch {close $vdb}
		set vdblist [split $vectdb " "]
		set tbl [lindex $vdblist 1]
		set db [lindex $vdblist 3]
		set drv [lindex $vdblist 4]
		set cmd "db.select table=$tbl database=$db driver=$drv"
		run_panel $cmd
	}
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

	#opacity
	set row [ frame $frm.opc]
	Label $row.a -text [G_msg "Opaque "]
	scale $row.b -from 1.0 -to 0.0 -showvalue 1  \
		-orient horizontal -length 300 -resolution 0.01 -fg "#656565"\
		-variable GmChart::opt($id,1,opacity) 
	Label $row.c -text [G_msg " Transparent"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes	
	
    # vector name
    set row [ frame $frm.map ]
    Label $row.a -text [G_msg "Vector map to chart:"]
    Button $row.b -image [image create photo -file "$gmpath/vector.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "vector map to chart"] \
		-command "GmChart::select_map $id"
    Entry $row.c -width 30 -text " $opt($id,1,map)" \
          -textvariable GmChart::opt($id,1,map) \
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
    checkbutton $row.b -text [G_msg "points"] -variable GmChart::opt($id,1,type_point)
    checkbutton $row.c -text [G_msg "lines"] -variable GmChart::opt($id,1,type_line)
    checkbutton $row.d -text [G_msg "boundaries"] -variable GmChart::opt($id,1,type_boundary)
    checkbutton $row.e -text [G_msg "centroids"] -variable GmChart::opt($id,1,type_centroid)
    checkbutton $row.f -text [G_msg "areas"] -variable GmChart::opt($id,1,type_area)
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left
    pack $row -side top -fill both -expand yes

    # attributes1 and data
    set row [ frame $frm.attr1 ]
    Label $row.a -text "Attributes to chart: attribute layer"
    LabelEntry $row.b -textvariable GmChart::opt($id,1,layer) -width 5 \
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
    LabelEntry $row.b -textvariable GmChart::opt($id,1,columns) -width 30 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # attributes3
    set row [ frame $frm.attr3 ]
    Label $row.a -text "     colors for columns (clr1,clr2,...)"
    LabelEntry $row.b -textvariable GmChart::opt($id,1,fcolors) -width 30 \
            -entrybg white -padx 2
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # attributes4
    set row [ frame $frm.attr4 ]
    Label $row.a -text "     column for variable chart size"
    LabelEntry $row.b -textvariable GmChart::opt($id,1,sizecol) -width 12 \
            -entrybg white -padx 9
    Label $row.c -text "   scale factor"
    LabelEntry $row.d -textvariable GmChart::opt($id,1,cscale) -width 4 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # chart options1
    set row [ frame $frm.chopt1 ]
    Label $row.a -text [G_msg "Chart type:"] 
    ComboBox $row.b -padx 2 -width 4 -textvariable GmChart::opt($id,1,ctype) \
                    -values {"pie" "bar"} -entrybg white
    Label $row.c -text "       fixed chart size (if size column not used)"
    LabelEntry $row.d -textvariable GmChart::opt($id,1,csize) -width 4 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # chart options2
    set row [ frame $frm.chopt2 ]
    Label $row.a -text [G_msg "     chart outline color:"] 
    ComboBox $row.b -padx 0 -width 10 -textvariable GmChart::opt($id,1,ocolor) \
                    -values {"none" "white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"} \
                    -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmChart::save { tree depth node } {
    variable opt
    variable optlist
    
    set id [GmTree::node_id $node]

    foreach key $optlist {
        GmTree::rc_write $depth "$key $opt($id,1,$key)"
    } 
}


proc GmChart::display { node mod } {
    global mon
    global mapfile
    global maskfile
    global complist
    global opclist
    global masklist
    variable optlist
    variable lfile 
    variable lfilemask
    variable opt
    variable tree

    set line ""
    set input ""
    global gmpath
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    set opt($id,1,mod) $mod

    if { $opt($id,1,map) == "" } { return } 
    if { $opt($id,1,columns) == "" } { return }
    if { !$opt($id,1,type_point) && !$opt($id,1,type_line) &&
         !$opt($id,1,type_boundary)  && !$opt($id,1,type_centroid) && 
         !$opt($id,1,type_area) } { return } 

    # combine vector types         
    set type ""
    if { $opt($id,1,type_point) } { append type "point" }
    if { $opt($id,1,type_line) && "$type" != "" } { 
        append type ",line"
    } elseif { $opt($id,1,type_line) && "$type" == "" } {
        append type "line"}
    if { $opt($id,1,type_boundary) && "$type" != "" } { 
        append type ",boundary" 
    } elseif { $opt($id,1,type_boundary) && "$type" == "" } {
        append type "boundary"}
    if { $opt($id,1,type_centroid) && "$type" != "" } { 
        append type ",centroid" 
    } elseif { $opt($id,1,type_centroid) && "$type" == "" } {
        append type "centroid"}
    if { $opt($id,1,type_area) && "$type" != "" } { 
        append type ",area" 
    } elseif { $opt($id,1,type_area) && "$type" == "" } {
        append type "area"}

    #create d.vect.chart command
    set cmd "d.vect.chart map=$opt($id,1,map) type=$type \
            layer=$opt($id,1,layer) columns=$opt($id,1,columns) \
            ctype=$opt($id,1,ctype) ocolor=$opt($id,1,ocolor) "
            
    # sizecol
    if { $opt($id,1,sizecol) != "" } { 
        append cmd " sizecol=$opt($id,1,sizecol)"
    }

    # csize
    if { $opt($id,1,csize) != "" } { 
        append cmd " size=$opt($id,1,csize)"
    }

    # cscale
    if { $opt($id,1,cscale) != "" } { 
        append cmd " scale=$opt($id,1,cscale)"
    }

    # fcolors
    if { $opt($id,1,fcolors) != "" } { 
        append cmd " colors=$opt($id,1,fcolors)"
    }

    # check to see if options have changed
    foreach key $optlist {
        if {$opt($id,0,$key) != $opt($id,1,$key)} {
        	set opt($id,1,mod) 1
        	set opt($id,0,$key) $opt($id,1,$key)
        }
    } 

	# redraw if options changed
	if {$opt($id,1,mod) == 1} {
	    run_panel $cmd
    	file copy -force $mapfile($mon) $lfile($id)
    	file copy -force $maskfile($mon) $lfilemask($id)
	}
    
    if { ! ( $opt($id,1,_check) ) } { return } 

    #add lfile to compositing list
	if {$complist($mon) != "" } {
	    append complist($mon) ","
	    append complist($mon) [file tail $lfile($id)]
	} else {
	    append complist($mon) [file tail $lfile($id)]
	}	

	if {$masklist($mon) != "" } {
	    append masklist($mon) ","
	    append masklist($mon) [file tail $lfilemask($id)]
	} else {
	    append masklist($mon) [file tail $lfilemask($id)]
	}	

	if {$opclist($mon) != "" } {
	    append opclist($mon) ","
	    append opclist($mon) $opt($id,1,opacity)
	} else {
	    append opclist($mon) $opt($id,1,opacity)
	}	
	
	# reset options changed flag
	set opt($id,1,mod) 0

}


proc GmChart::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "chart:$count"

    set frm [ frame .charticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmChart::opt($count,1,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo chartico -file "$gmpath/legend.gif"
    set ico [label $frm.ico -image chartico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,1,map) == ""} {
    	$tree insert end $parent $node \
		-text      "chart $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "chart for $opt($id,1,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,1,_check) $opt($id,1,_check)

    set opt($count,1,map) "$opt($id,1,map)" 
	set opt($count,1,opacity) opt($id,1,opacity)
    set opt($count,1,type_point) "$opt($id,1,type_point)" 
    set opt($count,1,type_line)  "$opt($id,1,type_line)"
    set opt($count,1,type_boundary)  "$opt($id,1,type_boundary)"
    set opt($count,1,type_centroid)  "$opt($id,1,type_centroid)"
    set opt($count,1,type_area)  "$opt($id,1,type_area)"
    set opt($count,1,layer)  "$opt($id,1,layer)"
    set opt($count,1,ctype)  "$opt($id,1,ctype)" 
    set opt($count,1,columns)  "$opt($id,1,columns)" 
    set opt($count,1,sizecol)  "$opt($id,1,sizecol)" 
    set opt($count,1,csize)  "$opt($id,1,csize)" 
    set opt($count,1,cscale)  "$opt($id,1,cscale)"
    set opt($count,1,color) "$opt($id,1,oolor)" 
    set opt($count,1,color) "$opt($id,1,folors)" 

    incr count
    return $node
}
