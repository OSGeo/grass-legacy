
namespace eval DmRaster {
    variable array opt # raster options
    variable count 1
}


proc DmRaster::create { tree parent } {
    variable opt
    variable count
    global dmpath

    set node "raster:$count"

    set frm [ frame .rastericon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable DmRaster::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rico -file "$dmpath/raster.gif"
    set ico [label $frm.ico -image rico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "raster $count"\
	-window    $frm \
	-drawcross auto  
    
    set opt($count,_check) 1 
    set opt($count,map) "" 
    set opt($count,cquery) "" 
    set opt($count,vquery) "" 
    set opt($count,bkcolor) "" 
    set opt($count,overlay) 0 
    set opt($count,legend) 0 
    set opt($count,legmon) "x1" 
    set opt($count,legthin) "1"
    
    incr count
    return $node
}

proc DmRaster::set_option { node key value } {
    variable opt
 
    set id [Dm::node_id $node]
    set opt($id,$key) $value
}

proc DmRaster::select_map { id } {
    variable tree
    variable node
    set m [GSelect cell]
    if { $m != "" } { 
        set DmRaster::opt($id,map) $m
        Dm::autoname $m
    }
}

# display raster options
proc DmRaster::options { id frm } {
    variable opt

    # raster name
    set row [ frame $frm.name ]
    Button $row.a -text [G_msg "Raster name:"] \
           -command "DmRaster::select_map $id"
    Entry $row.b -width 40 -text "$opt($id,map)" \
          -textvariable DmRaster::opt($id,map)
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # cat query
    set row [ frame $frm.cat ]
    LabelEntry $row.a -label [G_msg " List of cat# to display (integer maps only) "] \
    -textvariable DmRaster::opt($id,cquery) -width 30
    pack $row.a -side left
    pack $row -side top -fill both -expand yes


    # value query
    set row [ frame $frm.val ]
    LabelEntry $row.a -label [G_msg " List of values to display (fp maps only)     "] \
    -textvariable DmRaster::opt($id,vquery) -width 30
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
    
    
    # background color
    set row [ frame $frm.bg ]
    ComboBox $row.a -label [G_msg " Background color for null values                 "] \
                    -width 10 -textvariable DmRaster::opt($id,bkcolor) \
                    -values {"white" "grey" "gray" "black" "brown" "red" "orange" \
                    "yellow" "green" "aqua" "cyan" "indigo" "blue" "purple" "violet" "magenta"}
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
    
    # legend
    set row [ frame $frm.leg ]
    checkbutton $row.a -text [G_msg "show legend in selected display monitor"] -variable DmRaster::opt($id,legend) 
    ComboBox $row.b -width 4 -textvariable DmRaster::opt($id,legmon) \
                    -values {"x0" "x1" "x2" "x3" "x4" "x5" "x6"}
    LabelEntry $row.c -label [G_msg " thin legend by "] -textvariable DmRaster::opt($id,legthin) -width 6
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # overlay
    set row [ frame $frm.over ]
    checkbutton $row.a -text [G_msg "overlay (null values are transparent)"] -variable DmRaster::opt($id,overlay) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes
}

proc DmRaster::save { tree depth node } {
    variable opt
    
    set id [Dm::node_id $node]


    foreach key { _check map overlay legend } {
        Dm::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc DmRaster::display { node } {
    variable opt
    set currmon ""
    set line ""
    set input ""
    global dmpath

    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    set cmd "d.rast map=$opt($id,map)"

    # overlay
    if { $opt($id,overlay) } { 
        append cmd " -o"
    }
    
    # cat query
    if { $opt($id,cquery) != "" } { 
        append cmd " {catlist=$opt($id,cquery)}"
    }
    
    
    # value query
    if { $opt($id,vquery) != "" } { 
        append cmd " vallist=$opt($id,vquery)"
    }
    
    # background color
    if { $opt($id,bkcolor) != "" } { 
        append cmd " bg=$opt($id,bkcolor)"
    }
    run $cmd

    #display with legend
    if { $opt($id,legend) } { 
        if ![catch {open "|d.mon -L" r} input] {
            while {[gets $input line] >= 0} {
                 if {[regexp -nocase {.*(selected).*} $line]} {
                    regexp -nocase {..} $line currmon
                }              
            }
        }

        DmMonitorsel::displmon $opt($id,legmon)
        run "d.erase white"
        run "d.legend map=$opt($id,map) thin=$opt($id,legthin)"
        run "d.mon select=$currmon"
    }
}

proc DmRaster::print { file node } {
    variable opt
    
    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    puts $file "raster $opt($id,map)"
}

proc DmRaster::query { node } {
    variable opt
    
    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    set cmd "d.what.rast map=$opt($id,map)"
    
    term $cmd

}

proc DmRaster::duplicate { tree parent node id } {
    variable opt
    variable count 
    global dmpath

    set node "raster:$count"

    set frm [ frame .rastericon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable DmRaster::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo rico -file "$dmpath/raster.gif"
    set ico [label $frm.ico -image rico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,map) == ""} {
    	$tree insert end $parent $node \
		-text      "raster $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "$opt($id,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)

    set opt($count,map) "$opt($id,map)" 
    set opt($count,overlay) $opt($id,overlay)
    set opt($count,legend)  $opt($id,legend)

    incr count
    return $node
}