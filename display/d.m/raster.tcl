
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
	-text      "raster $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,_check) 1 

    set opt($count,map) "" 
    set opt($count,overlay) 0 
    set opt($count,legend) 0 

    incr count
    return $node
}

proc DmRaster::set_option { node key value } {
    variable opt
 
    set id [Dm::node_id $node]
    set opt($id,$key) $value
}

proc DmRaster::select_map { id } {
    set m [GSelect cell]
    if { $m != "" } { 
        set DmRaster::opt($id,map) $m 
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

    # overlay
    set row [ frame $frm.over ]
    checkbutton $row.a -text [G_msg "overlay"] -variable DmRaster::opt($id,overlay) 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # display with legend
    set row [ frame $frm.legend ]
    checkbutton $row.a -text [G_msg "display with legend"] -variable DmRaster::opt($id,legend) 
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
    
    set tree $Dm::tree
    set id [Dm::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,map) == "" } { return } 

    set cmd "d.rast map=$opt($id,map)"

    # overlay
    if { $opt($id,overlay) } { 
        append cmd " -o"
    }
    
    #display with legend
    if { $opt($id,legend) } { 
        set cmd "d.rast.leg map=$opt($id,map)"
    }
    
    run $cmd
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

    $tree insert end $parent $node \
	-text      "raster $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,_check) $opt($id,_check)

    set opt($count,map) "$opt($id,map)" 
    set opt($count,overlay) $opt($id,overlay)
    set opt($count,legend)  $opt($id,legend)

    incr count
    return $node
}