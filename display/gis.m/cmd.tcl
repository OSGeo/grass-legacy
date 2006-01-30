###############################################################
# cmd.tcl - command layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmCmd {
    variable array opt # cmd options
    variable count 1
}


proc GmCmd::create { tree parent } {
    variable opt
    variable count 
    global gmpath

    set node "cmd:$count"

    set frm [ frame .cmdicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmCmd::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo cico -file "$gmpath/cmd.gif"
    set ico [label $frm.ico -image cico -bd 1 -relief raised]
    
    pack $check $ico -side left

    $tree insert end $parent $node \
	-text      "cmd $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,_check) 1 

    set opt($count,cmd) "" 

    incr count
    return $node
}

proc GmCmd::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

# display cmd options
proc GmCmd::options { id frm } {
    variable opt

    # cmd name
    set row [ frame $frm.name ]
    Label $row.a -text [G_msg "Command:"] 
    Entry $row.b -width 40 -text "$opt($id,cmd)" \
          -textvariable GmCmd::opt($id,cmd)
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmCmd::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check cmd } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}

proc GmCmd::display { node } {
    variable opt
    variable tree
    global mon
    
    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,cmd) == "" } { return } 

    set cmd $opt($id,cmd)

    runcmd $cmd
}

proc GmCmd::query { node } {
    puts "Query not supported for Command type layer"
}

proc GmCmd::duplicate { tree parent node id} {
    variable opt
    variable count 
    global gmpath

    set node "cmd:$count"

    set frm [ frame .cmdicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmCmd::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo cico -file "$gmpath/cmd.gif"
    set ico [label $frm.ico -image cico -bd 1 -relief raised]
    
    pack $check $ico -side left

    $tree insert end $parent $node \
	-text      "cmd $count" \
	-window    $frm \
	-drawcross auto 

    set opt($count,_check) 1 

    set opt($count,cmd) "$opt($id,cmd)" 

    incr count
    return $node
}