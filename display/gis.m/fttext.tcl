###############################################################
# fftext.tcl - freetype text layer options file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmFTtext {
    variable array opt # fttext options
    variable count 1
}


proc GmFTtext::create { tree parent } {
    variable opt
    variable count
    global gmpath

    set node "fttext:$count"

    set frm [ frame .fttexticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmFTtext::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo fttico -file "$gmpath/fttext.gif"
    set ico [label $frm.ico -image fttico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "freetype text $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,_check) 1 

    set opt($count,text) "" 
    set opt($count,east_north) "" 
    set opt($count,font) "luximr" 
    set opt($count,charset) "UTF-8" 
    set opt($count,path) "" 
    set opt($count,color) \#000000 
    set opt($count,size) 5 
    set opt($count,align) "ll" 
    set opt($count,rotation) 0 
    set opt($count,linespacing) 1.1 
    set opt($count,bold) 0 
    set opt($count,textcoord) "geographic" 
    set opt($count,radians) 0 
    set opt($count,htpixel) 0 
    
    incr count
    return $node
}

proc GmFTtext::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmFTtext::select_file { id } {
    variable tree
    variable node
    set m [GSelect file ]
    if { $m != "" } { 
        set GmFTtext::opt($id,path) $m
    }
}


# fttext options
proc GmFTtext::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # text
    set row [ frame $frm.text ]
    Label $row.a -text "Text to display:"
    LabelEntry $row.b -textvariable GmFTtext::opt($id,text) -width 51 \
            -entrybg white
    Button $row.c -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.text.freetype" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
    
    # coordinates1
    set row [ frame $frm.east_north ]
    Label $row.a -text "Text placement: coordinates east,north or x,y"
    LabelEntry $row.b -textvariable GmFTtext::opt($id,east_north) -width 25 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
        
    # coordinates2
    set row [ frame $frm.textcoord2 ]
    Label $row.a -text \
        [G_msg "     (leave blank to place with mouse; position will not save)"] 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # coordinates3
    set row [ frame $frm.textcoord3 ]
    Label $row.a -text [G_msg "     coordinate type"] 
    ComboBox $row.b -padx 2 -width 10 -textvariable GmFTtext::opt($id,textcoord) \
                    -values {"geographic" "percent" "pixels" } -entrybg white
    Label $row.c -text [G_msg "  align text with coordinate point"] 
    ComboBox $row.d -padx 2 -width 2 -textvariable GmFTtext::opt($id,align) \
                    -values {"ll" "lc" "lr" "cl" "cc" "cr" "ul" "uc" "ur" } -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
        
    # coordinates3
    set row [ frame $frm.textcoord4 ]
    Label $row.a -text \
        [G_msg "     (for coordinates, % is from bottom left of display, pixels from top left)"] 
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # text options1
    set row [ frame $frm.textopt1 ]
    Label $row.a -text [G_msg "     text rotation (degrees counterclockwise)"] 
    SpinBox $row.b -range {1 360 1} -textvariable GmFTtext::opt($id,rotation) \
                   -entrybg white -width 4
    checkbutton $row.c -padx 10 -text [G_msg "rotation in radians"] -variable \
        GmFTtext::opt($id,radians)
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
    
    # text options2
    set row [ frame $frm.textopt2 ]
    Label $row.a -text "     line spacing"
    LabelEntry $row.b -textvariable GmFTtext::opt($id,linespacing) -width 5 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
    # standard font
    set row [ frame $frm.font ]
    Label $row.a -text [G_msg "Font:"] 
    ComboBox $row.b -padx 2 -width 7 -textvariable GmFTtext::opt($id,font) \
                    -values {"luximr" "luxirr" "luxisr" "luximb" "luxirb" \
                    "luxisb" "luximri" "luxirri" "luxisri" "luximbi" \
                    "luxirbi" "luxisbi"} -entrybg white
    Label $row.c -text [G_msg "  color"] 
    SelectColor $row.d -type menubutton -variable GmFTtext::opt($id,color)
    checkbutton $row.e -padx 10 -text [G_msg "bold text"] -variable \
        GmFTtext::opt($id,bold) 
    Label $row.f -text "character encoding"
    LabelEntry $row.g -textvariable GmFTtext::opt($id,charset) -width 10 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d $row.e $row.f $row.g -side left
    pack $row -side top -fill both -expand yes

    # standard font
    set row [ frame $frm.height ]
    Label $row.a -text "     text height (% of display)" 
    SpinBox $row.b -range {1 100 1} -textvariable GmFTtext::opt($id,size) \
                   -entrybg white -width 3 
    checkbutton $row.c -padx 10 -text [G_msg "height in pixels instead of %"] -variable \
        GmFTtext::opt($id,htpixel) 
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes

    # font path
    set row [ frame $frm.path ]
    Label $row.a -text "     path to custom font" 
    Entry $row.b -width 45 -text " $opt($id,path)" \
          -textvariable GmFTtext::opt($id,path) \
          -background white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

}

proc GmFTtext::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check text east_north font path charset color \
            size align rotation linespacing bold textcoord radians htpixel } {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}




proc GmFTtext::display { node } {
    variable opt
    variable tree
    global mon
    set line ""
    set input ""
    global gmpath
    set cmd ""

    set tree($mon) $GmTree::tree($mon)
    set id [GmTree::node_id $node]
    
    # set hex colors to rgb         
    set color [Gm::color $opt($id,color)]
    

    if { ! ( $opt($id,_check) ) } { return } 

    if { $opt($id,text) == "" } { return } 
    
    
    set cmd "d.text.freetype charset=$opt($id,charset) \
            color=$color size=$opt($id,size) align=$opt($id,align) \
            rotation=$opt($id,rotation) linespacing=$opt($id,linespacing)\
            {text=$opt($id,text)}"

    # coordinates
    if { $opt($id,east_north) != "" } { 
#    	set $opt($id,east_north) \"$opt($id,east_north)\"
        append cmd " east_north=$opt($id,east_north)"
#        append cmd " {east_north=$opt($id,east_north)}"
    }

    # font
    if { $opt($id,font) != "" } { 
        append cmd " font=$opt($id,font)"
    }

    # path
    if { $opt($id,path) != "" } { 
        append cmd " {path=$opt($id,path)}"
    }

    # textcoord percent
    if { $opt($id,textcoord) == "percent" } { 
        append cmd " -n"
    }

    # textcoord pixel
    if { $opt($id,textcoord) == "pixel" } { 
        append cmd " -p"
    }

    # font height in pixels
    if { $opt($id,htpixel) != 0 } { 
        append cmd " -s"
    }

    # bold text
    if { $opt($id,bold) != 0 } { 
        append cmd " -b"
    }

    # radians
    if { $opt($id,radians) != 0} { 
        append cmd " r"
    }

    run_panel $cmd
}


proc GmFTtext::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "fttext:$count"

    set frm [ frame .fttexticon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmFTtext::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo fttico -file "$gmpath/fttext.gif"
    set ico [label $frm.ico -image fttico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,text) == ""} {
    	$tree insert end $parent $node \
		-text      "freetype text $count" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)

    set opt($count,text) $opt($id,text) 
    set opt($count,east_north) $opt($id,east_north)
    set opt($count,font) $opt($id,font) 
    set opt($count,path) $opt($id,path)
    set opt($count,charset) $opt($id,charset) 
    set opt($count,color) $opt($id,color)
    set opt($count,size) $opt($id,size) 
    set opt($count,align) $opt($id,align) 
    set opt($count,rotation) $opt($id,rotation) 
    set opt($count,linespacing) $opt($id,linespacing) 
    set opt($count,bold) $opt($id,bold)
    set opt($count,textcoord) $opt($id,textcoord)
    set opt($count,radians) $opt($id,radians) 
    set opt($count,htpixel) $opt($id,htpixel)

    incr count
    return $node
}
