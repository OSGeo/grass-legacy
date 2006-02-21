###############################################################
# thematic.tcl - thematic vector mapping layer options file for GRASS GIS Manager
# February 2006 Michael Barton, Arizona State University
###############################################################

namespace eval GmThematic {
    variable array opt # thematic options
	variable array tlegend # mon
	variable array tlegcan # mon

    variable count 1
}


proc GmThematic::create { tree parent } {
    variable opt
    variable count
    global gmpath
    global mon
    global frm

    set node "thematic:$count"

    set frm [ frame .thematicicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmThematic::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo thematicico -file "$gmpath/thematic.gif"
    set ico [label $frm.ico -image thematicico -bd 1 -relief raised]
    
    pack $check $ico -side left
    
    $tree insert end $parent $node \
	-text  "thematic $count"\
	-window    $frm \
	-drawcross auto  
        
    set opt($count,_check) 1 
    
    set opt($count,map) "" 
    set opt($count,type) "area"
    set opt($count,column) "" 
    set opt($count,themetype) "graduated_colors" 
    set opt($count,themecalc) "interval" 
    set opt($count,breakpoints) "" 
    set opt($count,where) "" 
    set opt($count,layer) 1 
    set opt($count,icon) "basic/circle" 
    set opt($count,ptsize) 5 
    set opt($count,maxsize) 20 
    set opt($count,nint) 4 
    set opt($count,colorscheme) "blue-red" 
    set opt($count,pointcolor) \#FF0000 
    set opt($count,linecolor) \#000000 
    set opt($count,startcolor) \#FF0000 
    set opt($count,endcolor) \#0000FF 
    set opt($count,update_rgb) 0 
    set opt($count,math) 0 
    set opt($count,psmap) "" 
    set opt($count,border) 1 
    set opt($count,titlefont) "{times} 14 bold" 
    set opt($count,subtitlefont) "{times} 12 bold" 
    set opt($count,labelfont) "{times} 12" 
    set opt($count,tfontcolor) \#000000  
    set opt($count,lfontcolor) \#000000  
    
    incr count
    return $node
}

proc GmThematic::set_option { node key value } {
    variable opt
 
    set id [GmTree::node_id $node]
    set opt($id,$key) $value
}

proc GmThematic::select_map { id } {
    variable tree
    variable node
    set m [GSelect vector]
    if { $m != "" } { 
        set GmThematic::opt($id,map) $m
        GmTree::autonamel "thematic map for $m"
    }
}

proc GmThematic::select_tfont { id } {
	variable opt
    global frm 
    
    set fon [SelectFont $frm.font -type dialog -sampletext 1 -title "Select font"]
	if { $fon != "" } {set opt($id,titlefont) $fon}
}

proc GmThematic::select_stfont { id } {
	variable opt
    global frm 
    
    set fon [SelectFont $frm.font -type dialog -sampletext 1 -title "Select font"]
	if { $fon != "" } {set opt($id,subtitlefont) $fon}
}
proc GmThematic::select_lfont { id } {
	variable opt
    global frm 
    
    set fon [SelectFont $frm.font -type dialog -sampletext 1 -title "Select font"]
	if { $fon != "" } {set opt($id,labelfont) $fon}
}

proc GmThematic::show_columns { id } {
	variable opt
	global bgcolor
	set mapname $opt($id,map)
	set layernum $opt($id,layer)
	set cmd "v.info -c map=$mapname layer=$layernum"
	run_panel $cmd
}

proc GmThematic::show_data { id } {
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

# select symbols from directories
proc GmThematic::select_symbol { id } {
    variable opt
    set i [GSelect symbol]
    if { $i != "" } {
        set GmThematic::opt($id,icon) $i
    }
}

# thematic options
proc GmThematic::options { id frm } {
    variable opt
    global gmpath
    global bgcolor

    # Panel heading
    set row [ frame $frm.heading1 ]
    Label $row.a -text "Display vector maps thematically by graduate colors (all types)" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    set row [ frame $frm.heading2 ]
    Label $row.a -text "  or by graduated sizes (points and lines)" \
    	-fg MediumBlue
    pack $row.a -side left
    pack $row -side top -fill both -expand yes

    # vector name
    set row [ frame $frm.map ]
    Label $row.a -text [G_msg "Vector map:"]
    Button $row.b -image [image create photo -file "$gmpath/vector.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "vector for thematic mapping"] \
		-command "GmThematic::select_map $id"
    Entry $row.c -width 35 -text " $opt($id,map)" \
          -textvariable GmThematic::opt($id,map) \
          -background white
    Label $row.d -text "   "
    Button $row.e -text [G_msg "Help"] \
            -image [image create photo -file "$gmpath/grass.gif"] \
            -command "run g.manual d.vect.thematic" \
            -background $bgcolor \
            -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes

    # vector type and layer
    set row [ frame $frm.vtype ]
    Label $row.a -text [G_msg "    vector type"] 
    ComboBox $row.b -padx 2 -width 10 -textvariable GmThematic::opt($id,type) \
                    -values {"area" "point" "centroid" "line" "boundary"} -entrybg white
    Label $row.c -text " attribute layer"
    LabelEntry $row.d -textvariable GmThematic::opt($id,layer) -width 3 \
            -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # vector column
    set row [ frame $frm.column ]
    Label $row.a -text "    NUMERIC attribute column to use for thematic map"
    LabelEntry $row.b -textvariable GmThematic::opt($id,column) -width 15 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
    
	#show columns and data
	set row [ frame $frm.columns ]
    Label $row.a -text [G_msg "    show attribute columns"] 
    Button $row.b -text [G_msg "columns"] \
            -image [image create photo -file "$gmpath/columns.gif"] \
            -command "GmThematic::show_columns $id" \
            -background $bgcolor \
            -helptext [G_msg "Show columns"]
    Label $row.c -text [G_msg "   show data"] 
    Button $row.d -text [G_msg "data"] \
            -image [image create photo -file "$gmpath/columns.gif"] \
            -command "GmThematic::show_data $id" \
            -background $bgcolor \
            -helptext [G_msg "Show data"]
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes
    
    # Thematic type
    set row [ frame $frm.ttype ]
    Label $row.a -text [G_msg "Thematic map: type"] 
    ComboBox $row.b -padx 2 -width 16 -textvariable GmThematic::opt($id,themetype) \
		-values {"graduated_colors" "graduated_points" "graduated_lines"} -entrybg white
    Label $row.c -text [G_msg " map by"] 
    ComboBox $row.d -padx 2 -width 15 -textvariable GmThematic::opt($id,themecalc) \
		-values {"interval" "std_deviation" "quartiles" \
		"custom_breaks"} -entrybg white
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # intervals
    set row [ frame $frm.int ]
    Label $row.a -text "    number of intervals to map (interval themes):" 
    SpinBox $row.b -range {1 99 1} -textvariable GmThematic::opt($id,nint) \
                   -entrybg white -width 3 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # breakpoints
    set row [ frame $frm.break ]
    Label $row.a -text "    custom breakpoints (val val ...)"
    LabelEntry $row.b -textvariable GmThematic::opt($id,breakpoints) -width 32 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # where
    set row [ frame $frm.where ]
    Label $row.a -text "    query with SQL where clause   "
    LabelEntry $row.b -textvariable GmThematic::opt($id,where) -width 32 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # point options1
    set row [ frame $frm.pts1 ]  
    Label $row.a -text "Graduated points & lines: " 
    Button $row.b -text [G_msg "icon"] \
	    -command "GmThematic::select_symbol $id"
    Entry $row.c -width 10 -text "$opt($id,icon)" \
	    -textvariable GmThematic::opt($id,icon) \
	    -background white 
    Label $row.d -text [G_msg "point color"] 
    SelectColor $row.e -type menubutton -variable GmThematic::opt($id,pointcolor)
    Label $row.f -text [G_msg "line color"] 
    SelectColor $row.g -type menubutton -variable GmThematic::opt($id,linecolor)
    pack $row.a $row.b $row.c $row.d $row.e $row.f $row.g -side left
    pack $row -side top -fill both -expand yes

    # point options2
    set row [ frame $frm.pts2 ]  
    Label $row.a -text "    size/min size (graduated pts/lines)" 
    SpinBox $row.b -range {1 50 1} -textvariable GmThematic::opt($id,ptsize) \
        -width 2 -helptext "icon size/min size (graduated pts/lines)" -entrybg white 
    Label $row.c -text "max size (graduated pts)" 
    SpinBox $row.d -range {1 50 1} -textvariable GmThematic::opt($id,maxsize) \
        -width 2 -helptext " max size (graduated pts/lines)" -entrybg white 
    pack $row.a $row.b $row.c $row.d -side left
    pack $row -side top -fill both -expand yes

    # color options1
    set row [ frame $frm.color1 ]
    Label $row.a -text [G_msg "Graduated colors: preset color schemes"] 
    ComboBox $row.b -padx 2 -width 18 -textvariable GmThematic::opt($id,colorscheme) \
        -values {"blue-red" "red-blue" "green-red" "red-green" \
        "blue-green" "green-blue" "cyan-yellow" "yellow-cyan" "custom_gradient" \
        "single_color" } -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # color options2
    set row [ frame $frm.color2 ]
    Label $row.a -text "    custom color scheme - start color"
    SelectColor $row.b -type menubutton -variable GmThematic::opt($id,startcolor)
    Label $row.c -text " end color"
    SelectColor $row.d -type menubutton -variable GmThematic::opt($id,endcolor)
    checkbutton $row.e -text [G_msg "draw border"] -variable GmThematic::opt($id,border)     
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes
    
    # color options3
    set row [ frame $frm.color3 ]
    Label $row.a -text "   "
    checkbutton $row.b -text [G_msg "save thematic colors to GRASSRGB column of vector file"] -variable \
        GmThematic::opt($id,update_rgb) 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # legend 1
    set row [ frame $frm.legend1 ]
    Label $row.a -text [G_msg "Legend: title font "] 
    Button $row.b -image [image create photo -file "$gmpath/font.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "title font for legend"] \
	    -command "GmThematic::select_tfont $id"
    Entry $row.c -width 15 -text "$opt($id,titlefont)" \
	    -textvariable GmThematic::opt($id,titlefont) \
	    -background white 
    Label $row.d -text " font color"
    SelectColor $row.e -type menubutton -variable GmThematic::opt($id,tfontcolor)
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes
    
    # legend 2
    set row [ frame $frm.legend2 ]
    Label $row.a -text [G_msg "    subtitle font    "] 
    Button $row.b -image [image create photo -file "$gmpath/font.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "subtitle font for legend"] \
	    -command "GmThematic::select_stfont $id"
    Entry $row.c -width 15 -text "$opt($id,subtitlefont)" \
	    -textvariable GmThematic::opt($id,subtitlefont) \
	    -background white 
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
    
    # legend 3
    set row [ frame $frm.legend3 ]
    Label $row.a -text [G_msg "    label font        "] 
    Button $row.b -image [image create photo -file "$gmpath/font.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
        -helptext [G_msg "label font for legend"] \
	    -command "GmThematic::select_lfont $id"
    Entry $row.c -width 15 -text "$opt($id,labelfont)" \
	    -textvariable GmThematic::opt($id,labelfont) \
	    -background white 
    Label $row.d -text " font color"
    SelectColor $row.e -type menubutton -variable GmThematic::opt($id,lfontcolor)
    pack $row.a $row.b $row.c $row.d $row.e -side left
    pack $row -side top -fill both -expand yes
    
	# legend 4
    set row [ frame $frm.legend4 ]
    Label $row.a -text "   "
    checkbutton $row.b -text [G_msg "use math notation in legend"] -variable \
        GmThematic::opt($id,math) 
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes

    # psmap
    set row [ frame $frm.psmap ]
    Label $row.a -text "Name for ps.map instruction files"
    LabelEntry $row.b -textvariable GmThematic::opt($id,psmap) -width 34 \
            -entrybg white
    pack $row.a $row.b -side left
    pack $row -side top -fill both -expand yes
}

proc GmThematic::save { tree depth node } {
    variable opt
    
    set id [GmTree::node_id $node]

    foreach key { _check map type column themetype themecalc breakpoints where \
             layer icon ptsize maxsize nint colorscheme pointcolor linecolor\
             startcolor endcolor border legmon thmlegend update_rgb math psmap\
             titlefont tfontcolor subtitlefont labelfont lfontcolor} {
        GmTree::rc_write $depth "$key $opt($id,$key)"
    } 
}


proc GmThematic::display { node } {
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
    if { $opt($id,column) == "" } { return }

    # set hex colors to rgb         
    set pointcolor [Gm::color $opt($id,pointcolor)]
    set linecolor [Gm::color $opt($id,linecolor)]
    set startcolor [Gm::color $opt($id,startcolor)]
    set endcolor [Gm::color $opt($id,endcolor)]
    
    # turn off x11 display
    set monitor "none"

    #create d.vect.thematic command
    set cmd "d.vect.thematic -s map=$opt($id,map) type=$opt($id,type) column=$opt($id,column) \
			layer=$opt($id,layer) icon=$opt($id,icon) size=$opt($id,ptsize) \
            maxsize=$opt($id,maxsize) nint=$opt($id,nint) pointcolor=$pointcolor \
			linecolor=$linecolor startcolor=$startcolor endcolor=$endcolor \
			themetype=$opt($id,themetype) monitor=$monitor \
			themecalc=$opt($id,themecalc) colorscheme=$opt($id,colorscheme)"
             
    # breakpoints
    if { $opt($id,breakpoints) != "" } { 
        append cmd " {breakpoints=$opt($id,breakpoints)}"
    }

    # where query
    if { $opt($id,where) != "" } { 
        append cmd " {where=$opt($id,where)}"
    }

    # psmap file 
    if { $opt($id,psmap) != "" } { 
        append cmd " psmap=$opt($id,psmap)"
    }

    # hide border
    if { $opt($id,border) == 0 } { 
        append cmd "  -f"
    }

    # update_rgb
    if { $opt($id,update_rgb) == 1 } { 
        append cmd " -u"
    }

    # math notation
    if { $opt($id,math) == 1 } { 
        append cmd " -m"
    }
    

	run_panel $cmd
	GmThematic::tlegend $mon
	GmThematic::tleg_item $mon $id
}


proc GmThematic::duplicate { tree parent node id } {
    variable opt
    variable count 
    global gmpath

    set node "thematic:$count"

    set frm [ frame .thematicicon$count]
    set fon [font create -size 10] 
    set check [checkbutton $frm.check -font $fon \
                           -variable GmThematic::opt($count,_check) \
                           -height 1 -padx 0 -width 0]

    image create photo thematicico -file "$gmpath/thematic.gif"
    set ico [label $frm.ico -image thematicico -bd 1 -relief raised]
    
    pack $check $ico -side left

	if { $opt($id,map) == ""} {
    	$tree insert end $parent $node \
		-text      "thematic $count" \
		-window    $frm \
		-drawcross auto
	} else {
	    $tree insert end $parent $node \
		-text      "thematic map for $opt($id,map)" \
		-window    $frm \
		-drawcross auto
	}

    set opt($count,_check) $opt($id,_check)

    set opt($count,map) "$opt($id,map)" 
    set opt($count,type) "$opt($id,type)"
    set opt($count,column) "$opt($id,column)"
    set opt($count,themetype) "$opt($id,themetype)" 
    set opt($count,themecalc) "$opt($id,themecalc)" 
    set opt($count,breakpoints) "$opt($id,breakpoints)" 
    set opt($count,where) "$opt($id,where)" 
    set opt($count,layer) "$opt($id,layer)"
    set opt($count,icon) "$opt($id,icon)" 
    set opt($count,ptsize) "$opt($id,ptsize)"
    set opt($count,maxsize) "$opt($id,maxsize)"
    set opt($count,nint) "$opt($id,nint)"
    set opt($count,colorscheme) "$opt($id,colorscheme)"
    set opt($count,pointcolor) "$opt($id,pointcolor)"
    set opt($count,linecolor) "$opt($id,linecolor)"
    set opt($count,startcolor) "$opt($id,startcolor)"
    set opt($count,endcolor) "$opt($id,endcolor)"
    set opt($count,border) "$opt($id,border)"
    set opt($count,legmon) "$opt($id,legmon)"
    set opt($count,thmlegend) "$opt($id,thmlegend)"
    set opt($count,update_rgb) "$opt($id,update_rgb)" 
    set opt($count,math) "$opt($id,math)" 
    set opt($count,psmap) "$opt($id,psmap)" 
    set opt($count,titlefont) "$opt($id,titlefont)"
    set opt($count,subtitlefont) "$opt($id,subtitlefont)"
    set opt($count,labelfont) "$opt($id,labelfont)"
    set opt($count,tfontcolor) "$opt($id,tfontcolor)" 
    set opt($count,lfontcolor) "$opt($id,lfontcolor)" 

    incr count
    return $node
}

###############################################################################
# create graphic legend in separate display canvas
proc GmThematic::tlegend { mon } {
	global legendtitle
	global bgcolor
	global dtxt
	global keycontrol
	global gmpath
    global env

	variable tlegend
	variable tlegcan

	if { [winfo exists .tlegend($mon)] } {return}

	set legendtitle "Legend for Map $mon"
	toplevel .tlegend($mon)
    wm title .tlegend($mon) [G_msg $legendtitle]


    wm withdraw .tlegend($mon)
    #wm overrideredirect $txt 1

	# create canvas for legend
	set tlegmf [MainFrame .tlegend($mon).mf -bg $bgcolor ]
	set tlegcan($mon) [canvas $tlegmf.can -background #ffffff \
		-borderwidth 0 -closeenough 1.0 \
        -relief ridge -selectbackground #c4c4c4 \
        -width 300 -height 300 ]
	   
    # setting geometry
    place $tlegcan($mon) \
        -in $tlegmf -x 0 -y 0 -anchor nw \
        -bordermode ignore 

	# control buttons
	set tleg_tb [$tlegmf addtoolbar]
	set tlbb [ButtonBox $tleg_tb.bb -orient horizontal -background $bgcolor]
	$tlbb add -text "clear" -command "GmThematic::tleg_erase $mon" -bg #dddddd \
		-highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Clear legend"] -highlightbackground $bgcolor
	$tlbb add -text "save" -command "GmThematic::tleg_save $mon"  -bg #dddddd \
		-highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1 \
        -helptext [G_msg "Save legend to EPS file"] -highlightbackground $bgcolor

	pack $tlegmf -expand yes -fill both -padx 0 -pady 0
	pack $tlegcan($mon) -fill both -expand yes
	pack $tlbb -side left -anchor w
			
	BWidget::place .tlegend($mon) 0 0 at 500 100
    wm deiconify .tlegend($mon)

}

# read legend file and create legend items
proc GmThematic::tleg_item { mon id } {
	variable tlegend
	variable tlegcan
	variable opt
	
	GmThematic::tleg_erase $mon 
	set ltxt [open "gismlegend.txt" r]
	set x1 30
	set y1 40
	set txtx 60
	set font $opt($id,labelfont)
	regexp {.*\s(\d*)} $font string lineht
	set yinc [expr $lineht * 2]	
	
	set titlefont $opt($id,titlefont)
	set tfontcolor $opt($id,tfontcolor)
	set subtitlefont $opt($id,subtitlefont)

	set labelfont $opt($id,labelfont)
	set lfontcolor $opt($id,lfontcolor)
	while {![eof $ltxt]} {
		gets $ltxt line
		set type [lindex $line 0]
		set fcolor [lindex $line 1]
		set lcolor [lindex $line 2]
		set size [lindex $line 3]
		set label [lindex $line 4]
		if { $fcolor != "-" } { set xfcolor [GmThematic::rgb2hex $fcolor] }
		if { $lcolor != "-" } { set xlcolor [GmThematic::rgb2hex $lcolor] }
		switch $type {
			title {
				regexp {.*\s(\d*)\s.*} $titlefont string lineht
				set yinc [expr $lineht * 2]	
				set x2 [expr $x1 + 15]
				set y2 [expr $y1 + 15]
				$tlegcan($mon) create text $x1 $y2 -anchor sw -width 250 \
					-fill $tfontcolor -font $titlefont -text "$label"
			}
			subtitle {
				regexp {.*\s(\d*)\s.*} $subtitlefont string lineht
				set yinc [expr $lineht * 2]	
				set x2 [expr $x1 + 15]
				set y2 [expr $y1 + 15]
				$tlegcan($mon) create text $x1 $y2 -anchor sw -width 250 \
					-fill $tfontcolor -font $subtitlefont -text "$label"
				incr y2 10
				$tlegcan($mon) create line $x1 $y2 [expr $x1 + 250] $y2 \
					-width 1 -fill #000000				
				incr y1 10
			}
			text {
				regexp {.*\s(\d*)\s.*} $labelfont string lineht
				set yinc [expr $lineht * 2]	
				set x2 [expr $x1 + 15]
				set y2 [expr $y1 + 15]
				$tlegcan($mon) create text $x1 $y2 -anchor sw -width 250 \
					-fill $lfontcolor -font $labelfont -text "$label"
			}
			area {
				regexp {.*\s(\d*)\s.*} $labelfont string lineht
				set yinc [expr $lineht * 2]	
				set x2 [expr $x1 + 15]
				set y2 [expr $y1 + 15]
				$tlegcan($mon) create rectangle $x1 $y1 $x2 $y2 -fill $xfcolor \
					-outline $xlcolor
				$tlegcan($mon) create text [expr $x2 + 15] [expr (($y2-$y1)/2) + $y1] \
				-fill $lfontcolor -anchor w -font $labelfont -text "$label"
			}
			point {
				regexp {.*\s(\d*)\s.*} $labelfont string lineht
				set yinc [expr $lineht * 2]	
				if { $size > [expr $yinc + 2] } {
					incr y1 [expr int(($size/5) + 2)]
				}
				if { $txtx <= [expr $x1 + $size + 15] } {
					set txtx [expr $x1 + $size + 15]
				}
				set x2 [expr $x1 + $size]
				set y2 [expr $y1 + $size]
				$tlegcan($mon) create oval $x1 $y1 $x2 $y2 -fill $xfcolor \
					-outline $xlcolor
				$tlegcan($mon) create text $txtx [expr (($y2-$y1)/2) + $y1] \
				-fill $lfontcolor -anchor w -font $labelfont -text "$label"
			}
			line {
				regexp {.*\s(\d*)\s.*} $labelfont string lineht
				set yinc [expr $lineht * 2]	
				set x2 [expr $x1 + 15]
				set y2 [expr $y1 + 15]
				$tlegcan($mon) create line $x1 $y1 $x2 $y2 -width $size  \
					-fill $xlcolor
				$tlegcan($mon) create text [expr $x2 + 15] [expr (($y2-$y1)/2) + $y1] \
				-fill $lfontcolor -anchor w -font $labelfont -text "$label"
			}
			default { break }
		}
		if { $size > $yinc } {
			incr y1 [expr int($size + 2)]
		} else {
			incr y1 $yinc
		}
	}
	

	return
}

# rgb to hex color convertor
proc GmThematic::rgb2hex { clr } {
	set rgb [split $clr :]
	set r [lindex $rgb 0]
	set g [lindex $rgb 1]
	set b [lindex $rgb 2]
	if {$r == "" || $g == "" || $b == ""} {return}
	set xclr [format "#%02x%02x%02x" $r $g $b]
	return $xclr
}

# erase legend canvas
proc GmThematic::tleg_erase { mon } {
	variable tlegcan
	
	$tlegcan($mon) delete all
	return
}

#save legend canvas (might use maptool procedures)
proc GmThematic::tleg_save { mon } {
	global env
	variable tlegcan
		
	set types {
    {{EPS} {.eps}}
	}

	if { [info exists HOME] } {
		set dir $env(HOME)
		set path [tk_getSaveFile -filetypes $types -initialdir $dir \
			-defaultextension ".eps"]
	} else {
		set path [tk_getSaveFile -filetypes $types -defaultextension ".eps"]
	}
	
	$tlegcan($mon) postscript -file "$path"

	return
}