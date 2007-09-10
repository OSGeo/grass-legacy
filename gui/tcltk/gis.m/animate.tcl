#!/bin/sh
# the next line restarts using wish \
# exec $GRASS_WISH "$0" "$@"
##########################################################################
#
# animate.tcl
#
# animates one or more series of raster maps in GRASS 6 TclTk GUI
# Author: Glynn Clements and Michael Barton (Arizona State University)
#
# September 2007
#
# COPYRIGHT:	(C) 1999 - 2007 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

namespace eval GmAnim {

    variable view1; #maps to animate in frame 1
    variable view2; #maps to animate in frame 2
    variable view3; #maps to animate in frame 3
    variable view4; #maps to animate in frame 4
    
    variable first # first time animation is run (0 1)
    variable cnt; # number of maps to animate
    variable tmpfile; # temp file for storing images
    variable border
    variable numviews
    variable numframes
    variable flabel
    variable step
    variable speed
    variable stop
    variable direction
    variable rewind
    variable prevframe
    variable currframe
    variable nframes
    global loop
    global swing
    variable shownames
     
    variable ncols 
    variable nrows 
    variable icols 
    variable irows 
    variable vcols 
    variable vrows 
    variable vscale
    variable pic_array 
    variable vfiles 
    variable label_pos
    variable tmpfile 
    variable border
    variable anim_can
    
    variable cb_loop # control button for looping
    variable cb_swing # control button for swing




    set border    2
    set cnt       0
    set numviews  1
    set numframes 0
    set flabel    0
    set first     0
    set step      0
    set speed     100
    set stop      1
    set direction 1
    set rewind    0
    set prevframe 0
    set currframe 1
    set nframes   0
    set loop      0
    set swing     0
    set shownames 0
    
	# create file for temporary image output
	if {[catch {set tmpfile [exec g.tempfile pid=[pid]]} error]} {
		Gm::errmsg $error [G_msg "Error creating tempfile"]
	}
    

}


########################################################
# Animation control button procedures

proc GmAnim::cmd_rewind {} {
    variable step 
    variable stop 
    variable rewind
    
    set step 0
    set stop 1
    set rewind 1
}

proc GmAnim::cmd_rplay {} {
    variable step 
    variable stop 
    variable direction 
    variable currframe 
    variable prevframe
    
    set step 0
    set stop 0
    set direction -1
    set currframe [expr $prevframe + $direction]
}

proc GmAnim::cmd_stepb {} {
    variable step 
    variable direction 
    variable currframe 
    variable prevframe
    
    set step 1
    set direction -1
    set currframe [expr $prevframe + $direction]
}

proc GmAnim::cmd_stop {} {
    variable stop
    set stop 1
}

proc GmAnim::cmd_stepf {} {
    variable step 
    variable direction 
    variable currframe 
    variable prevframe
    
    set step 1
    set direction 1
    set currframe [expr $prevframe + $direction]
}

proc GmAnim::cmd_play {} {
    variable step 
    variable stop 
    variable direction 
    variable currframe 
    variable prevframe
    
    set step 0
    set stop 0
    set direction 1
    set currframe [expr $prevframe + $direction]
}

proc GmAnim::cmd_loop {} {
    global swing 
    global loop
    variable stop 
    variable cb_swing

    $cb_swing deselect
    set swing 0
}

proc GmAnim::cmd_swing {} {
    global loop
    global swing
    variable stop 
    variable cb_loop
    
    $cb_loop deselect
    set loop 0
}

proc GmAnim::cmd_slower {} {
    variable speed
    if {$speed > 1} {
        if {$speed < 200000} {
            set speed [expr $speed * 3]
        }
    } else {
	    set speed 1
    }
}

proc GmAnim::cmd_faster {} {
    variable speed
    
    if {$speed > 1} {
	    set speed [expr $speed / 3]
    }
}

proc GmAnim::cmd_names {} {
    variable shownames
    
    set shownames [expr (1 + $shownames) % 3]
}

proc GmAnim::cmd_exit {} {
    variable numviews
    variable numframes
    variable vfiles
    variable pic_array
    
    # close all windows and delete temporary image file
    catch {if { [winfo exists .animwin] } { destroy .animwin }}
    catch {if { [winfo exists .animmaps_win] } { destroy .animmaps_win }}
    catch {if { [file exists $tmpfile] } {file delete -force $tmpfile }}
    set numviews 1
    set numframes 0
    if {[array exists vfiles]} {array unset vfiles}
    if {[array exists pic_array]} {array unset pic_array }

}

proc GmAnim::make_buttons {anim_tb} {
    global bgcolor
    global iconpath
    global loop 
    global swing 
    variable flabel
    variable cb_loop
    variable cb_swing
    
    set selclr #88aa88
    
    # make button images
    image create photo img_rast   -file "$iconpath/element-cell.gif"
    image create photo img_rewind -file "$iconpath/gui-rewind.gif"
    image create photo img_rplay  -file "$iconpath/gui-rplay.gif" 
    image create photo img_stepb  -file "$iconpath/gui-stepb.gif" 
    image create photo img_stop   -file "$iconpath/gui-stop.gif"  
    image create photo img_stepf  -file "$iconpath/gui-stepf.gif" 
    image create photo img_play   -file "$iconpath/gui-play.gif"  
    image create photo img_loop   -file "$iconpath/gui-loop.gif"  
    image create photo img_swing  -file "$iconpath/gui-swing.gif" 
    image create photo img_snail  -file "$iconpath/gui-snail.gif" 
    image create photo img_rabbit -file "$iconpath/gui-rabbit.gif"


    #Create button bars

	set bbox0 [ButtonBox $anim_tb.bbox0 -spacing 0 ]
	
	# Select maps
	$bbox0 add -command GmAnim::sel_maps -image img_rast \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor  -activebackground $bgcolor\
		-helptext [G_msg "Select maps to animate"]

    pack $bbox0 -side left -anchor w

	set sep0 [Separator $anim_tb.sep0 -orient vertical ]
	pack $sep0 -side left -fill y -padx 5 -anchor w


	set bbox1 [ButtonBox $anim_tb.bbox1 -spacing 0 ]

	# Rewind
	$bbox1 add -command GmAnim::cmd_rewind -image img_rewind \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor  -activebackground $bgcolor\
		-helptext [G_msg "Rewind animation"]

	# Replay
	$bbox1 add -command GmAnim::cmd_rplay  -image img_rplay \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Replay animation"]

	# Step backwards
	$bbox1 add -command GmAnim::cmd_stepb  -image img_stepb \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Step backwards through animation"]

	# Stop
	$bbox1 add -command GmAnim::cmd_stop   -image img_stop \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Stop animation"]

	# Step forwards
	$bbox1 add -command GmAnim::cmd_stepf  -image img_stepf \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Step forwards through animation"]

	# Play
	$bbox1 add -command GmAnim::cmd_play   -image img_play \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Play animation"]
		
    pack $bbox1 -side left -anchor w

	set sep1 [Separator $anim_tb.sep1 -orient vertical ]
	pack $sep1 -side left -fill y -padx 5 -anchor w

	set bbox2 [ButtonBox $anim_tb.bbox2 -spacing 0 ]

	# Slower
	$bbox2 add -command GmAnim::cmd_slower -image img_snail \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Slower animation"]

	# Faster
	$bbox2 add -command GmAnim::cmd_faster -image img_rabbit \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Faster animation"]

    pack $bbox2 -side left -anchor w

	# Loop
	set cb_loop [checkbutton $anim_tb.loop -command "GmAnim::cmd_loop" -image img_loop \
	    -variable loop -offvalue 0 -onvalue 1 -relief flat \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selclr \
		-activebackground $bgcolor -highlightbackground $bgcolor ]
	
    DynamicHelp::register $cb_loop balloon [G_msg "Continuously loop through animation"]
    

	# Swing
	set cb_swing [checkbutton $anim_tb.swing -command "GmAnim::cmd_swing" -image img_swing \
	    -variable swing -offvalue 0 -onvalue 1 -relief flat \
		-borderwidth 1 -indicatoron false -bg $bgcolor -selectcolor $selclr \
		-activebackground $bgcolor -highlightbackground $bgcolor ]
	
    DynamicHelp::register $cb_swing balloon [G_msg "Run animation alternately forward and backward"]

    pack $cb_loop $cb_swing -side left -anchor w
    
	set sep2 [Separator $anim_tb.sep2 -orient vertical ]
	pack $sep2 -side left -fill y -padx 5 -anchor w

	set bbox3 [ButtonBox $anim_tb.bbox3 -spacing 0 ]

	# Show names
	$bbox3 add -command GmAnim::cmd_names  -text Names \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Show names"]

	# Quit
	$bbox3 add -command GmAnim::cmd_exit   -text Exit \
		-highlightthickness 0 -takefocus 0 -relief link -borderwidth 1	\
		-highlightbackground $bgcolor -activebackground $bgcolor \
		-helptext [G_msg "Quit animation"]

    pack $bbox3 -side left -anchor w
#    pack $buttons.rew $buttons.rplay $buttons.stepb $buttons.stop $buttons.stepf $buttons.play $buttons.loop $buttons.swing $buttons.slower $buttons.faster $buttons.shownames $buttons.exit -side left -expand 1
}



########################################################
# select maps to animate
proc GmAnim::select_map { mapgroup } {

    variable view1
    variable view2
    variable view3
    variable view4
    variable first

    set mapvar "GmAnim::view"
    append mapvar $mapgroup

    set m [GSelect cell multiple title [G_msg "Select maps"] parent "."]
    if { $m != "" } {
        if {$mapvar == ""} { 
            set $mapvar $m 
        } else { 
            set m ", $m"
            append $mapvar $m 
        }
    }

    set GmAnim::first 1
}

########################################################
proc GmAnim::sel_maps {} {

    # window for selecting maps and frames to animate
    global iconpath
    global bgcolor
    variable view1
    variable view2
    variable view3
    variable view4
    
    set view1 ""
    set view2 ""
    set view3 ""
    set view4 ""


	# Create raster map input window
	set mapswin [toplevel .animmaps_win]
	wm title $mapswin [ G_msg "Maps for Animation" ]
	# put it in the middle of the screen
	update idletasks
	set winWidth [winfo reqwidth $mapswin]
	set winHeight [winfo reqheight $mapswin]
	set scrnWidth [winfo screenwidth $mapswin]
	set scrnHeight [winfo screenheight $mapswin]
	set x [expr ($scrnWidth - $winWidth) / 2-250]
	set y [expr ($scrnHeight  - $winHeight) / 2]
	wm geometry $mapswin +$x+$y
	wm deiconify $mapswin
	
	# keep this on top somehow
            
    # Heading
    set row [ frame $mapswin.heading ]
    Label $row.a -text [G_msg "Select maps to animate in one or more frames (1 frame required)"] \
    	-fg MediumBlue
    Label $row.b -text "   "
    Button $row.c -text [G_msg "Help"] \
		-image [image create photo -file "$iconpath/gui-help.gif"] \
		-command "spawn g.manual --q xganim" \
		-background $bgcolor -helptext [G_msg "Help"]
    pack $row.a $row.b $row.c -side left
    pack $row -side top -fill both -expand yes
	
    # Frame 1
    set row [ frame $mapswin.view1 ]
    Label $row.a -text [G_msg "Maps for frame 1 (required): "]
    Button $row.b -image [image create photo -file "$iconpath/element-cell.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmAnim::select_map 1"
    Entry $row.c -width 35 -text " $view1" \
          -textvariable GmAnim::view1
    pack $row.c $row.b $row.a -side right
    pack $row -side top -fill both -expand yes

    # Frame 2
    set row [ frame $mapswin.view2 ]
    Label $row.a -text [G_msg "Maps for frame 2 (optional): "]
    Button $row.b -image [image create photo -file "$iconpath/element-cell.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmAnim::select_map 2"
    Entry $row.c -width 35 -text " $view2" \
          -textvariable GmAnim::view2
    pack $row.c $row.b $row.a -side right
    pack $row -side top -fill both -expand yes

    # Frame 3
    set row [ frame $mapswin.view3 ]
    Label $row.a -text [G_msg "Maps for frame 3 (optional): "]
    Button $row.b -image [image create photo -file "$iconpath/element-cell.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmAnim::select_map 3"
    Entry $row.c -width 35 -text " $view3" \
          -textvariable GmAnim::view3
    pack $row.c $row.b $row.a -side right
    pack $row -side top -fill both -expand yes

    # Frame 4
    set row [ frame $mapswin.view4 ]
    Label $row.a -text [G_msg "Maps for frame 4 (optional): "]
    Button $row.b -image [image create photo -file "$iconpath/element-cell.gif"] \
        -highlightthickness 0 -takefocus 0 -relief raised -borderwidth 1  \
		-command "GmAnim::select_map 4"
    Entry $row.c -width 35 -text " $view4" \
          -textvariable GmAnim::view4
    pack $row.c $row.b $row.a -side right
    pack $row -side top -fill both -expand yes
    

    set row [ frame $mapswin.buttons ]
    Button $row.a -text [G_msg "OK"] -width 8 -bd 1 \
    	-command "GmAnim::create_viewlist 1" 
    Button $row.b -text [G_msg "Cancel"] -width 8 -bd 1 \
    	-command "destroy .animmaps_win"
    Button $row.c -text [G_msg "Apply"] -width 8 -bd 1 \
    	-command "GmAnim::create_viewlist 0"
    pack $row.a $row.b $row.c -side right -padx 5
    pack $row -side bottom -pady 3 -padx 5 -expand 0 -fill none -anchor e
    
    
}


########################################################

proc GmAnim::create_viewlist { closeval } {
    variable view1
    variable view2
    variable view3
    variable view4
    variable numviews
    
    set viewlist {}

    if { $view1 == "" } {
        tk_messageBox -type ok -icon warning -parent .animmaps_win \
			-message [G_msg "You must select maps to animate for frame 1"] \
			-title [G_msg "No maps selected"]
		return
	}

    # creat list of views with maps to animate
    set viewlist "view1=$view1"
    
    if { $view2 != "" } {
        lappend viewlist "view2=$view2"
    }
    
    if { $view3 != "" } {
        lappend viewlist "view3=$view3"
    }
    
    if { $view4 != "" } {
        lappend viewlist "view4=$view4"
    }
        
    if { $closeval == 1 } { destroy .animmaps_win }
    
    GmAnim::parse_viewmaps $viewlist
    
}

########################################################

proc GmAnim::parse_viewmaps {viewlist} {
    variable vfiles 
    variable numviews 
    variable numframes
    variable first
    variable pic_array

    # Reset variables
    set numviews 1
    set numframes 0
    if {[array exists vfiles]} {array unset vfiles}
    if {[array exists pic_array]} {array unset pic_array }

    set allmaps [exec g.list rast | grep -v {^raster files available} | grep -v {^---}]    

    foreach view $viewlist {
        if {![regexp -- {^view([1-4])=(.*)$} $view dummy num val]} {
            error "invald argument: $view"
        }
        set maps {}
        set pats [split $val ,]

        foreach pat $pats {
            foreach map $allmaps {
                if {[string match $pat $map]} {
                    lappend maps $map
                }
            }
        }
        
        set vfiles($numviews) $maps
        set numframes [llength $maps]
        
        incr numviews
        update
    }
    
    set first 1
        
}

########################################################
proc GmAnim::do_run {} {
    global loop 
    global swing 
    variable first
    variable vfiles
    variable pic_array
    variable label_pos
    variable numviews 
    variable nframes
    variable step 
    variable stop 
    variable rewind 
    variable shownames
    variable currframe 
    variable prevframe 
    variable direction 
    variable speed
    variable flabel 
    variable anim_can
    variable cnt
    

    if {$first} {
        set first 0
        set nframes [GmAnim::load_files]
        set currframe [expr $direction > 0 ? 1 : $nframes]
        set prevframe $currframe
    }

    if {$rewind} {
        set rewind    0
        set currframe  1
        set direction 1
        set step      1
    }

    if {$stop == 1 && $step == 0} {
	    return
    }

    if {$swing} {
        if {$currframe == $nframes } {
            set direction -1
        } elseif { $currframe == 1} {
            set direction 1
        }
        #incr currframe $direction
    } elseif {$loop} {
        if {$currframe == $nframes} {
            set currframe 1
        } 
    } elseif {$currframe == $nframes || $currframe == 1} {
    	set stop 1
    }


    if {$currframe <= $nframes && $currframe >= 1} {
        # This is the main loop for displaying animation images
        $anim_can delete all
        $anim_can create image 0 0 -anchor nw -image $pic_array($currframe)
    
        # draw labels
        if {$shownames > 0} {
            for {set i 1} {$i < $numviews} {incr i} {
                set x [expr $label_pos($i,0) + 10]
                set y [expr $label_pos($i,1) - 5]
                set s [lindex $vfiles($i) $currframe]
                if {$shownames == 1} {
                    set fg black
                } else {
                    set fg white
                }
                $anim_can create text $x $y -text $s -fill $fg
            }
        }
    
        set flabel [expr $currframe + 1]
    
        set prevframe $currframe
    }

    incr currframe $direction

    if {$step} {
        set step 0
        set stop 1
    }
}

########################################################
proc GmAnim::load_files {} {
    # exports maps to ppm and displays them in a TclTk canvas
    
    variable numframes 
    variable numviews
    variable ncols 
    variable nrows 
    variable icols 
    variable irows 
    variable vcols 
    variable vrows 
    variable vscale
    variable pic_array 
    variable vfiles 
    variable label_pos
    variable tmpfile 
    variable border
    variable anim_can
    variable cnt
    
    for {set cnt 0} {$cnt < $numframes} {incr cnt} {
        set img [image create photo -width $ncols -height $nrows]
        set pic_array([expr $cnt + 1]) $img
    
        for {set vnum 1} {$vnum < $numviews} {incr vnum} {
            if {$icols == $vcols} {
                set vxoff $border
                set vyoff [expr $irows == $vrows ? $border : $border + $vnum * ($border + $vrows)]
            } elseif {$irows == $vrows} {
                set vxoff [expr $icols == $vcols ? $border : $border + $vnum * ($border + $vcols)]
                set vyoff $border
            } else { # 4 views
                set vxoff [expr $vnum % 2 ? $border : $vcols + 2 * $border]
                set vyoff [expr $vnum > 1 ? $vrows + 2 * $border : $border]
            }
    
            if {! $cnt} {
                set label_pos($vnum,0) $vxoff
                set label_pos($vnum,1) [expr $vyoff + $vrows - 1]
            }
    
            set name [lindex $vfiles($vnum) $cnt]
    
#            catch {exec r.out.ppm input=$name output=- 2>@stderr | pnmscale $vscale >$tmpfile 2>@stderr}
            catch {exec r.out.ppm input=$name output=- 2>@stderr >$tmpfile 2>@stderr}
            set subimg [image create photo -file $tmpfile]
            file delete $tmpfile
    
            $img copy $subimg \
                -to $vxoff $vyoff [expr $vxoff + $vcols] [expr $vyoff + $vrows]
    
            image delete $subimg
        }
    
        $anim_can delete all
        $anim_can create image 0 0 -anchor nw -image $img
        set flabel [expr $cnt + 1]
        update
    }

    return $cnt
}

########################################################
proc GmAnim::cmd_idle {} {
    variable speed
    
    GmAnim::do_run
    after $speed GmAnim::cmd_idle
}

########################################################
proc GmAnim::main {} {
    variable numframes 
    variable numviews
    variable ncols 
    variable nrows 
    variable icols 
    variable irows 
    variable vcols 
    variable vrows 
    variable vscale
    variable border
    variable speed 
    variable direction 
    variable shownames
    variable anim_can

    set region [exec g.region -p]

    regexp {rows: *([0-9]+)} $region dummy vrows
    regexp {cols: *([0-9]+)} $region dummy vcols
    regexp {nsres: *([0-9]+)} $region dummy vres1
    regexp {ewres: *([0-9]+)} $region dummy vres2
    
    set vres [expr $vres1 > $vres2 ? $vres1 : $vres2]
   # puts "vres = $vres"
   # puts "numviews = $numviews"

    set nrows $vrows
    set ncols $vcols

    # short dimension
    upvar 0 [expr {$nrows > $ncols ? "ncols" : "nrows"}] sdim

    # these proportions should work fine for 1 or 4 views, but for
    # 2 views, want to double the narrow dim & for 3 views triple it

    if {$numviews == 2} {
    	set sdim [expr $sdim * 2]
    } elseif {$numviews == 3} {
    	set sdim [expr $sdim * 3]
    }

    set longdim [expr $nrows > $ncols ? $nrows : $ncols]
    set scale 1.0

    set max 900
    set min 600

    if {[array get env XGANIM_SIZE] != ""} {
    	set max $env(XGANIM_SIZE)
	    set min $env(XGANIM_SIZE)
    }

    if {$longdim > $max} {   # scale down
    	set scale [expr 1.0 * $max / $longdim]
    } elseif {$longdim < $min} { # scale up
    	set scale [expr 1.0 * $min / $longdim]
    }

    set vscale $scale

    if {$numviews == 4} {
    	set vscale [expr $scale / 2.0]
    }

    set nrows [expr int($nrows * $scale)]
    set ncols [expr int($ncols * $scale)]
    # now nrows & ncols are the size of the combined - views image
    set vrows [expr int($vrows * $vscale)]
    set vcols [expr int($vcols * $vscale)]
    # now vrows & vcols are the size for each sub-image

    # add to nrows & ncols for borders
    # irows, icols used for vert/horizontal determination in loop below
    set irows $nrows
    set icols $ncols

    set nrows [expr int($nrows + (1 + ($nrows/$vrows)) * $border)]
    set ncols [expr int($ncols + (1 + ($ncols/$vcols)) * $border)]

    if {$ncols > $nrows} {
        set w $ncols
        set h [expr $nrows + 60]
    } else {
        set w [expr $ncols + 80]
        set h $nrows
    }

    #puts "nrows, ncols, vrows, vcols = $nrows, $ncols, $vrows, $vcols"

    # create file viewing frame
	toplevel .animwin
	wm title .animwin [G_msg "Animation Window"]

	set anim_fr [MainFrame .animwin.mf \
			-textvariable GmAnim::msg \
			-progressvar drawprog -progressmax 100 -progresstype incremental]

	set mf_frame [$anim_fr getframe]

	# toolbar creation
	set anim_tb	[$anim_fr addtoolbar]
	#MapToolBar::create $map_tb

	# canvas creation
	set anim_can [canvas $mf_frame.canvas \
		-borderwidth 0 -closeenough 10.0 -relief groove \
		-width $ncols -height $nrows ]

	# setting geometry
	place $anim_can -in $mf_frame -x 0 -y 0 -anchor nw
	pack $anim_can -fill both -expand yes

	# indicator creation
	set anim_ind [$anim_fr addindicator -textvariable animstatus \
		-width 33 -justify left -padx 5 -bg white]

	pack $anim_fr -fill both -expand yes
	
	# create animation control buttons
	GmAnim::make_buttons $anim_tb

    # bindings for closing window
	bind .animwin <Destroy> "GmAnim::cmd_exit"

    update

    # start animation timer
    after $speed GmAnim::cmd_idle
    
}
########################################################

#GmAnim::main
