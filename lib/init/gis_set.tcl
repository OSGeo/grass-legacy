#############################################################################
#
# $Id$
#
# MODULE:   	Grass Tcl/Tk Initialization
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
#   	    	Markus Neteler - Germany - neteler@geog.uni-hannover.de
# PURPOSE:  	The source file for this shell script is in
#   	    	src/tcltkgrass/main/gis_set.tcl. It allows the user to choose
#   	    	the database, location, and mapset to use with grass by
#   	    	presenting a user interface window.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

source $env(GISBASE)/etc/gtcltk/gmsg.tcl
#############################################################################
#
#   part regarding to the creation of a new location using proj and 
#   the EPSG codes    (routines epsgLocCom and infoEpsg)
#
#############################################################################
source $env(GISBASE)/etc/epsg_option.tcl

#HTML help
source $env(GISBASE)/docs/nviz/help.tcl
proc make_help_window { w } {

        frame $w
        frame $w.f2
        frame $w.f1

        pack $w -side top -expand 1 -fill both
        pack $w.f1 -side top -expand 1 -fill both
        pack $w.f2 -side bottom -expand 0 -fill both

}

proc searchGISRC { filename } {
 
  global database
  global location
  global mapset
  global oldDb oldLoc oldMap

  global grassrc_list
  set grassrc_list ""

  set flag 0
  if { [file exists $filename] } {
      set ifp [open $filename "r"]
      set thisline [gets $ifp]
      while { [eof $ifp] == 0 } {

            lappend grassrc_list "$thisline"

            if { [scan $thisline "GISDBASE: %s" env_database] } {
                set database $env_database
            }
            if { [scan $thisline "LOCATION_NAME: %s" env_location] } {
                set location $env_location
            }
            if { [scan $thisline "MAPSET: %s" env_mapset] } {
                set mapset $env_mapset
            }
            set thisline [gets $ifp]
      }
      
      set oldDb $database
      set oldLoc $location
      set oldMap $mapset
      
      close $ifp
      if { $database != "" && $location != "" && $mapset != "" } {
         set flag 1
      }
  }
  return $flag
}

proc putGRASSRC { filename } {
 
  global database
  global location
  global mapset

  global grassrc_list

  set ofp [open $filename "w"]

  foreach i $grassrc_list {
      if { [regexp {^GISDBASE:} $i] } \
      {
	  puts $ofp "GISDBASE: $database"
      } \
      elseif { [regexp {^LOCATION_NAME:} $i] } \
      {
 	  puts $ofp "LOCATION_NAME: $location"
      } \
      elseif { [regexp {^MAPSET:} $i] } \
      {
          puts $ofp "MAPSET: $mapset"
      } \
      else \
      {
      	  puts $ofp $i
      }
  }

  close $ofp
}

proc GetDir {entWidget locList mapList} \
{
    global database
    
    toplevel .getDir
    wm title .getDir "New Database Directory"
    wm resizable .getDir 0 0
    
    frame .getDir.base 
    pack .getDir.base -side top -fill both -expand 1
    
    listbox .getDir.base.list -selectmode single -height 10 -width 40\
    	-yscrollcommand ".getDir.base.vertScroll set" \
    	-xscrollcommand ".getDir.horzScroll set"
    pack .getDir.base.list -side left -fill x -expand 1
    scrollbar .getDir.base.vertScroll -width 12 \
    	-command ".getDir.base.list yview"
    scrollbar .getDir.horzScroll -width 12 -command ".getDir.base.list xview" \
    	-orient horizontal
    pack .getDir.base.vertScroll -side left -fill y
    pack .getDir.horzScroll -side top -fill x

    # Create a frame for the pushbuttons
    frame .getDir.sepFrame -height 4 -bd 2 -relief raised
    pack .getDir.sepFrame -side top -fill x
    frame .getDir.buttonFrame 
    pack .getDir.buttonFrame -side bottom -fill x
    
    # Create the pushbuttons
    button .getDir.buttonFrame.add -text "OK" \
    	-command \
	    "SetDatabase .getDir.base.list .getDir $entWidget $locList $mapList"
    button .getDir.buttonFrame.quit -text "Cancel" -command {destroy .getDir}
    pack .getDir.buttonFrame.add .getDir.buttonFrame.quit -side left \
    	-expand 1 -padx 10 -pady 5

    # Bind the double click to change directories
    bind .getDir.base.list <Double-Button-1> {ChangeDir %W %y}
    GetListItems .getDir.base.list [file dirname $database] \
    	[file tail $database]
}

proc GetListItems {widget dir default} \
{
    set currDir [pwd]
    cd $dir
    
    # Insert the parent directory in the list
    $widget delete 0 end
    set parent $dir
    append parent "/.."
    $widget insert end $parent
    
    # Set counter to 1 since parent is already in the list
    set i 1
    set index 0
    
    foreach filename [lsort [glob -nocomplain *]] \
    {
	if {[file isdirectory $filename]} \
	{
	    if {[string compare $default $filename] == 0} \
	    {
		set index $i
	    }
	
	    set path $dir
	    append path "/" $filename
	    set filename $path
	
	    $widget insert end $filename

	    incr i
	}
    }
    
    $widget see $index
    $widget selection set $index
    $widget xview moveto 1
    
    cd $currDir
}

proc SetDatabase {widget top entryWidget locList mapList} \
{
    global database
    
    set database [$widget get [$widget curselection]]
    
    if {[string compare [file tail $database] ".."] == 0} \
    {
    	set database [file dirname [file dirname $database]]
    }
    
    $entryWidget xview moveto 1
    
    cd $database

    $locList delete 0 end
    $mapList delete 0 end
    
    foreach filename [lsort [glob -nocomplain *]] \
    {
	if {[file isdirectory $filename]} \
	{
	    $locList insert end $filename
	}
    }
    
    .frame0.frameBUTTONS.ok configure -state disabled
    destroy $top
}

proc ChangeDir {widget y} \
{
    set dir [$widget get [$widget nearest $y]]
    
    if {[string compare [file tail $dir] ".."] == 0} \
    {
    	set dir [file dirname [file dirname $dir]]
    }
    
    GetListItems $widget $dir ""
}

proc CheckLocation {} \
{
    global database location
    
    set found 0
    set dir $database
    append dir "/$location"
    
    set currDir [pwd]
    cd $dir
    
    foreach filename [glob -nocomplain *] \
    {
	if {[string compare $filename "PERMANENT"] == 0} \
	{
	    set found 1
	}
    }
    
    if {$found == 0} \
    {
    	set location "##ERROR##"
    }
    
    cd $currDir
}



proc gisSetWindow {} {

    # Window manager configurations

    wm geometry . +100+100
    wm title . "Grass 5.7 Data Selection"

    global database
    global location
    global mymapset
    global mapset
    global oldDb oldLoc oldMap

    global grassrc_list
    global gisrc_name

    # ---------------------------
    # build .frame0
    # ---------------------------
    frame .frame0 \
    	-borderwidth {2} \
    	-relief {raised}


    frame .frame0.intro -borderwidth 2
    text .frame0.intro.msg -relief ridge -height 4 -width 50 \
    	-font {Helvetica -12 bold}
    pack .frame0.intro -side top
    pack .frame0.intro.msg -side top

    .frame0.intro.msg tag configure all -justify center
    .frame0.intro.msg insert end [G_msg "Welcome to GRASS GIS Version 5.7\n\n"]
    .frame0.intro.msg insert end [G_msg "Please select location and mapset\n"]
    .frame0.intro.msg insert end [G_msg "or define a new location\n"]
    .frame0.intro.msg tag add all 1.0 end
    .frame0.intro.msg configure -state disabled

    # -----------------------------------
    # build .frame0.frameDB
    # -----------------------------------

    frame .frame0.frameDB \
    	-borderwidth {2}

    frame .frame0.frameDB.left \
    	-borderwidth {2}

    frame .frame0.frameDB.mid \
    	-borderwidth {2}

    frame .frame0.frameDB.right \
    	-borderwidth {2}

    label .frame0.frameDB.left.label \
    	-anchor {n} \
    	-text [G_msg "Database : "]

    entry .frame0.frameDB.mid.entry \
    	-relief {sunken} \
    	-textvariable database \
	-width 40 \
    	-xscrollcommand { .frame0.frameDB.mid.hscrollbar set}
    
    scrollbar .frame0.frameDB.mid.hscrollbar \
    	-command { .frame0.frameDB.mid.entry xview} \
    	-relief {raised} \
    	-width 12 \
    	-orient {horizontal}
 
    button .frame0.frameDB.right.button \
    	-text [G_msg "Browse..."] \
    	-command {GetDir .frame0.frameDB.mid.entry .frame0.frameLOC.listbox \
    	    .frame0.frameMS.listbox}


    pack .frame0.frameDB.left.label -side top
    pack .frame0.frameDB.mid.entry -side top -fill x
    pack .frame0.frameDB.mid.hscrollbar -side bottom -fill x
    pack .frame0.frameDB.right.button -side left -fill x
    pack .frame0.frameDB.left -side left  -anchor n -fill x
    pack .frame0.frameDB.mid -side left -fill x
    pack .frame0.frameDB.right -side left -anchor n -fill x

    # -----------------------------------
    # build .frame0.frameLOC
    # -----------------------------------
    frame .frame0.frameLOC \
    	-borderwidth {2}

    label .frame0.frameLOC.label \
    	-anchor {w} \
    	-text [G_msg "Location"] 

    listbox .frame0.frameLOC.listbox \
    	-relief {raised} \
    	-exportselection false \
    	-yscrollcommand {.frame0.frameLOC.vscrollbar set} \
    	-xscrollcommand {.frame0.frameLOC.hscrollbar set}

    scrollbar .frame0.frameLOC.vscrollbar -width 12 \
    	-command {.frame0.frameLOC.listbox yview} \
    	-relief {raised}

    scrollbar .frame0.frameLOC.hscrollbar -width 12 \
    	-command {.frame0.frameLOC.listbox xview} \
    	-orient {horizontal} \
    	-relief {raised}

    pack append .frame0.frameLOC \
    	.frame0.frameLOC.label { top fill } \
    	.frame0.frameLOC.vscrollbar { right filly } \
    	.frame0.frameLOC.hscrollbar { bottom fillx } \
    	.frame0.frameLOC.listbox { left expand fill }


    # -----------------------------------
    # build .frame0.frameMS
    # -----------------------------------
    frame .frame0.frameMS \
    	-borderwidth {2}

    label .frame0.frameMS.label \
    	-anchor {w} \
    	-text [G_msg "(Accessible) Mapsets"] 

    listbox .frame0.frameMS.listbox \
    	-relief {raised} \
    	-yscrollcommand {.frame0.frameMS.vscrollbar set} \
    	-xscrollcommand {.frame0.frameMS.hscrollbar set}

    scrollbar .frame0.frameMS.vscrollbar -width 12 \
    	-command {.frame0.frameMS.listbox yview} \
    	-relief {raised}

    scrollbar .frame0.frameMS.hscrollbar -width 12 \
    	-command {.frame0.frameMS.listbox xview} \
    	-orient {horizontal} \
    	-relief {raised}

    pack append .frame0.frameMS \
    	.frame0.frameMS.label { top fill } \
    	.frame0.frameMS.vscrollbar { right filly } \
    	.frame0.frameMS.hscrollbar { bottom fillx } \
    	.frame0.frameMS.listbox { left expand fill }

    # -----------------------------------
    # build .frame0.frameNMS
    # -----------------------------------
    frame .frame0.frameNMS \
    	-borderwidth {2}

    frame .frame0.frameNMS.left \
    	-borderwidth {2}

    frame .frame0.frameNMS.mid \
    	-borderwidth {2}

    frame .frame0.frameNMS.right \
    	-borderwidth {2}

    label .frame0.frameNMS.left.label \
    	-anchor {n} \
    	-text [G_msg "Create new mapset : "]

    entry .frame0.frameNMS.mid.entry \
    	-relief {sunken} \
    	-textvariable mymapset \
    	-width 15
	
    button .frame0.frameNMS.right.button \
    	-text [G_msg "Create..."] \
     	-command { 
            .frame0.frameNMS.right.button configure -state disabled
	    if { $mymapset != "" } {
            	CheckLocation
                cd $database
                cd $location
                file mkdir $mymapset
                file copy $mymapset/../PERMANENT/WIND $mymapset
                file attributes $mymapset/WIND -permissions u+rw,go+r
                .frame0.frameMS.listbox insert end $mymapset
                #TODO: select new MAPSET
            }
	}

    pack append .frame0.frameNMS
    pack .frame0.frameNMS.left.label -side top
    pack .frame0.frameNMS.mid.entry -side top -fill x
    pack .frame0.frameNMS.right.button -side left -fill x
    pack .frame0.frameNMS.left -side top  -anchor n
    pack .frame0.frameNMS.mid -side top -expand yes
    pack .frame0.frameNMS.right -side bottom -anchor n -expand yes

    # ----------------------------------
    # build .frame0.frameBUTTONS
    # ----------------------------------
    frame .frame0.frameBUTTONS \
    	-borderwidth {2}
    
    
    button .frame0.frameBUTTONS.ok \
     	-text [G_msg "Enter GRASS"] \
    	-relief raised \
     	-padx 10 \
     	-command { 
            if {[file exists "$database/$location/PERMANENT/WIND"] == 0} {
                DialogGen .wrnDlg "WARNING: not a mapset" warning "Warning: This is not \
                a valid mapset" \
                0 OK;
            }
            if { $mapset != "" && [file exists "$database/$location/PERMANENT/WIND"] != 0} {
            	CheckLocation
                puts stdout "GISDBASE='$database';"
                puts stdout "LOCATION_NAME='$location';"
                puts stdout "MAPSET='$mapset';"
                if {[string compare $location "##ERROR##"] != 0} {
                    putGRASSRC $gisrc_name
                }
                destroy .
            } 
        }

    button .frame0.frameBUTTONS.newLoc \
    	-text [G_msg "Create New Location"] \
    	-relief raised \
    	-padx 10 \
    	-command {
            puts stdout "OLD_DB='$oldDb';"
            puts stdout "OLD_LOC='$oldLoc';"
            puts stdout "OLD_MAP='$oldMap';"
	    puts stdout "GISDBASE='$database';"
    	    puts stdout "LOCATION_NAME='##NONE##';"
            puts stdout "MAPSET='';"
            set location ""
            set mapset ""
            putGRASSRC $gisrc_name
            destroy . 
            }

    button .frame0.frameBUTTONS.help \
    	-text [G_msg "Help"] \
    	-relief raised \
    	-padx 10 \
	-command {
		if { [winfo exists .help] } {
                     puts "Help already opened"
                     wm deiconify .help
                     raise .help
                     return
                }
                set help [toplevel .help]
		help::init $env(GISBASE)/docs/start/helptext.html "" $help 500 400
		wm title $help "GRASS Help"
        }
	
    button .frame0.frameBUTTONS.cancel \
    	-text [G_msg "Exit"] \
    	-relief raised \
    	-padx 10 \
    	-command { 
            puts stdout "exit" 
            destroy . 
        }

    #############################################################################
    button .frame0.frameBUTTONS.newLocEpsg \
    	-text [G_msg "Create Location From EPSG"] \
    	-relief raised \
    	-padx 10 \
    	-command {epsgLocCom}

    pack append .frame0.frameBUTTONS \
    	.frame0.frameBUTTONS.ok { left expand } \
    	.frame0.frameBUTTONS.newLoc {left expand } \
    	.frame0.frameBUTTONS.newLocEpsg {left expand } \
    	.frame0.frameBUTTONS.help { left expand } \
    	.frame0.frameBUTTONS.cancel { right expand }



    # ----------------------------------
    # packed it all
    # ----------------------------------

    # pack widget .frame0
    pack append .frame0 \
    	.frame0.frameDB { top expand fill } \
    	.frame0.frameBUTTONS { bottom expand fill } \
    	.frame0.frameLOC { left expand  } \
    	.frame0.frameMS { left expand  } \
     	.frame0.frameNMS { right expand fill }

    .frame0.frameNMS.right.button configure -state disabled

    pack append . \
    	.frame0 { top frame center expand fill }

    .frame0.frameDB.mid.entry xview moveto 1
    
    if { ! [file exists $database] } \
    {
      	DialogGen .wrnDlg "WARNING: Invalid Database" warning "WARNING: \
	    Invalid database. Finding first valid directory in parent tree" \
	    0 OK
      
      	while { ! [file exists $database] } \
      	{
	    set database [file dirname $database]
      	}
    }
    
    cd $database
    foreach i [exec ls -a [exec pwd]] {
      	if { [string compare $i "."] != 0 && \
            [string compare $i ".."] != 0 && \
            [file isdirectory $i] } {
            .frame0.frameLOC.listbox insert end $i
      	}
    }
        
    set i 0
    set curSelected 0
    set length [.frame0.frameLOC.listbox size]
    while { $i <  $length } {
    	if { $location == [.frame0.frameLOC.listbox get $i] } {
            set curSelected $i
            break
      	}
        
	incr i 1
    }
    
    .frame0.frameLOC.listbox select set $curSelected

    cd $database
    if { [file exists $location] } \
    {
	cd $location
	foreach i [exec ls -a [exec pwd]] {
     	    if { [string compare $i "."] != 0 && \
        	[string compare $i ".."] != 0 && \
        	[file isdirectory $i] && [file owned $i] } {
        	.frame0.frameMS.listbox insert end $i
      	    }
	}

	set i 0
	set curSelected 0
	set length [.frame0.frameMS.listbox size]
	while { $i <  $length } {
    	    if { $mapset == [.frame0.frameMS.listbox get $i] } {
        	set curSelected $i
        	break
            }

	    incr i 1
	}

	.frame0.frameMS.listbox yview $curSelected
	.frame0.frameMS.listbox select set $curSelected
    }
    


  bind .frame0.frameDB.mid.entry <Return> {
        set new_path [%W get]
        if { "$new_path" != "" \
             && [file exists $new_path] && [file isdirectory $new_path] } {
           %W delete 0 end
           %W insert 0 $new_path
           cd $new_path
           .frame0.frameLOC.listbox delete 0 end
           foreach i [exec ls -a [exec pwd]] {
               if { [string compare $i "."] != 0 && \
                    [string compare $i ".."] != 0 && \
                    [file isdirectory $i] } {
                   .frame0.frameLOC.listbox insert end $i
               }
           }
           .frame0.frameMS.listbox delete 0 end
           set database [exec pwd]
        }
	.frame0.frameBUTTONS.ok configure -state disabled
	.frame0.frameNMS.right.button configure -state disabled
  }

  bind .frame0.frameLOC.listbox <Double-ButtonPress-1> {
        %W select set [%W nearest %y]
	cd $database
        set location [%W get [%W nearest %y]]
        cd $location
        .frame0.frameMS.listbox delete 0 end
        foreach i [exec ls -a [exec pwd]] {
           if { [string compare $i "."] != 0 && \
                [string compare $i ".."] != 0 && \
                [file isdirectory $i] && [file owned $i] } { 
                .frame0.frameMS.listbox insert end $i
           }
        }
        set mapset ""
	.frame0.frameBUTTONS.ok configure -state disabled
  }

  bind .frame0.frameLOC.listbox <ButtonPress-1> {
        %W select set [%W nearest %y]
        cd $database
        set location [%W get [%W nearest %y]]
        cd $location
        .frame0.frameMS.listbox delete 0 end
        foreach i [exec ls -a [exec pwd]] {
           if { [string compare $i "."] != 0 && \
                [string compare $i ".."] != 0 && \
                [file isdirectory $i] && [file owned $i] } {
                .frame0.frameMS.listbox insert end $i
           }
        }
        set mapset ""
	.frame0.frameBUTTONS.ok configure -state disabled
  }

  bind .frame0.frameMS.listbox <Double-ButtonPress-1> {
        %W select set [%W nearest %y]
        set mapset [%W get [%W nearest %y]]
	.frame0.frameBUTTONS.ok configure -state normal
  }

  bind .frame0.frameMS.listbox <ButtonPress-1> {
        %W select set [%W nearest %y]
        set mapset [%W get [%W nearest %y]]
	.frame0.frameBUTTONS.ok configure -state normal
  }

  bind .frame0.frameNMS.mid.entry <KeyRelease> {
	.frame0.frameNMS.right.button configure -state active
  }
  
  grab .
  tkwait window . 

}

#############################################################################
#
# proc DialogGen {widget title bitmap text default buttons}
#
# PURPOSE:  	This function simply pops up a dialog box with a given message.
#   	    	Note that it is similar to tk_dialog but has a slightly
#   	    	different look to the dialog.
#   	    	Example call:
#   	    	    set val [DialogGen .warnDlg "WARNING: List Changed" \
#   	    	    	warning "WARNING: You have changed the current list.\
#   	    	    	Do you want to discard the changes and open a new \
#   	    	    	file?" 0 OK Cancel]
#    	    	    if { $val == 0 } { puts stderr "OK button pressed" }
#   	    	    if { $val == 1 } { puts stderr "Cancel button pressed" }
# INPUT VARS:	widget	=>  name of the dialog box starting with . eg .errDlg
#   	    	title	=>  title to display in window border
#   	    	bitmap	=>  bitmap icon to display - must be one of
#   	    	    	    	error	    	gray12
#   	    	    	    	gray50 	    	hourglass
#   	    	    	    	info   	    	questhead
#   	    	    	    	question    	warning
#   	    	text	=>  text of the message to be displayed
#   	    	default =>  index of default button (0, 1, 2...) must be less
#   	    	    	    than number of buttons
#   	    	buttons =>  text to be used for each button eg OK Cancel
# RETURN VAL:	index of button that was clicked - can be ignored if only one
#   	    	button is defined
#
#############################################################################

# Procedure to generate the dialog box
proc DialogGen {widget title bitmap text default buttons} \
{
    global buttonNum
    
    # Create a popup window to warn the user
    toplevel $widget
    wm title $widget $title
    wm resizable $widget 0 0
    wm protocol $widget WM_DELETE_WINDOW "CancelDialog $widget"

    # Create a label for the bitmap and a message for the text
    frame $widget.dlgFrame
    pack $widget.dlgFrame -side top -fill both
    label $widget.dlgFrame.icon -bitmap $bitmap
    message $widget.dlgFrame.text -text $text -width 10c
    pack $widget.dlgFrame.icon $widget.dlgFrame.text -side left -fill x \
	-padx 10
    
    # Create a frame for the pushbuttons
    frame $widget.sepFrame -height 4 -bd 2 -relief raised
    frame $widget.buttonFrame 
    pack $widget.buttonFrame $widget.sepFrame -side bottom -fill x

    # Create the pushbuttons
    set i 0
    foreach buttonLabel $buttons \
    {
	button $widget.buttonFrame.$i -text $buttonLabel \
	    -command "set buttonNum $i"
	pack $widget.buttonFrame.$i -side left -expand 1 -padx 10 -pady 5
	incr i
    }
    
    # Position the top left corner of the window over the root window
    wm withdraw $widget
    update idletasks
    wm geometry $widget +[expr [winfo rootx .] + ([winfo width .] \
	-[winfo width $widget]) / 2]+[expr [winfo rooty .] + ([winfo \
	height .] - [winfo height $widget]) / 2]
    wm deiconify $widget

    # Grab the pointer to make sure this window is closed before continuing
    grab set $widget

    if {$default >= 0} \
    {
	focus $widget.buttonFrame.$default
    }
    
    tkwait variable buttonNum
    
    # Destroy the popup window
    destroy $widget
    
    # Return the number of the button that was pushed
    return "$buttonNum"
}

# Procedure to cancel the dialog
proc CancelDialog {widget} \
{
    global buttonNum

    # Set the wait variable so that the dialog box can cancel properly
    set buttonNum 999
}

global database
global location
global mapset

global grassrc_list
global gisrc_name

set ver [info tclversion]

if { [string compare $ver "8.0"] < 0} \
{
    puts stderr "Sorry your version of the Tcl/Tk libraries is $ver and is too"
    puts stderr "old for GRASS which requires a Tcl/Tk library version of 8.0 or later."
    puts stderr "Reverting default settings back to GRASS text mode interface."
    exit 1
}

set database ""
set location ""
set mapset ""

set gisrc_name ""
if { [info exists env(GISRC)] } {
   set gisrc_name $env(GISRC)
}

if { [searchGISRC $gisrc_name] } {
   gisSetWindow
}







