#
# Program : gis_set.tcl
# 
#

proc searchGISRC { filename } {
 
  global database
  global location
  global mapset

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
            if { [scan $i "GISDBASE: %s" env_database] } {
                puts $ofp "GISDBASE: $database"
            } else {
                if { [scan $i "LOCATION_NAME: %s" env_location] } {
                   puts $ofp "LOCATION_NAME: $location"
                } else {
                   if { [scan $i "MAPSET: %s" env_mapset] } {
                      puts $ofp "MAPSET: $mapset"
                   } else {
                      puts $ofp $i
                   }
                }
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

proc gisSetWindow {} {

  # Window manager configurations

  wm geometry . +100+100
  wm title . "Grass 5.0 Data Selection"

  global database
  global location
  global mapset

  global grassrc_list
  global gisrc_name

  # ---------------------------
  # build .frame0
  # ---------------------------
  frame .frame0 \
    -borderwidth {2} \
    -relief {raised}


  # -----------------------------------
  # build .frame0.frame1
  # -----------------------------------

  frame .frame0.frame1 \
    -borderwidth {2}

  frame .frame0.frame1.left \
    -borderwidth {2}

  frame .frame0.frame1.mid \
    -borderwidth {2}

  frame .frame0.frame1.right \
    -borderwidth {2}

  label .frame0.frame1.left.label \
    -anchor {n} \
    -text "Database : "

  entry .frame0.frame1.mid.entry \
    -relief {sunken} \
    -textvariable database \
    -xscrollcommand { .frame0.frame1.mid.hscrollbar set}
    
  scrollbar .frame0.frame1.mid.hscrollbar \
    -command { .frame0.frame1.mid.entry xview} \
    -relief {raised} \
    -orient {horizontal}
 
  button .frame0.frame1.right.button \
    -text "Browse..." \
    -command {GetDir .frame0.frame1.mid.entry .frame0.frame2.listbox \
    	.frame0.frame3.listbox}

  pack .frame0.frame1.left.label -side top
  pack .frame0.frame1.mid.entry -side top -fill x
  pack .frame0.frame1.mid.hscrollbar -side bottom -fill x
  pack .frame0.frame1.right.button -side left -fill x
  pack .frame0.frame1.left -side left  -anchor n -fill x
  pack .frame0.frame1.mid -side left -fill x
  pack .frame0.frame1.right -side left -anchor n -fill x
  
  # -----------------------------------
  # build .frame0.frame2
  # -----------------------------------
  frame .frame0.frame2 \
    -borderwidth {2}

  label .frame0.frame2.label \
    -anchor {w} \
    -text "Location" 

  scrollbar .frame0.frame2.vscrollbar \
    -command {.frame0.frame2.listbox yview} \
    -relief {raised}

  scrollbar .frame0.frame2.hscrollbar \
    -command {.frame0.frame2.listbox xview} \
    -orient {horizontal} \
    -relief {raised}

  listbox .frame0.frame2.listbox \
    -relief {raised} \
    -exportselection false \
    -yscrollcommand {.frame0.frame2.vscrollbar set} \
    -xscrollcommand {.frame0.frame2.vscrollbar set}

  pack append .frame0.frame2 \
    .frame0.frame2.label { top fill } \
    .frame0.frame2.vscrollbar { right filly } \
    .frame0.frame2.hscrollbar { bottom fillx } \
    .frame0.frame2.listbox { left expand fill }


  # -----------------------------------
  # build .frame0.frame3
  # -----------------------------------
  frame .frame0.frame3 \
    -borderwidth {2}

  label .frame0.frame3.label \
    -anchor {w} \
    -text "Mapset" 

  scrollbar .frame0.frame3.vscrollbar \
    -command {.frame0.frame3.listbox yview} \
    -relief {raised}

  scrollbar .frame0.frame3.hscrollbar \
    -command {.frame0.frame3.listbox xview} \
    -orient {horizontal} \
    -relief {raised}

  listbox .frame0.frame3.listbox \
    -relief {raised} \
    -yscrollcommand {.frame0.frame3.vscrollbar set} \
    -xscrollcommand {.frame0.frame3.vscrollbar set}

  pack append .frame0.frame3 \
    .frame0.frame3.label { top fill } \
    .frame0.frame3.vscrollbar { right filly } \
    .frame0.frame3.hscrollbar { bottom fillx } \
    .frame0.frame3.listbox { left expand fill }


  # ----------------------------------
  # build .frame0.frame4
  # ----------------------------------
  frame .frame0.frame4 \
    -borderwidth {2}

  button .frame0.frame4.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command { 
                if { $mapset != "" } {
                   putGRASSRC $gisrc_name
                   puts stdout "GISDBASE='$database'; export GISDBASE;"
                   puts stdout "LOCATION_NAME='$location'; export LOCATION_NAME;"
                   puts stdout "MAPSET='$mapset'; export MAPSET;"
                   destroy .
                } 
              }

  button .frame0.frame4.newLoc \
    -text "New Location" \
    -relief raised \
    -padx 10 \
    -command {
                puts stdout "GISDBASE='$database'; export GISDBASE;"
    	    	puts stdout "LOCATION_NAME='##NONE##'; export LOCATION_NAME;"
                puts stdout "MAPSET=''; export MAPSET;"
                destroy . 
             }

  button .frame0.frame4.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command { 
               puts stdout "exit" 
               destroy . 
             }

  pack append .frame0.frame4 \
    .frame0.frame4.ok { left expand } \
    .frame0.frame4.newLoc {left expand } \
    .frame0.frame4.cancel { right expand }



  # ----------------------------------
  # packed it all
  # ----------------------------------

  # pack widget .frame0
  pack append .frame0 \
    .frame0.frame4 { bottom expand fill } \
    .frame0.frame1 { top expand fill } \
    .frame0.frame2 { left expand fill } \
    .frame0.frame3 { right expand fill }

  pack append . \
    .frame0 { top frame center expand fill }

  .frame0.frame1.mid.entry xview moveto 1

  foreach i [exec ls -a [exec pwd]] {
      if { [string compare $i "."] != 0 && \
           [string compare $i ".."] != 0 && \
           [file isdirectory $i] } {
           .frame0.frame2.listbox insert end $i
      }
  }
        
  set i 0
  set curSelected 0
  set length [.frame0.frame2.listbox size]
  while { $i <  $length } {
          if { $location == [.frame0.frame2.listbox get $i] } {
            set curSelected $i
            break
          }
          incr i 1
  }
  .frame0.frame2.listbox select set $curSelected


  cd $database
  cd $location
  foreach i [exec ls -a [exec pwd]] {
      if { [string compare $i "."] != 0 && \
           [string compare $i ".."] != 0 && \
           [file isdirectory $i] && [file owned $i] } {
           .frame0.frame3.listbox insert end $i
      }
  }

  set i 0
  set curSelected 0
  set length [.frame0.frame3.listbox size]
  while { $i <  $length } {
          if { $mapset == [.frame0.frame3.listbox get $i] } {
            set curSelected $i
            break
          }
          incr i 1
  }
  .frame0.frame3.listbox yview $curSelected
  .frame0.frame3.listbox select set $curSelected


  bind .frame0.frame1.mid.entry <Return> {
        set new_path [%W get]
        if { "$new_path" != "" \
             && [file exists $new_path] && [file isdirectory $new_path] } {
           %W delete 0 end
           %W insert 0 $new_path
           cd $new_path
           .frame0.frame2.listbox delete 0 end
           foreach i [exec ls -a [exec pwd]] {
               if { [string compare $i "."] != 0 && \
                    [string compare $i ".."] != 0 && \
                    [file isdirectory $i] } {
                   .frame0.frame2.listbox insert end $i
               }
           }
           .frame0.frame3.listbox delete 0 end
           set database [exec pwd]
        }
  }

  bind .frame0.frame2.listbox <Double-ButtonPress-1> {
        %W select set [%W nearest %y]
        cd $database
        set location [%W get [%W nearest %y]]
        cd $location
        .frame0.frame3.listbox delete 0 end
        foreach i [exec ls -a [exec pwd]] {
           if { [string compare $i "."] != 0 && \
                [string compare $i ".."] != 0 && \
                [file isdirectory $i] && [file owned $i] } { 
                .frame0.frame3.listbox insert end $i
           }
        }
        set mapset ""
  }

  bind .frame0.frame2.listbox <ButtonPress-1> {
        %W select set [%W nearest %y]
        cd $database
        set location [%W get [%W nearest %y]]
        cd $location
        .frame0.frame3.listbox delete 0 end
        foreach i [exec ls -a [exec pwd]] {
           if { [string compare $i "."] != 0 && \
                [string compare $i ".."] != 0 && \
                [file isdirectory $i] && [file owned $i] } {
                .frame0.frame3.listbox insert end $i
           }
        }
        set mapset ""
  }

  bind .frame0.frame3.listbox <Double-ButtonPress-1> {
        %W select set [%W nearest %y]
        set mapset [%W get [%W nearest %y]]
  }

  bind .frame0.frame3.listbox <ButtonPress-1> {
        %W select set [%W nearest %y]
        set mapset [%W get [%W nearest %y]]
  }

  grab .
  tkwait window . 

}

global database
global location
global mapset

global grassrc_list
global gisrc_name

set database ""
set location ""
set mapset ""

set gisrc_name ""
if { [info exists env(GISRC)] } {
   set gisrc_name $env(GISRC)
}

if { [searchGISRC $gisrc_name] } {
   cd $database
   gisSetWindow
}
