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




proc gisSetWindow {} {

  # Window manager configurations

  wm geometry . +100+100
  wm title . {}

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

  label .frame0.frame1.label \
    -anchor {n} \
    -text "Database : "

  scrollbar .frame0.frame1.hscrollbar \
    -command { .frame0.frame1.entry view} \
    -relief {raised} \
    -orient {horizontal}
 
  entry .frame0.frame1.entry \
    -relief {sunken} \
    -xscrollcommand { .frame0.frame1.hscrollbar set}

  pack append .frame0.frame1 \
    .frame0.frame1.label { left frame n} \
    .frame0.frame1.hscrollbar { bottom fill } \
    .frame0.frame1.entry { top fill }

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

  .frame0.frame1.entry insert 0 [exec pwd]
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
  .frame0.frame3.listbox select set $curSelected


  bind .frame0.frame1.entry <Return> {
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
        %W select set [%W nearest %y]
        set mapset [%W get [%W nearest %y]]
  }

  bind .frame0.frame3.listbox <ButtonPress-1> {
        %W select set [%W nearest %y]
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
