

proc get_feature { type } {

  set dir ""
  case $type in {

       {raster} {
                      set dir "cell"
                  }

       {vector} {
                      set dir "dig"
                  }

       {sites} {
                      set dir "site_lists"
                  }

       {label} {
                      set dir "paint/labels"
                  }

       {group} {
                      set dir "group"
                  }

       {icon} {
                      set dir "paint/icons"
                  }

       {region} {
                      set dir "windows"
                  }

       {dlg} {
                      set dir "dlg"
                  }

       {dlg_ascii} {
                      set dir "dlg_ascii"
                  }

  }
  return $dir

}




proc put_list { the_listbox the_list } {

  $the_listbox delete 0 end
  foreach i $the_list {
      $the_listbox insert end $i
  }
}




proc get_list { path } {
  set list ""
  if { [file isdirectory $path] != 1 } {
     return $list
  }
  set current_dir [exec pwd]
  cd $path
  foreach i [exec ls -a [exec pwd]] {
     if { [string compare $i "."] != 0 && [string compare $i ".."] != 0 } {
        lappend list $i
     }
  }
  cd $current_dir
  return $list
}




proc mapset_listbox { type } {

  global database
  global location
  global mapset
  global feature

  global file_name
  set file_name ""

  toplevel .mapset

  wm geometry .mapset +100+100
  wm title .mapset {spatial layer}

  set feature [get_feature $type]

  global mapset_list

  frame .mapset.frame0 \
    -borderwidth {2} \
    -relief {flat}

  label .mapset.frame0.label \
    -anchor {w} \
    -text "Mapset"

  entry .mapset.frame0.mapset \
    -relief {sunken} \
   -width 20

  menubutton .mapset.frame0.mapsets \
    -bitmap {@../bitmap/arrow} \
    -menu {.mapset.frame0.mapsets.pulldown}

  menu .mapset.frame0.mapsets.pulldown

  set mapset_list [get_list "$database/$location"]
  foreach i $mapset_list {
      .mapset.frame0.mapsets.pulldown add command \
          -label $i \
          -command {
                     set mapset [lindex $mapset_list \
                               [.mapset.frame0.mapsets.pulldown index active] ]
                     .mapset.frame0.mapset delete 0 end
                     .mapset.frame0.mapset insert 0 $mapset
                     put_list .mapset.frame1.listbox \
                         [get_list "$database/$location/$mapset/$feature"]
                     set file_name ""
                   }
  }

  pack append .mapset.frame0 \
    .mapset.frame0.label { left } \
    .mapset.frame0.mapset { left } \
    .mapset.frame0.mapsets { right }

  frame .mapset.frame1 \
    -borderwidth {2} \
    -relief {raised}

  listbox .mapset.frame1.listbox \
    -relief {sunken} \
    -geometry 20x10 \
    -yscrollcommand {.mapset.frame1.vscrollbar set}

  scrollbar .mapset.frame1.vscrollbar \
    -command {.mapset.frame1.listbox yview}

  pack append .mapset.frame1 \
    .mapset.frame1.listbox { left expand fill } \
    .mapset.frame1.vscrollbar { right fill }

  frame .mapset.frame2 \
    -borderwidth {2}

  frame .mapset.frame2.frame

  button .mapset.frame2.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command { if { $file_name != "" } {
                   destroy .mapset
               }
              }

  button .mapset.frame2.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command { set file_name ""
               destroy .mapset
             }

  pack append .mapset.frame2.frame \
    .mapset.frame2.frame.ok { left expand } \
    .mapset.frame2.frame.cancel { right expand }

  pack append .mapset.frame2 \
    .mapset.frame2.frame { bottom frame center fill }

  pack append .mapset \
    .mapset.frame0 { top expand fill } \
    .mapset.frame1 { top expand fill } \
    .mapset.frame2 { bottom expand fill }

  bind .mapset.frame1.listbox <Button-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	    set file_name [%W get [%W nearest %y]]
  }

  bind .mapset.frame1.listbox <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	    set file_name [%W get [%W nearest %y]]
  }

  bind .mapset.frame1.listbox <Double-ButtonPress-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	    set file_name [%W get [%W nearest %y]]
  }

  .mapset.frame0.mapset delete 0 end
  .mapset.frame0.mapset insert 0 $mapset

  .mapset.frame1.listbox delete 0 end
  put_list .mapset.frame1.listbox \
             [get_list "$database/$location/$mapset/$feature"]

  grab set .mapset
  tkwait window .mapset

  return $file_name

}




proc corner { } {


  toplevel .corner

  wm geometry .corner +100+20
  wm title .corner "corner"


  global corner


  frame .corner.frame0 \
    -relief {flat}

  label .corner.frame0.label \
     -anchor {w} \
     -text {Corner of the input:} \
     -padx {2}

  listbox .corner.frame0.listbox \
     -relief sunken \
     -yscrollcommand {.corner.frame0.vscrollbar set}

  scrollbar .corner.frame0.vscrollbar \
     -command {.corner.frame0.listbox yview}


  pack append .corner.frame0 \
    .corner.frame0.label { top fillx } \
    .corner.frame0.listbox { left expand fill } \
    .corner.frame0.vscrollbar { right fill }

  bind .corner.frame0.listbox <Button-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set corner [%W get [%W nearest %y]]
  }

  bind .corner.frame0.listbox <ButtonRelease-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set corner [%W get [%W nearest %y]]
  }

  bind .corner.frame0.listbox <Double-ButtonPress-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set corner [%W get [%W nearest %y]]
  }

  .corner.frame0.listbox delete 0 end
  foreach i {ne se sw nw} {
      .corner.frame0.listbox insert 0 $i
  }


  frame .corner.frame1 \
     -borderwidth {2}

  button .corner.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .corner }

  button .corner.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .corner }

  pack append .corner.frame1 \
    .corner.frame1.ok { left expand } \
    .corner.frame1.cancel { right expand }



  pack append .corner \
    .corner.frame0 { top expand fill } \
    .corner.frame1 { bottom expand fill }



  grab set .corner
  tkwait window .corner

}




proc latitude { } {


  toplevel .latitude

  wm geometry .latitude +100+20
  wm title .latitude "latitude"


  global ldd


  frame .latitude.frame0 \
    -relief {flat}

  scale .latitude.frame0.scale \
     -label "Latitude Degrees" \
     -from 0 \
     -to 90 \
     -length 350 \
     -orient {horizontal}

  pack append .latitude.frame0 \
    .latitude.frame0.scale { left expand fill }

  .latitude.frame0.scale set 0

  global lmm


  frame .latitude.frame1 \
    -relief {flat}

  scale .latitude.frame1.scale \
     -label "Latitude Minutes" \
     -from 0 \
     -to 60 \
     -length 350 \
     -orient {horizontal}

  pack append .latitude.frame1 \
    .latitude.frame1.scale { left expand fill }

  .latitude.frame1.scale set 0

  global lss


  frame .latitude.frame2 \
    -relief {flat}

  scale .latitude.frame2.scale \
     -label "Latitude Seconds" \
     -from 0 \
     -to 60 \
     -length 350 \
     -orient {horizontal}

  pack append .latitude.frame2 \
    .latitude.frame2.scale { left expand fill }

  .latitude.frame2.scale set 0

  global ns


  frame .latitude.frame3 \
    -relief {flat}

  label .latitude.frame3.label \
    -anchor {w} \
    -text {North/South} \
    -padx {2}

  scrollbar .latitude.frame3.hscrollbar \
    -command {.latitude.frame3.entry view} \
    -orient {horizontal}

  entry .latitude.frame3.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.latitude.frame3.hscrollbar set}

  pack append .latitude.frame3 \
    .latitude.frame3.label { top fillx } \
    .latitude.frame3.entry { top fillx } \
    .latitude.frame3.hscrollbar { bottom fillx }

  .latitude.frame3.entry configure -state normal

  bind .latitude.frame3.entry <Return> {
       set ns [%W get] }



  frame .latitude.frame4 \
     -borderwidth {2}

  button .latitude.frame4.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set ldd [.latitude.frame0.scale get]
                  set lmm [.latitude.frame1.scale get]
                  set lss [.latitude.frame2.scale get]
                  set ns [.latitude.frame3.entry get]
                 destroy .latitude }

  button .latitude.frame4.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set ldd ""
                  set lmm ""
                  set lss ""
                  set ns ""
                 destroy .latitude }

  pack append .latitude.frame4 \
    .latitude.frame4.ok { left expand } \
    .latitude.frame4.cancel { right expand }


bind .latitude.frame0.scale <Return> {
     focus .latitude.frame1.scale
}

bind .latitude.frame1.scale <Return> {
     focus .latitude.frame2.scale
}

bind .latitude.frame2.scale <Return> {
     focus .latitude.frame3.entry
}

bind .latitude.frame3.entry <Return> {
     focus .latitude.frame0.scale
}


  pack append .latitude \
    .latitude.frame0 { top expand fill } \
    .latitude.frame1 { top expand fill } \
    .latitude.frame2 { top expand fill } \
    .latitude.frame3 { top expand fill } \
    .latitude.frame4 { bottom expand fill }



  grab set .latitude
  tkwait window .latitude

}




proc longitude { } {


  toplevel .longitude

  wm geometry .longitude +100+20
  wm title .longitude "longitude"


  global mdd


  frame .longitude.frame0 \
    -relief {flat}

  scale .longitude.frame0.scale \
     -label "Longitude Degrees" \
     -from 0 \
     -to 90 \
     -length 350 \
     -orient {horizontal}

  pack append .longitude.frame0 \
    .longitude.frame0.scale { left expand fill }

  .longitude.frame0.scale set 0

  global mmm


  frame .longitude.frame1 \
    -relief {flat}

  scale .longitude.frame1.scale \
     -label "Longitude Minutes" \
     -from 0 \
     -to 60 \
     -length 350 \
     -orient {horizontal}

  pack append .longitude.frame1 \
    .longitude.frame1.scale { left expand fill }

  .longitude.frame1.scale set 0

  global mss


  frame .longitude.frame2 \
    -relief {flat}

  scale .longitude.frame2.scale \
     -label "Longitude Seconds" \
     -from 0 \
     -to 60 \
     -length 350 \
     -orient {horizontal}

  pack append .longitude.frame2 \
    .longitude.frame2.scale { left expand fill }

  .longitude.frame2.scale set 0

  global ew


  frame .longitude.frame3 \
    -relief {flat}

  label .longitude.frame3.label \
    -anchor {w} \
    -text {East/West} \
    -padx {2}

  scrollbar .longitude.frame3.hscrollbar \
    -command {.longitude.frame3.entry view} \
    -orient {horizontal}

  entry .longitude.frame3.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.longitude.frame3.hscrollbar set}

  pack append .longitude.frame3 \
    .longitude.frame3.label { top fillx } \
    .longitude.frame3.entry { top fillx } \
    .longitude.frame3.hscrollbar { bottom fillx }

  .longitude.frame3.entry configure -state normal

  bind .longitude.frame3.entry <Return> {
       set ew [%W get] }



  frame .longitude.frame4 \
     -borderwidth {2}

  button .longitude.frame4.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set mdd [.longitude.frame0.scale get]
                  set mmm [.longitude.frame1.scale get]
                  set mss [.longitude.frame2.scale get]
                  set ew [.longitude.frame3.entry get]
                 destroy .longitude }

  button .longitude.frame4.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set mdd ""
                  set mmm ""
                  set mss ""
                  set ew ""
                 destroy .longitude }

  pack append .longitude.frame4 \
    .longitude.frame4.ok { left expand } \
    .longitude.frame4.cancel { right expand }


bind .longitude.frame0.scale <Return> {
     focus .longitude.frame1.scale
}

bind .longitude.frame1.scale <Return> {
     focus .longitude.frame2.scale
}

bind .longitude.frame2.scale <Return> {
     focus .longitude.frame3.entry
}

bind .longitude.frame3.entry <Return> {
     focus .longitude.frame0.scale
}


  pack append .longitude \
    .longitude.frame0 { top expand fill } \
    .longitude.frame1 { top expand fill } \
    .longitude.frame2 { top expand fill } \
    .longitude.frame3 { top expand fill } \
    .longitude.frame4 { bottom expand fill }



  grab set .longitude
  tkwait window .longitude

}




proc rows { } {


  toplevel .rows

  wm geometry .rows +100+20
  wm title .rows "rows"


  global rows


  frame .rows.frame0 \
    -relief {flat}

  label .rows.frame0.label \
    -anchor {w} \
    -text {Number of rows in the input file:} \
    -padx {2}

  scrollbar .rows.frame0.hscrollbar \
    -command {.rows.frame0.entry view} \
    -orient {horizontal}

  entry .rows.frame0.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.rows.frame0.hscrollbar set}

  pack append .rows.frame0 \
    .rows.frame0.label { top fillx } \
    .rows.frame0.entry { top fillx } \
    .rows.frame0.hscrollbar { bottom fillx }

  .rows.frame0.entry configure -state normal

  bind .rows.frame0.entry <Return> {
       set rows [%W get] }



  frame .rows.frame1 \
     -borderwidth {2}

  button .rows.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set rows [.rows.frame0.entry get]
                 destroy .rows }

  button .rows.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set rows ""
                 destroy .rows }

  pack append .rows.frame1 \
    .rows.frame1.ok { left expand } \
    .rows.frame1.cancel { right expand }



  pack append .rows \
    .rows.frame0 { top expand fill } \
    .rows.frame1 { bottom expand fill }



  grab set .rows
  tkwait window .rows

}




proc columns { } {


  toplevel .columns

  wm geometry .columns +100+20
  wm title .columns "columns"


  global cols


  frame .columns.frame0 \
    -relief {flat}

  label .columns.frame0.label \
    -anchor {w} \
    -text {Number of columns in the input file:} \
    -padx {2}

  scrollbar .columns.frame0.hscrollbar \
    -command {.columns.frame0.entry view} \
    -orient {horizontal}

  entry .columns.frame0.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.columns.frame0.hscrollbar set}

  pack append .columns.frame0 \
    .columns.frame0.label { top fillx } \
    .columns.frame0.entry { top fillx } \
    .columns.frame0.hscrollbar { bottom fillx }

  .columns.frame0.entry configure -state normal

  bind .columns.frame0.entry <Return> {
       set cols [%W get] }



  frame .columns.frame1 \
     -borderwidth {2}

  button .columns.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set cols [.columns.frame0.entry get]
                 destroy .columns }

  button .columns.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set cols ""
                 destroy .columns }

  pack append .columns.frame1 \
    .columns.frame1.ok { left expand } \
    .columns.frame1.cancel { right expand }



  pack append .columns \
    .columns.frame0 { top expand fill } \
    .columns.frame1 { bottom expand fill }



  grab set .columns
  tkwait window .columns

}




proc resolution { } {


  toplevel .resolution

  wm geometry .resolution +100+20
  wm title .resolution "resolution"


  global latres


  frame .resolution.frame0 \
    -relief {flat}

  scale .resolution.frame0.scale \
     -label "Latitude resolution:" \
     -from 0 \
     -to 10 \
     -length 350 \
     -orient {horizontal}

  pack append .resolution.frame0 \
    .resolution.frame0.scale { left expand fill }

  .resolution.frame0.scale set 5

  global lonres


  frame .resolution.frame1 \
    -relief {flat}

  scale .resolution.frame1.scale \
     -label "Longitude resolution:" \
     -from 0 \
     -to 10 \
     -length 350 \
     -orient {horizontal}

  pack append .resolution.frame1 \
    .resolution.frame1.scale { left expand fill }

  .resolution.frame1.scale set 5


  frame .resolution.frame2 \
     -borderwidth {2}

  button .resolution.frame2.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set latres [.resolution.frame0.scale get]
                  set lonres [.resolution.frame1.scale get]
                 destroy .resolution }

  button .resolution.frame2.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set latres ""
                  set lonres ""
                 destroy .resolution }

  pack append .resolution.frame2 \
    .resolution.frame2.ok { left expand } \
    .resolution.frame2.cancel { right expand }


bind .resolution.frame0.scale <Return> {
     focus .resolution.frame1.scale
}

bind .resolution.frame1.scale <Return> {
     focus .resolution.frame0.scale
}


  pack append .resolution \
    .resolution.frame0 { top expand fill } \
    .resolution.frame1 { top expand fill } \
    .resolution.frame2 { bottom expand fill }



  grab set .resolution
  tkwait window .resolution

}




proc spheroid { } {


  toplevel .spheroid

  wm geometry .spheroid +100+20
  wm title .spheroid "spheroid"


  global sname


  frame .spheroid.frame0 \
    -relief {flat}

  label .spheroid.frame0.label \
     -anchor {w} \
     -text {Spheroid:} \
     -padx {2}

  listbox .spheroid.frame0.listbox \
     -relief sunken \
     -yscrollcommand {.spheroid.frame0.vscrollbar set}

  scrollbar .spheroid.frame0.vscrollbar \
     -command {.spheroid.frame0.listbox yview}


  pack append .spheroid.frame0 \
    .spheroid.frame0.label { top fillx } \
    .spheroid.frame0.listbox { left expand fill } \
    .spheroid.frame0.vscrollbar { right fill }

  bind .spheroid.frame0.listbox <Button-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set sname [%W get [%W nearest %y]]
  }

  bind .spheroid.frame0.listbox <ButtonRelease-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set sname [%W get [%W nearest %y]]
  }

  bind .spheroid.frame0.listbox <Double-ButtonPress-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set sname [%W get [%W nearest %y]]
  }

  .spheroid.frame0.listbox delete 0 end
  foreach i {airy australian bessel clark66 everest grs80 hayford international krasovsky wgs66 wgs72 wgs84} {
      .spheroid.frame0.listbox insert 0 $i
  }


  frame .spheroid.frame1 \
     -borderwidth {2}

  button .spheroid.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .spheroid }

  button .spheroid.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .spheroid }

  pack append .spheroid.frame1 \
    .spheroid.frame1.ok { left expand } \
    .spheroid.frame1.cancel { right expand }



  pack append .spheroid \
    .spheroid.frame0 { top expand fill } \
    .spheroid.frame1 { bottom expand fill }



  grab set .spheroid
  tkwait window .spheroid

}




proc flag { } {


  toplevel .flag

  wm geometry .flag +100+20
  wm title .flag "flag"


  global s


  frame .flag.frame0 \
    -relief {flat}

  checkbutton .flag.frame0.checkbutton \
     -text "Signed data (high bit means negative value)." \
     -relief flat \
     -anchor {w} \
     -onvalue "s" \
     -offvalue "" \
     -variable s

  pack append .flag.frame0 \
    .flag.frame0.checkbutton { left }


  frame .flag.frame1 \
     -borderwidth {2}

  button .flag.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .flag }

  button .flag.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .flag }

  pack append .flag.frame1 \
    .flag.frame1.ok { left expand } \
    .flag.frame1.cancel { right expand }



  pack append .flag \
    .flag.frame0 { top expand fill } \
    .flag.frame1 { bottom expand fill }



  grab set .flag
  tkwait window .flag

}




proc put_command { } {


  global s
  global iname
  global oname
  global value
  global corner
  global ldd
  global lmm
  global lss
  global ns
  global mdd
  global mmm
  global mss
  global ew
  global rows
  global cols
  global latres
  global lonres
  global sname


  set cmd ""

  if {  $s != "" } {
         set cmd "$cmd -$s"
  }

  if {  $iname != "" } {
         set cmd "$cmd input=$iname"
  }

  if {  $oname != "" } {
         set cmd "$cmd output=$oname"
  }

  if {  $value != "" } {
         set cmd "$cmd bpc=$value"
  }

  if {  $corner != "" || $ldd != "" || $lmm != "" || $lss != "" || $ns != "" || $mdd != "" || $mmm != "" || $mss != "" || $ew != "" } {
         set cmd "$cmd corner=$corner,$ldd,$lmm,$lss,$ns,$mdd,$mmm,$mss,$ew"
  }

  if {  $rows != "" || $cols != "" } {
         set cmd "$cmd dimension=$rows,$cols"
  }

  if {  $latres != "" || $lonres != "" } {
         set cmd "$cmd res=$latres,$lonres"
  }

  if {  $sname != "" } {
         set cmd "$cmd spheroid=$sname"
  }

  if { $cmd != "" } {
     set cmd "r.in.ll $cmd"
  }
}




proc set_command_entry { } {

  .cmd.frame0.entry configure -state normal
  .cmd.frame0.entry delete 0 end
  .cmd.frame0.entry insert 0 [put_command]
  .cmd.frame0.entry configure -state disabled

}


proc proc_r.in.ll { } {


  global s
  global iname
  global oname
  global value
  global corner
  global ldd
  global lmm
  global lss
  global ns
  global mdd
  global mmm
  global mss
  global ew
  global rows
  global cols
  global latres
  global lonres
  global sname


  toplevel .cmd

  wm geometry .cmd +100+20

  wm title .cmd "Convert Raster Data to UTM-referenced Map"


  frame .cmd.frame0 \
    -relief {flat}

  label .cmd.frame0.label \
    -anchor {w} \
    -text {Command:} \
    -padx {2}

  scrollbar .cmd.frame0.hscrollbar \
    -command {.cmd.frame0.entry view} \
    -orient {horizontal}

  entry .cmd.frame0.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.cmd.frame0.hscrollbar set}

  pack append .cmd.frame0 \
    .cmd.frame0.label { top fillx } \
    .cmd.frame0.entry { top fillx } \
    .cmd.frame0.hscrollbar { bottom fillx }

  .cmd.frame0.entry delete 0 end
  .cmd.frame0.entry insert 0 {r.in.ll}

  .cmd.frame0.entry configure -state disabled



  frame .cmd.frame1 \
    -relief {flat}

  label .cmd.frame1.label \
    -anchor {w} \
    -text {Description:} \
    -padx {2}

  scrollbar .cmd.frame1.hscrollbar \
    -command {.cmd.frame1.entry view} \
    -orient {horizontal}

  entry .cmd.frame1.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.cmd.frame1.hscrollbar set}

  pack append .cmd.frame1 \
    .cmd.frame1.label { top fillx } \
    .cmd.frame1.entry { top fillx } \
    .cmd.frame1.hscrollbar { bottom fillx }

  .cmd.frame1.entry delete 0 end
  .cmd.frame1.entry insert 0 {r.in.ll converts raster data referenced using latitude and longitude coordinates to a UTM-referenced map layer in GRASS raster format.}

  .cmd.frame1.entry configure -state disabled



  frame .cmd.frame2 \
    -relief {flat}

  label .cmd.frame2.label \
    -anchor {w} \
    -text {Name of existing raster map layer:} \
    -padx {2}

  scrollbar .cmd.frame2.hscrollbar \
    -command {.cmd.frame2.entry view} \
    -orient {horizontal}

  entry .cmd.frame2.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.cmd.frame2.hscrollbar set}

  button .cmd.frame2.button \
    -relief {raised} \
    -anchor {n} \
    -text {raster} \
    -command { set file [mapset_listbox raster]
                if { $file != "" } {
                   set iname $file
                   .cmd.frame2.entry delete 0 end
                   .cmd.frame2.entry insert 0 $file
                   set_command_entry
                }
             }


  pack append .cmd.frame2 \
    .cmd.frame2.label { top fillx } \
    .cmd.frame2.button { right frame n } \
    .cmd.frame2.entry { top fill } \
    .cmd.frame2.hscrollbar { top fillx }



  bind .cmd.frame2.entry <Return> {
       set iname [%W get] }


  frame .cmd.frame3 \
    -relief {flat}

  label .cmd.frame3.label \
    -anchor {w} \
    -text {Name of output raster map layer:} \
    -padx {2}

  scrollbar .cmd.frame3.hscrollbar \
    -command {.cmd.frame3.entry view} \
    -orient {horizontal}

  entry .cmd.frame3.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.cmd.frame3.hscrollbar set}

  pack append .cmd.frame3 \
    .cmd.frame3.label { top fillx } \
    .cmd.frame3.entry { top fillx } \
    .cmd.frame3.hscrollbar { bottom fillx }

  .cmd.frame3.entry configure -state normal

  bind .cmd.frame3.entry <Return> {
       set oname [%W get] }



  frame .cmd.frame4 \
    -relief {flat}

  label .cmd.frame4.label \
    -anchor {w} \
    -text {Number of bytes per cell:} \
    -padx {2}

  scrollbar .cmd.frame4.hscrollbar \
    -command {.cmd.frame4.entry view} \
    -orient {horizontal}

  entry .cmd.frame4.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.cmd.frame4.hscrollbar set}

  pack append .cmd.frame4 \
    .cmd.frame4.label { top fillx } \
    .cmd.frame4.entry { top fillx } \
    .cmd.frame4.hscrollbar { bottom fillx }

  .cmd.frame4.entry configure -state normal

  bind .cmd.frame4.entry <Return> {
       set value [%W get] }



  frame .cmd.frame5 \
    -relief {flat}

  label .cmd.frame5.label \
    -anchor {w} \
    -text {Other options available:} \
    -padx {2}

  button .cmd.frame5.button0 \
     -text "corner ..." \
     -relief raised \
     -padx 10 \
     -command "corner
                set_command_entry"


  button .cmd.frame5.button1 \
     -text "latitude ..." \
     -relief raised \
     -padx 10 \
     -command "latitude
                set_command_entry"


  button .cmd.frame5.button2 \
     -text "longitude ..." \
     -relief raised \
     -padx 10 \
     -command "longitude
                set_command_entry"


  button .cmd.frame5.button3 \
     -text "rows ..." \
     -relief raised \
     -padx 10 \
     -command "rows
                set_command_entry"


  button .cmd.frame5.button4 \
     -text "columns ..." \
     -relief raised \
     -padx 10 \
     -command "columns
                set_command_entry"


  button .cmd.frame5.button5 \
     -text "resolution ..." \
     -relief raised \
     -padx 10 \
     -command "resolution
                set_command_entry"


  button .cmd.frame5.button6 \
     -text "spheroid ..." \
     -relief raised \
     -padx 10 \
     -command "spheroid
                set_command_entry"


  button .cmd.frame5.button7 \
     -text "flag ..." \
     -relief raised \
     -padx 10 \
     -command "flag
                set_command_entry"


  frame .cmd.frame6 \
     -borderwidth {2}

  button .cmd.frame6.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command { 
		if {  $iname != "" && $oname != "" && $value != "" && $corner != "" && $ldd != "" && $lmm != "" && $lss != "" && $ns != "" && $mdd != "" && $mmm != "" && $mss != "" && $ew != "" && $rows != "" && $cols != "" && $latres != "" && $lonres != "" && $sname != "" } {
 		set cmd [put_command]
 		if { $cmd != "" } {
 		   eval " exec xterm -title r.in.ll -geometry 50x5 -exec $cmd " 
 		   destroy .cmd
 		}
 	}
 }

  button .cmd.frame6.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command { destroy .cmd }

  pack append .cmd.frame6 \
    .cmd.frame6.ok { left expand } \
    .cmd.frame6.cancel { right expand }

  pack append .cmd.frame5 \
    .cmd.frame5.label { top fillx } \
    .cmd.frame5.button0 { left } \
    .cmd.frame5.button1 { left } \
    .cmd.frame5.button2 { left } \
    .cmd.frame5.button3 { left } \
    .cmd.frame5.button4 { left } \
    .cmd.frame5.button5 { left } \
    .cmd.frame5.button6 { left } \
    .cmd.frame5.button7  { left }



  pack append .cmd \
    .cmd.frame1 { top expand fill } \
    .cmd.frame2 { top expand fill } \
    .cmd.frame3 { top expand fill } \
    .cmd.frame4 { top expand fill } \
    .cmd.frame5 { top expand fill } \
    .cmd.frame0 { top expand fill } \
    .cmd.frame6 { bottom expand fill }


bind .cmd.frame3.entry <KeyRelease> {
		set oname [.cmd.frame3.entry get]
		set_command_entry
}

bind .cmd.frame3.entry <Return> {
		set oname [.cmd.frame3.entry get]
		set_command_entry
}

bind .cmd.frame4.entry <KeyRelease> {
		set value [.cmd.frame4.entry get]
		set_command_entry
}

bind .cmd.frame4.entry <Return> {
		set value [.cmd.frame4.entry get]
		set_command_entry
}

  grab set .cmd
  tkwait window .cmd

}

global s
set s ""

global iname
set iname ""

global oname
set oname ""

global value
set value ""

global corner
set corner ""

global ldd
set ldd ""

global lmm
set lmm ""

global lss
set lss ""

global ns
set ns ""

global mdd
set mdd ""

global mmm
set mmm ""

global mss
set mss ""

global ew
set ew ""

global rows
set rows ""

global cols
set cols ""

global latres
set latres ""

global lonres
set lonres ""

global sname
set sname ""


global database
global location
global mapset
global feature
if { [info exists env(GISDBASE)] == 0 ||
     [info exists env(LOCATION_NAME)] == 0 ||
     [info exists env(MAPSET)] == 0 } {
   puts stdout "GISDBASE, LOCATION_NAME and MAPSET must be set !!!"
   return
}
set database $env(GISDBASE)
set location $env(LOCATION_NAME)
set mapset $env(MAPSET)
set feature ""

proc_r.in.ll
