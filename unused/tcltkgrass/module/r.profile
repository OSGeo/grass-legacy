

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




proc result { } {


  toplevel .result

  wm geometry .result +100+20
  wm title .result "result"


  global rtype


  frame .result.frame0 \
    -relief {flat}

  label .result.frame0.label \
     -anchor {w} \
     -text {Type of result to be output:} \
     -padx {2}

  listbox .result.frame0.listbox \
     -relief sunken \
     -yscrollcommand {.result.frame0.vscrollbar set}

  scrollbar .result.frame0.vscrollbar \
     -command {.result.frame0.listbox yview}


  pack append .result.frame0 \
    .result.frame0.label { top fillx } \
    .result.frame0.listbox { left expand fill } \
    .result.frame0.vscrollbar { right fill }

  bind .result.frame0.listbox <Button-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set rtype [%W get [%W nearest %y]]
  }

  bind .result.frame0.listbox <ButtonRelease-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set rtype [%W get [%W nearest %y]]
  }

  bind .result.frame0.listbox <Double-ButtonPress-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set rtype [%W get [%W nearest %y]]
  }

  .result.frame0.listbox delete 0 end
  foreach i {raw median average} {
      .result.frame0.listbox insert 0 $i
  }


  frame .result.frame1 \
     -borderwidth {2}

  button .result.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .result }

  button .result.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .result }

  pack append .result.frame1 \
    .result.frame1.ok { left expand } \
    .result.frame1.cancel { right expand }



  pack append .result \
    .result.frame0 { top expand fill } \
    .result.frame1 { bottom expand fill }



  grab set .result
  tkwait window .result

}




proc width { } {


  toplevel .width

  wm geometry .width +100+20
  wm title .width "width"


  global value


  frame .width.frame0 \
    -relief {flat}

  label .width.frame0.label \
     -anchor {w} \
     -text {Profile width in cells:} \
     -padx {2}

  listbox .width.frame0.listbox \
     -relief sunken \
     -yscrollcommand {.width.frame0.vscrollbar set}

  scrollbar .width.frame0.vscrollbar \
     -command {.width.frame0.listbox yview}


  pack append .width.frame0 \
    .width.frame0.label { top fillx } \
    .width.frame0.listbox { left expand fill } \
    .width.frame0.vscrollbar { right fill }

  bind .width.frame0.listbox <Button-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set value [%W get [%W nearest %y]]
  }

  bind .width.frame0.listbox <ButtonRelease-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set value [%W get [%W nearest %y]]
  }

  bind .width.frame0.listbox <Double-ButtonPress-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set value [%W get [%W nearest %y]]
  }

  .width.frame0.listbox delete 0 end
  foreach i {1 3 5 7 9 11 13 15} {
      .width.frame0.listbox insert 0 $i
  }


  frame .width.frame1 \
     -borderwidth {2}

  button .width.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .width }

  button .width.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                 destroy .width }

  pack append .width.frame1 \
    .width.frame1.ok { left expand } \
    .width.frame1.cancel { right expand }



  pack append .width \
    .width.frame0 { top expand fill } \
    .width.frame1 { bottom expand fill }



  grab set .width
  tkwait window .width

}




proc put_command { } {


  global name
  global rtype
  global value
  global line


  set cmd ""

  if {  $name != "" } {
         set cmd "$cmd map=$name"
  }

  if {  $rtype != "" } {
         set cmd "$cmd result=$rtype"
  }

  if {  $value != "" } {
         set cmd "$cmd width=$value"
  }

  if {  $line != "" } {
         set cmd "$cmd line=$line"
  }

  if { $cmd != "" } {
     set cmd "r.profile $cmd"
  }
}




proc set_command_entry { } {

  .cmd.frame0.entry configure -state normal
  .cmd.frame0.entry delete 0 end
  .cmd.frame0.entry insert 0 [put_command]
  .cmd.frame0.entry configure -state disabled

}


proc proc_r.profile { } {


  global name
  global rtype
  global value
  global line


  toplevel .cmd

  wm geometry .cmd +100+20

  wm title .cmd "Output Values Lying on User-Defined Lines"


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
  .cmd.frame0.entry insert 0 {r.profile}

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
  .cmd.frame1.entry insert 0 {r.profile outputs raster map layer values lying on user-defined lines.}

  .cmd.frame1.entry configure -state disabled



  frame .cmd.frame2 \
    -relief {flat}

  label .cmd.frame2.label \
    -anchor {w} \
    -text {Raster map to be queried:} \
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
                   set name $file
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
       set name [%W get] }


  frame .cmd.frame3 \
    -relief {flat}

  label .cmd.frame3.label \
    -anchor {w} \
    -text {Geographic coordinates:} \
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
       set line [%W get] }



  frame .cmd.frame4 \
    -relief {flat}

  label .cmd.frame4.label \
    -anchor {w} \
    -text {Other options available:} \
    -padx {2}

  button .cmd.frame4.button0 \
     -text "result ..." \
     -relief raised \
     -padx 10 \
     -command "result
                set_command_entry"


  button .cmd.frame4.button1 \
     -text "width ..." \
     -relief raised \
     -padx 10 \
     -command "width
                set_command_entry"


  frame .cmd.frame5 \
     -borderwidth {2}

  button .cmd.frame5.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command { 
		if {  $name != "" && $line != "" } {
 		set cmd [put_command]
 		if { $cmd != "" } {
 		   eval " exec xterm -title r.profile -geometry 50x5 -exec $cmd " 
 		   destroy .cmd
 		}
 	}
 }

  button .cmd.frame5.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command { destroy .cmd }

  pack append .cmd.frame5 \
    .cmd.frame5.ok { left expand } \
    .cmd.frame5.cancel { right expand }

  pack append .cmd.frame4 \
    .cmd.frame4.label { top fillx } \
    .cmd.frame4.button0 { left } \
    .cmd.frame4.button1  { left }



  pack append .cmd \
    .cmd.frame1 { top expand fill } \
    .cmd.frame2 { top expand fill } \
    .cmd.frame3 { top expand fill } \
    .cmd.frame4 { top expand fill } \
    .cmd.frame0 { top expand fill } \
    .cmd.frame5 { bottom expand fill }


bind .cmd.frame3.entry <KeyRelease> {
		set line [.cmd.frame3.entry get]
		set_command_entry
}

bind .cmd.frame3.entry <Return> {
		set line [.cmd.frame3.entry get]
		set_command_entry
}

  grab set .cmd
  tkwait window .cmd

}

global name
set name ""

global rtype
set rtype ""

global value
set value ""

global line
set line ""


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

proc_r.profile
