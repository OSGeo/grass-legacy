

proc put_command { } {


  global name


  set cmd ""

  if {  $name != "" } {
         set cmd "$cmd spheroid=$name"
  }

  if { $cmd != "" } {
     set cmd "m.region.ll $cmd"
  }
}




proc set_command_entry { } {

  .cmd.frame0.entry configure -state normal
  .cmd.frame0.entry delete 0 end
  .cmd.frame0.entry insert 0 [put_command]
  .cmd.frame0.entry configure -state disabled

}


proc proc_m.region.ll { } {


  global name


  toplevel .cmd

  wm geometry .cmd +100+20

  wm title .cmd "Convert Region UTM Coordinates to Geographic"


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
  .cmd.frame0.entry insert 0 {m.region.ll}

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
  .cmd.frame1.entry insert 0 {m.region.ll converts Universal Transverse Mercator (UTM) coordinates falling within the current geographic region from UTM coordinates to geographic (latitude/longitude coordinates.}

  .cmd.frame1.entry configure -state disabled



  frame .cmd.frame2 \
    -relief {flat}

  label .cmd.frame2.label \
     -anchor {w} \
     -text {Spheroid:} \
     -padx {2}

  listbox .cmd.frame2.listbox \
     -relief sunken \
     -yscrollcommand {.cmd.frame2.vscrollbar set}

  scrollbar .cmd.frame2.vscrollbar \
     -command {.cmd.frame2.listbox yview}


  pack append .cmd.frame2 \
    .cmd.frame2.label { top fillx } \
    .cmd.frame2.listbox { left expand fill } \
    .cmd.frame2.vscrollbar { right fill }

  bind .cmd.frame2.listbox <Button-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set name [%W get [%W nearest %y]]
  }

  bind .cmd.frame2.listbox <ButtonRelease-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set name [%W get [%W nearest %y]]
  }

  bind .cmd.frame2.listbox <Double-ButtonPress-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set name [%W get [%W nearest %y]]
  }

  .cmd.frame2.listbox delete 0 end
  foreach i {airy australian bessel clark66 everest grs80 hayford international krasovsky wgs66 wgs72 wgs84} {
      .cmd.frame2.listbox insert 0 $i
  }


  frame .cmd.frame3 \
     -borderwidth {2}

  button .cmd.frame3.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command { 
		if {  $name != "" } {
 		set cmd [put_command]
 		if { $cmd != "" } {
 		   eval " exec xterm -title m.region.ll -geometry 50x5 -exec $cmd " 
 		   destroy .cmd
 		}
 	}
 }

  button .cmd.frame3.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command { destroy .cmd }

  pack append .cmd.frame3 \
    .cmd.frame3.ok { left expand } \
    .cmd.frame3.cancel { right expand }


  pack append .cmd \
    .cmd.frame1 { top expand fill } \
    .cmd.frame2 { top expand fill } \
    .cmd.frame0 { top expand fill } \
    .cmd.frame3 { bottom expand fill }


bind .cmd.frame2.listbox <ButtonPress-1> {
 %W select from [%W nearest %y]
 %W select to [%W nearest %y]
		set name [%W get [%W nearest %y]]
		set_command_entry
}

bind .cmd.frame2.listbox <ButtonRelease-1> {
 %W select from [%W nearest %y]
 %W select to [%W nearest %y]
		set name [%W get [%W nearest %y]]
		set_command_entry
}

  grab set .cmd
  tkwait window .cmd

}

global name
set name ""


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

proc_m.region.ll
