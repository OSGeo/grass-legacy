

proc redirection { } {


  toplevel .redirection

  wm geometry .redirection +100+20
  wm title .redirection "redirection"


  global file_name


  frame .redirection.frame0 \
    -relief {flat}

  label .redirection.frame0.label \
    -anchor {w} \
    -text {Output file:} \
    -padx {2}

  scrollbar .redirection.frame0.hscrollbar \
    -command {.redirection.frame0.entry view} \
    -orient {horizontal}

  entry .redirection.frame0.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.redirection.frame0.hscrollbar set}

  pack append .redirection.frame0 \
    .redirection.frame0.label { top fillx } \
    .redirection.frame0.entry { top fillx } \
    .redirection.frame0.hscrollbar { bottom fillx }

  .redirection.frame0.entry configure -state normal

  bind .redirection.frame0.entry <Return> {
       set file_name [%W get] }



  frame .redirection.frame1 \
     -borderwidth {2}

  button .redirection.frame1.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set file_name [.redirection.frame0.entry get]
                 destroy .redirection }

  button .redirection.frame1.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set file_name ""
                 destroy .redirection }

  pack append .redirection.frame1 \
    .redirection.frame1.ok { left expand } \
    .redirection.frame1.cancel { right expand }



  pack append .redirection \
    .redirection.frame0 { top expand fill } \
    .redirection.frame1 { bottom expand fill }



  grab set .redirection
  tkwait window .redirection

}




proc put_command { } {


  global tape_drive_name
  global file_name


  set cmd ""

  if {  $tape_drive_name != "" } {
         set cmd "$cmd $tape_drive_name"
  }

  if {  $file_name != "" } {
         set cmd "$cmd >$file_name"
  }

  if { $cmd != "" } {
     set cmd "i.tape.mss.h $cmd"
  }
}




proc set_command_entry { } {

  .cmd.frame0.entry configure -state normal
  .cmd.frame0.entry delete 0 end
  .cmd.frame0.entry insert 0 [put_command]
  .cmd.frame0.entry configure -state disabled

}


proc proc_i.tape.mss.h { } {


  global tape_drive_name
  global file_name


  toplevel .cmd

  wm geometry .cmd +100+20

  wm title .cmd "Extract Header Information from LANDSAT MSS"


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
  .cmd.frame0.entry insert 0 {i.tape.mss.h}

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
  .cmd.frame1.entry insert 0 {i.tape.mss.h extracts header information from LANDSAT Multispectral Scanner (MSS) imagery data stored on half-inch tape.}

  .cmd.frame1.entry configure -state disabled



  frame .cmd.frame2 \
    -relief {flat}

  label .cmd.frame2.label \
    -anchor {w} \
    -text {Input file:} \
    -padx {2}

  scrollbar .cmd.frame2.hscrollbar \
    -command {.cmd.frame2.entry view} \
    -orient {horizontal}

  entry .cmd.frame2.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.cmd.frame2.hscrollbar set}

  pack append .cmd.frame2 \
    .cmd.frame2.label { top fillx } \
    .cmd.frame2.entry { top fillx } \
    .cmd.frame2.hscrollbar { bottom fillx }

  .cmd.frame2.entry configure -state normal

  bind .cmd.frame2.entry <Return> {
       set tape_drive_name [%W get] }



  frame .cmd.frame3 \
    -relief {flat}

  label .cmd.frame3.label \
    -anchor {w} \
    -text {Other options available:} \
    -padx {2}

  button .cmd.frame3.button0 \
     -text "redirection ..." \
     -relief raised \
     -padx 10 \
     -command "redirection
                set_command_entry"


  frame .cmd.frame4 \
     -borderwidth {2}

  button .cmd.frame4.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command { 
		if {  $tape_drive_name != "" } {
 		set cmd [put_command]
 		if { $cmd != "" } {
 		   eval " exec xterm -title i.tape.mss.h -geometry 50x5 -exec $cmd " 
 		   destroy .cmd
 		}
 	}
 }

  button .cmd.frame4.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command { destroy .cmd }

  pack append .cmd.frame4 \
    .cmd.frame4.ok { left expand } \
    .cmd.frame4.cancel { right expand }

  pack append .cmd.frame3 \
    .cmd.frame3.label { top fillx } \
    .cmd.frame3.button0  { left }



  pack append .cmd \
    .cmd.frame1 { top expand fill } \
    .cmd.frame2 { top expand fill } \
    .cmd.frame3 { top expand fill } \
    .cmd.frame0 { top expand fill } \
    .cmd.frame4 { bottom expand fill }


bind .cmd.frame2.entry <KeyRelease> {
		set tape_drive_name [.cmd.frame2.entry get]
		set_command_entry
}

bind .cmd.frame2.entry <Return> {
		set tape_drive_name [.cmd.frame2.entry get]
		set_command_entry
}

  grab set .cmd
  tkwait window .cmd

}

global tape_drive_name
set tape_drive_name ""

global file_name
set file_name ""


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

proc_i.tape.mss.h
