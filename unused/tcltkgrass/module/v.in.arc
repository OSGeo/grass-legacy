

proc files { } {


  toplevel .files

  wm geometry .files +100+20
  wm title .files "files"


  global pname


  frame .files.frame0 \
    -relief {flat}

  label .files.frame0.label \
    -anchor {w} \
    -text {ARC/INFO ungenerate label-points file:} \
    -padx {2}

  scrollbar .files.frame0.hscrollbar \
    -command {.files.frame0.entry view} \
    -orient {horizontal}

  entry .files.frame0.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.files.frame0.hscrollbar set}

  pack append .files.frame0 \
    .files.frame0.label { top fillx } \
    .files.frame0.entry { top fillx } \
    .files.frame0.hscrollbar { bottom fillx }

  .files.frame0.entry configure -state normal

  bind .files.frame0.entry <Return> {
       set pname [%W get] }


  global textname


  frame .files.frame1 \
    -relief {flat}

  label .files.frame1.label \
    -anchor {w} \
    -text {ARC/INFO ungenerate label-text file:} \
    -padx {2}

  scrollbar .files.frame1.hscrollbar \
    -command {.files.frame1.entry view} \
    -orient {horizontal}

  entry .files.frame1.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.files.frame1.hscrollbar set}

  pack append .files.frame1 \
    .files.frame1.label { top fillx } \
    .files.frame1.entry { top fillx } \
    .files.frame1.hscrollbar { bottom fillx }

  .files.frame1.entry configure -state normal

  bind .files.frame1.entry <Return> {
       set textname [%W get] }



  frame .files.frame2 \
     -borderwidth {2}

  button .files.frame2.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set pname [.files.frame0.entry get]
                  set textname [.files.frame1.entry get]
                 destroy .files }

  button .files.frame2.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set pname ""
                  set textname ""
                 destroy .files }

  pack append .files.frame2 \
    .files.frame2.ok { left expand } \
    .files.frame2.cancel { right expand }


bind .files.frame0.entry <Return> {
     focus .files.frame1.entry
}

bind .files.frame1.entry <Return> {
     focus .files.frame0.entry
}


  pack append .files \
    .files.frame0 { top expand fill } \
    .files.frame1 { top expand fill } \
    .files.frame2 { bottom expand fill }



  grab set .files
  tkwait window .files

}




proc colums { } {


  toplevel .colums

  wm geometry .colums +100+20
  wm title .colums "colums"


  global ivalue


  frame .colums.frame0 \
    -relief {flat}

  label .colums.frame0.label \
    -anchor {w} \
    -text {ID number column in label-text file:} \
    -padx {2}

  scrollbar .colums.frame0.hscrollbar \
    -command {.colums.frame0.entry view} \
    -orient {horizontal}

  entry .colums.frame0.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.colums.frame0.hscrollbar set}

  pack append .colums.frame0 \
    .colums.frame0.label { top fillx } \
    .colums.frame0.entry { top fillx } \
    .colums.frame0.hscrollbar { bottom fillx }

  .colums.frame0.entry configure -state normal

  bind .colums.frame0.entry <Return> {
       set ivalue [%W get] }


  global cvalue


  frame .colums.frame1 \
    -relief {flat}

  label .colums.frame1.label \
    -anchor {w} \
    -text {GRASS category column in label-text file:} \
    -padx {2}

  scrollbar .colums.frame1.hscrollbar \
    -command {.colums.frame1.entry view} \
    -orient {horizontal}

  entry .colums.frame1.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.colums.frame1.hscrollbar set}

  pack append .colums.frame1 \
    .colums.frame1.label { top fillx } \
    .colums.frame1.entry { top fillx } \
    .colums.frame1.hscrollbar { bottom fillx }

  .colums.frame1.entry configure -state normal

  bind .colums.frame1.entry <Return> {
       set cvalue [%W get] }


  global avalue


  frame .colums.frame2 \
    -relief {flat}

  label .colums.frame2.label \
    -anchor {w} \
    -text {GRASS attribute column in label-text file:} \
    -padx {2}

  scrollbar .colums.frame2.hscrollbar \
    -command {.colums.frame2.entry view} \
    -orient {horizontal}

  entry .colums.frame2.entry \
    -relief {sunken} \
    -width 50 \
    -scrollcommand {.colums.frame2.hscrollbar set}

  pack append .colums.frame2 \
    .colums.frame2.label { top fillx } \
    .colums.frame2.entry { top fillx } \
    .colums.frame2.hscrollbar { bottom fillx }

  .colums.frame2.entry configure -state normal

  bind .colums.frame2.entry <Return> {
       set avalue [%W get] }



  frame .colums.frame3 \
     -borderwidth {2}

  button .colums.frame3.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                  set ivalue [.colums.frame0.entry get]
                  set cvalue [.colums.frame1.entry get]
                  set avalue [.colums.frame2.entry get]
                 destroy .colums }

  button .colums.frame3.cancel \
     -text Cancel \
     -relief raised \
     -padx 10 \
     -command {
                  set ivalue ""
                  set cvalue ""
                  set avalue ""
                 destroy .colums }

  pack append .colums.frame3 \
    .colums.frame3.ok { left expand } \
    .colums.frame3.cancel { right expand }


bind .colums.frame0.entry <Return> {
     focus .colums.frame1.entry
}

bind .colums.frame1.entry <Return> {
     focus .colums.frame2.entry
}

bind .colums.frame2.entry <Return> {
     focus .colums.frame0.entry
}


  pack append .colums \
    .colums.frame0 { top expand fill } \
    .colums.frame1 { top expand fill } \
    .colums.frame2 { top expand fill } \
    .colums.frame3 { bottom expand fill }



  grab set .colums
  tkwait window .colums

}




proc flag { } {


  toplevel .flag

  wm geometry .flag +100+20
  wm title .flag "flag"


  global n


  frame .flag.frame0 \
    -relief {flat}

  checkbutton .flag.frame0.checkbutton \
     -text "Neatline." \
     -relief flat \
     -anchor {w} \
     -onvalue "n" \
     -offvalue "" \
     -variable n

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


  global n
  global typename
  global lname
  global pname
  global textname
  global vname
  global ivalue
  global cvalue
  global avalue


  set cmd ""

  if {  $n != "" } {
         set cmd "$cmd -$n"
  }

  if {  $typename != "" } {
         set cmd "$cmd type=$typename"
  }

  if {  $lname != "" } {
         set cmd "$cmd lines_in=$lname"
  }

  if {  $pname != "" } {
         set cmd "$cmd points_in=$pname"
  }

  if {  $textname != "" } {
         set cmd "$cmd text_in=$textname"
  }

  if {  $vname != "" } {
         set cmd "$cmd vector_out=$vname"
  }

  if {  $ivalue != "" } {
         set cmd "$cmd idcol=$ivalue"
  }

  if {  $cvalue != "" } {
         set cmd "$cmd catcol=$cvalue"
  }

  if {  $avalue != "" } {
         set cmd "$cmd attcol=$avalue"
  }

  if { $cmd != "" } {
     set cmd "v.in.arc $cmd"
  }
}




proc set_command_entry { } {

  .cmd.frame0.entry configure -state normal
  .cmd.frame0.entry delete 0 end
  .cmd.frame0.entry insert 0 [put_command]
  .cmd.frame0.entry configure -state disabled

}


proc proc_v.in.arc { } {


  global n
  global typename
  global lname
  global pname
  global textname
  global vname
  global ivalue
  global cvalue
  global avalue


  toplevel .cmd

  wm geometry .cmd +100+20

  wm title .cmd "Convert ARC/INFO Data to GRASS Vector"


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
  .cmd.frame0.entry insert 0 {v.in.arc}

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
  .cmd.frame1.entry insert 0 {v.in.arc converts data in ARC/INFO format to GRASS's vector format, and stores output in the user's current GRASS mapset.}

  .cmd.frame1.entry configure -state disabled



  frame .cmd.frame2 \
    -relief {flat}

  label .cmd.frame2.label \
     -anchor {w} \
     -text {Coverage type:} \
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
       set typename [%W get [%W nearest %y]]
  }

  bind .cmd.frame2.listbox <ButtonRelease-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set typename [%W get [%W nearest %y]]
  }

  bind .cmd.frame2.listbox <Double-ButtonPress-1> {
       %W select from [%W nearest %y]
       %W select to [%W nearest %y]
       set typename [%W get [%W nearest %y]]
  }

  .cmd.frame2.listbox delete 0 end
  foreach i {polygon line} {
      .cmd.frame2.listbox insert 0 $i
  }


  frame .cmd.frame3 \
    -relief {flat}

  label .cmd.frame3.label \
    -anchor {w} \
    -text {ARC/INFO ungenerate lines file:} \
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
       set lname [%W get] }



  frame .cmd.frame4 \
    -relief {flat}

  label .cmd.frame4.label \
    -anchor {w} \
    -text {Resultant GRASS vector output file:} \
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
       set vname [%W get] }



  frame .cmd.frame5 \
    -relief {flat}

  label .cmd.frame5.label \
    -anchor {w} \
    -text {Other options available:} \
    -padx {2}

  button .cmd.frame5.button0 \
     -text "files ..." \
     -relief raised \
     -padx 10 \
     -command "files
                set_command_entry"


  button .cmd.frame5.button1 \
     -text "colums ..." \
     -relief raised \
     -padx 10 \
     -command "colums
                set_command_entry"


  button .cmd.frame5.button2 \
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
		if {  $typename != "" && $lname != "" && $vname != "" } {
 		set cmd [put_command]
 		if { $cmd != "" } {
 		   eval " exec xterm -title v.in.arc -geometry 50x5 -exec $cmd " 
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
    .cmd.frame5.button2  { left }



  pack append .cmd \
    .cmd.frame1 { top expand fill } \
    .cmd.frame2 { top expand fill } \
    .cmd.frame3 { top expand fill } \
    .cmd.frame4 { top expand fill } \
    .cmd.frame5 { top expand fill } \
    .cmd.frame0 { top expand fill } \
    .cmd.frame6 { bottom expand fill }


bind .cmd.frame2.listbox <ButtonPress-1> {
 %W select from [%W nearest %y]
 %W select to [%W nearest %y]
		set typename [%W get [%W nearest %y]]
		set_command_entry
}

bind .cmd.frame2.listbox <ButtonRelease-1> {
 %W select from [%W nearest %y]
 %W select to [%W nearest %y]
		set typename [%W get [%W nearest %y]]
		set_command_entry
}

bind .cmd.frame3.entry <KeyRelease> {
		set lname [.cmd.frame3.entry get]
		set_command_entry
}

bind .cmd.frame3.entry <Return> {
		set lname [.cmd.frame3.entry get]
		set_command_entry
}

bind .cmd.frame4.entry <KeyRelease> {
		set vname [.cmd.frame4.entry get]
		set_command_entry
}

bind .cmd.frame4.entry <Return> {
		set vname [.cmd.frame4.entry get]
		set_command_entry
}

  grab set .cmd
  tkwait window .cmd

}

global n
set n ""

global typename
set typename ""

global lname
set lname ""

global pname
set pname ""

global textname
set textname ""

global vname
set vname ""

global ivalue
set ivalue ""

global cvalue
set cvalue ""

global avalue
set avalue ""


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

proc_v.in.arc
