#
# Program : tcltkgrass.tcl
# 
#


#***********************************************************************
#
# PROCEDURE: dir_dialog
#
# ARGUMENTS: void
#
# RETURNED VALUES: void
#
# DESCRIPTION : 
#
#***********************************************************************

proc dir_dialog { } {

  toplevel .dir

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .dir +100+20
  wm title .dir {Selecting file}

  wm transient .dir .

  global path_label \
         dir_list \
         file_entry \
         ok \
         cancel \
         fileselect_ok \
         fileselect_cancel

  global file_selected

  set path_label  .dir.frame0.label1
  set dir_list    .dir.frame0.listbox1
  set scroll_list .dir.frame0.scrollbar2
  set file_entry  .dir.frame0.entry1

  set ok          .dir.frame1.frame.ok
  set cancel      .dir.frame1.frame.cancel
  set dir "."

  frame .dir.frame0 \
    -borderwidth {2} \
    -relief {raised}

  label .dir.frame0.label1 \
    -anchor {w} \
    -text [exec pwd] 

  scrollbar .dir.frame0.scrollbar2 \
    -command {.dir.frame0.listbox1 yview} \
    -relief {raised}

  listbox .dir.frame0.listbox1 \
    -relief {raised} \
    -yscrollcommand {.dir.frame0.scrollbar2 set}

  scrollbar .dir.frame0.scrollbar3 \
    -command { .dir.frame0.entry1 view} \
    -relief {raised} \
    -orient {horizontal}
 
  entry .dir.frame0.entry1 \
    -relief {sunken} \
    -xscrollcommand { .dir.frame0.scrollbar3 set}

  frame .dir.frame1 \
    -borderwidth {2}

  frame .dir.frame1.frame
  
  button .dir.frame1.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                set selected [$file_entry get]

                if {[file isdirectory $selected] != 0} {
	           cd $selected
	           set dir [exec pwd]
	           eval $path_label configure -text $dir
	           eval $file_entry delete 0 end
	           eval $dir_list delete 0 end
	           foreach i [exec ls -a $dir] {
	               if {[string compare $i "."] != 0} {
		          eval $dir_list insert end $i
	               }
	           }
                }
                set file_selected "$dir/$selected"
                destroy .dir
              }


  button .dir.frame1.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command { 
               set file_selected ""
               destroy .dir
             }

  pack append .dir.frame1.frame \
    .dir.frame1.frame.ok { left } \
    .dir.frame1.frame.cancel { right }

  pack append .dir.frame1 \
    .dir.frame1.frame { bottom frame center }

  pack append .dir.frame0 \
    .dir.frame0.label1 { top fill } \
    .dir.frame0.scrollbar3 { bottom fill } \
    .dir.frame0.entry1 { bottom fill } \
    .dir.frame0.scrollbar2 { right filly } \
    .dir.frame0.listbox1 { left expand fill }

  pack append .dir \
    .dir.frame0 { top expand fill } \
    .dir.frame1 { bottom expand fill }


  foreach i [exec ls -a [exec pwd]] {
      if { [string compare $i "."] != 0 } {
           .dir.frame0.listbox1 insert end $i
      }
  }

  bind .dir.frame0.listbox1 <Control-c> {
      set file_selected ""
      destroy .dir
  }

  bind . <Control-c> {
      set file_selected ""
      destroy .dir
  }

  bind $dir_list <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	eval $file_entry delete 0 end
	eval $file_entry insert 0 [%W get [%W nearest %y]]
        set file_selected [$file_entry get] 
  }

  bind $dir_list <Button-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	eval $file_entry delete 0 end
	eval $file_entry insert 0 [%W get [%W nearest %y]]
        set file_selected [$file_entry get] 
  }

  bind $dir_list <Key> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
        eval $file_entry delete 0 end
	eval $file_entry insert 0 [%W get [%W nearest %y]]
        set file_selected [$file_entry get] 
  }

  bind $dir_list <Double-ButtonPress-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	eval $file_entry delete 0 end
	eval $file_entry insert 0 [%W get [%W nearest %y]]
        set selected [$file_entry get]

        if {[file isdirectory $selected] != 0} {
	   cd $selected
	   set dir [exec pwd]
	   eval $path_label configure -text $dir
	   eval $file_entry delete 0 end
	   eval $dir_list delete 0 end
	   foreach i [exec ls -a $dir] {
	       if {[string compare $i "."] != 0} {
   		  eval $dir_list insert end $i
	       }
	   }
        }
        set file_selected $selected
  }

  bind $dir_list <Return> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	eval $file_entry delete 0 end
	eval $file_entry insert 0 [%W get [%W nearest %y]]

        set selected [$file_entry get]

        if {[file isdirectory $selected] != 0} {
	   cd $selected
	   set dir [exec pwd]
	   eval $path_label configure -text $dir
	   eval $file_entry delete 0 end
	   eval $dir_list delete 0 end
	   foreach i [exec ls -a $dir] {
	       if {[string compare $i "."] != 0} {
		  eval $dir_list insert end $i
	       }
	   }
        }
        set file_selected $selected
  }

  grab set .dir
  tkwait window .dir

  return $file_selected
}





#***********************************************************************
#
# PROCEDURE: fonts
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc fonts { } {

   global tcltkgrassbase
 
   return [FontBox $tcltkgrassbase/script/font.txt] 
}
#***********************************************************************
#
# PROCEDURE: colors
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc getcolors { } {
 
  global tcltkgrassbase
 
  return  [ColorBox $tcltkgrassbase/script/color.txt]
}
 




#***********************************************************************
#
# PROCEDURE: labels
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc labels { } {

  global fname
  set fname ""

  global cname
  set cname ""

  global up_scale
  set upper_limit ""

  global down_scale
  set lower_limit ""


  toplevel .labels

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .labels +100+100
  wm title .labels {labels}


  # ---------------------------
  # build .labels.frame0
  # ---------------------------

  frame .labels.frame0 \
    -borderwidth {2} \
    -relief {flat}

  label .labels.frame0.label \
    -anchor {w} \
    -text "Color"

  entry .labels.frame0.entry \
    -relief {sunken} \
    -width 20

  button .labels.frame0.button \
     -text "colors ..." \
     -relief raised \
     -padx 10 \
     -command {
                set color [getcolors]
                if { $color != "" } {
                   .labels.frame0.entry delete 0 end
                   .labels.frame0.entry insert 0 $color
                   set cname $color
                }
              }
 
 
  pack append .labels.frame0 \
    .labels.frame0.label { top fillx } \
    .labels.frame0.entry { left expand fill } \
    .labels.frame0.button { right }

  bind .labels.frame0.entry <Return> {
    set cname [.labels.frame0.entry get]
  }


  # ---------------------------
  # build .labels.frame1
  # ---------------------------

  frame .labels.frame1 \
    -borderwidth {2} \
    -relief {flat}

  label .labels.frame1.label \
    -anchor {w} \
    -text "Font"

  entry .labels.frame1.entry \
    -relief {sunken} \
    -width 20

  button .labels.frame1.button \
     -text "fonts ..." \
     -relief raised \
     -padx 10 \
     -command {
                set fonts [fonts]
                if { $fonts != "" } {
                   .labels.frame1.entry delete 0 end
                   .labels.frame1.entry insert 0 $fonts
                   set fname $fonts
                }
              }
 
 
  pack append .labels.frame1 \
    .labels.frame1.label { top fillx } \
    .labels.frame1.entry { left expand fill } \
    .labels.frame1.button { right }
 
  bind .labels.frame1.entry <Return> {
    set fname [.labels.frame1.entry get]
  }


  # ---------------------------
  # build .labels.frame2
  # ---------------------------

  frame .labels.frame2 \
    -borderwidth {2} \
    -relief {groove}

  frame .labels.frame2.frame0 \
    -borderwidth {2}

  label .labels.frame2.frame0.label0 \
    -text {lower limit}

  label .labels.frame2.frame0.label1 \
    -text {Magnitude}

  radiobutton .labels.frame2.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {down_scale}

  radiobutton .labels.frame2.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {down_scale}

  radiobutton .labels.frame2.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {down_scale}

  scale  .labels.frame2.frame0.scale \
    -sliderlength {20}

  pack append .labels.frame2.frame0 \
     .labels.frame2.frame0.label0 { top frame center fillx } \
     .labels.frame2.frame0.scale { right } \
     .labels.frame2.frame0.label1 { top frame center } \
     .labels.frame2.frame0.button0 { top frame nw } \
     .labels.frame2.frame0.button1 { top frame nw } \
     .labels.frame2.frame0.button2 { top frame nw }


  pack append .labels.frame2 \
     .labels.frame2.frame0 { top frame center }



  # ---------------------------
  # build .labels.frame3
  # ---------------------------

  frame .labels.frame3 \
    -borderwidth {2} \
    -relief {groove}


  frame .labels.frame3.frame0 \
    -borderwidth {2}

  label .labels.frame3.frame0.label0 \
    -text {upper limit}

  label .labels.frame3.frame0.label1 \
    -text {Magnitude}

  radiobutton .labels.frame3.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {up_scale}

  radiobutton .labels.frame3.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {up_scale}

  radiobutton .labels.frame3.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {up_scale}

  scale  .labels.frame3.frame0.scale \
    -sliderlength {20}

  pack append .labels.frame3.frame0 \
     .labels.frame3.frame0.label0 { top frame center fillx } \
     .labels.frame3.frame0.scale { right } \
     .labels.frame3.frame0.label1 { top frame center } \
     .labels.frame3.frame0.button0 { top frame nw } \
     .labels.frame3.frame0.button1 { top frame nw } \
     .labels.frame3.frame0.button2 { top frame nw }


  pack append .labels.frame3 \
     .labels.frame3.frame0 { top frame center }


  # ---------------------------
  # build .labels.frame4
  # ---------------------------

  frame .labels.frame4 \
    -borderwidth {2}

  frame .labels.frame4.frame
  
  button .labels.frame4.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
	        set cname [.labels.frame0.entry get]
                set fname [.labels.frame1.entry get]
                if { $down_scale != "" && $up_scale != "" } {
                   set down_scale [expr [.labels.frame3.frame0.scale get]*$down_scale]
                   set up_scale [expr [.labels.frame2.frame0.scale get]*$up_scale]
                }
                destroy .labels
              }

  button .labels.frame4.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command { 
               set cname ""
               set fname ""
               set down_scale ""
               set up_scale ""
               destroy .labels
             }

  pack append .labels.frame4.frame \
    .labels.frame4.frame.ok { left expand } \
    .labels.frame4.frame.cancel { right expand }

  pack append .labels.frame4 \
    .labels.frame4.frame { bottom frame center fill }


  pack append .labels \
    .labels.frame0 { top expand fill } \
    .labels.frame1 { top expand fill } \
    .labels.frame4 { bottom expand fill } \
    .labels.frame2 { right } \
    .labels.frame3 { left }

  grab set .labels
  tkwait window .labels

  set cmd ""
  if { $cname != "" } {
     set cmd "$cmd -color $cname"
  }

  if { $fname != "" } {
     set cmd "$cmd -font $fname"
  }

  if { $down_scale != "" } {
     set cmd "$cmd -downscale $down_scale"
  }

  if { $up_scale != "" } {
     set cmd "$cmd -upscale $up_scale"
  }

  return $cmd
}





#***********************************************************************
#
# PROCEDURE: points
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc points { } {

  global sname
  set sname ""

  global cname
  set cname ""

  global up_scale
  set upper_limit ""

  global down_scale
  set lower_limit ""


  toplevel .points

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .points +100+100
  wm title .points {points}


  # ---------------------------
  # build .points.frame0
  # ---------------------------

  frame .points.frame0 \
    -borderwidth {2} \
    -relief {flat}

  label .points.frame0.label \
    -anchor {w} \
    -text "Color"

  entry .points.frame0.entry \
    -relief {sunken} \
    -width 20

  button .points.frame0.button \
     -text "colors ..." \
     -relief raised \
     -padx 10 \
     -command {
                set color [getcolors]
                if { $color != "" } {
                   .points.frame0.entry delete 0 end
                   .points.frame0.entry insert 0 $color
                   set cname $color
                }
              }
 
 
  pack append .points.frame0 \
    .points.frame0.label { top fillx } \
    .points.frame0.entry { left expand fill } \
    .points.frame0.button { right }

  bind .points.frame0.entry <Return> {
    set cname [.points.frame0.entry get]
  }


  # ---------------------------
  # build .points.frame5
  # ---------------------------

  frame .points.frame5 \
    -borderwidth {2} \
    -relief {flat}

  label .points.frame5.label \
    -anchor {w} \
    -text "Symbol"

  entry .points.frame5.entry \
    -relief {sunken} \
    -width 20

  button .points.frame5.button \
     -text "Symbol ..." \
     -relief raised \
     -padx 10 \
     -command {
                set symbol_name [dir_dialog]
                if { $symbol_name != "" } {
                   focus .points.frame5.entry
                   .points.frame5.entry delete 0 end
                   .points.frame5.entry insert 0 $symbol_name
                   set sname $symbol_name 
                }
              }
 
 
  pack append .points.frame5 \
    .points.frame5.label { top fillx } \
    .points.frame5.entry { left expand fill } \
    .points.frame5.button { right }


  bind .points.frame5.entry <Return> {
    set sname [.points.frame5.entry get]
  }

  # ---------------------------
  # build .points.frame2
  # ---------------------------

  frame .points.frame2 \
    -borderwidth {2} \
    -relief {groove}


  frame .points.frame2.frame0 \
    -borderwidth {2}

  label .points.frame2.frame0.label0 \
    -text {lower limit}

  label .points.frame2.frame0.label1 \
    -text {Magnitude}

  radiobutton .points.frame2.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {down_scale}

  radiobutton .points.frame2.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {down_scale}

  radiobutton .points.frame2.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {down_scale}

  scale  .points.frame2.frame0.scale \
    -sliderlength {20}

  pack append .points.frame2.frame0 \
     .points.frame2.frame0.label0 { top frame center fillx } \
     .points.frame2.frame0.scale { right } \
     .points.frame2.frame0.label1 { top frame center } \
     .points.frame2.frame0.button0 { top frame nw } \
     .points.frame2.frame0.button1 { top frame nw } \
     .points.frame2.frame0.button2 { top frame nw }


  pack append .points.frame2 \
     .points.frame2.frame0 { top frame center }



  # ---------------------------
  # build .points.frame3
  # ---------------------------

  frame .points.frame3 \
    -borderwidth {2} \
    -relief {groove}


  frame .points.frame3.frame0 \
    -borderwidth {2}

  label .points.frame3.frame0.label0 \
    -text {upper limit}

  label .points.frame3.frame0.label1 \
    -text {Magnitude}

  radiobutton .points.frame3.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {up_scale}

  radiobutton .points.frame3.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {up_scale}

  radiobutton .points.frame3.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {up_scale}

  scale  .points.frame3.frame0.scale \
    -sliderlength {20}

  pack append .points.frame3.frame0 \
     .points.frame3.frame0.label0 { top frame center fillx } \
     .points.frame3.frame0.scale { right } \
     .points.frame3.frame0.label1 { top frame center } \
     .points.frame3.frame0.button0 { top frame nw } \
     .points.frame3.frame0.button1 { top frame nw } \
     .points.frame3.frame0.button2 { top frame nw }


  pack append .points.frame3 \
     .points.frame3.frame0 { top frame center }


  # ---------------------------
  # build .points.frame4
  # ---------------------------

  frame .points.frame4 \
    -borderwidth {2}

  frame .points.frame4.frame
  
  button .points.frame4.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                set cname [.points.frame0.entry get]
                set sname [.points.frame5.entry get]
                if { $down_scale != "" && $up_scale != "" } {
                   set down_scale [expr [.points.frame3.frame0.scale get]*$down_scale]
                   set up_scale [expr [.points.frame2.frame0.scale get]*$up_scale]
                }
                destroy .points
              }

  button .points.frame4.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command {
               set cname ""
               set sname ""
               set down_scale ""
               set up_scale ""
               destroy .points
             }

  pack append .points.frame4.frame \
    .points.frame4.frame.ok { left expand } \
    .points.frame4.frame.cancel { right expand }

  pack append .points.frame4 \
    .points.frame4.frame { bottom frame center fill }


  pack append .points \
    .points.frame0 { top expand fill } \
    .points.frame5 { top expand fill } \
    .points.frame4 { bottom expand fill } \
    .points.frame2 { right } \
    .points.frame3 { left }

  grab set .points
  tkwait window .points

  set cmd ""
  if { $cname != "" } {
     set cmd "$cmd -color $cname"
  }

  if { $sname != "" } {
     set cmd "$cmd -bitmap @$sname"
  }

  if { $down_scale != "" } {
     set cmd "$cmd -downscale $down_scale"
  }

  if { $up_scale != "" } {
     set cmd "$cmd -upscale $up_scale"
  }

  return $cmd
}




#***********************************************************************
#
# PROCEDURE: polyline
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc polyline { } {

  global color
  set color ""

  global dash
  set category ""

  global fill
  set fill ""

  global width
  set width ""

  global down_scale
  set down_scale ""

  global up_scale
  set up_scale ""

  toplevel .polyline

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .polyline +100+100
  wm title .polyline {polyline}


  # ---------------------------
  # build .polyline.frame0
  # ---------------------------

  frame .polyline.frame0 \
    -borderwidth {2} \
    -relief {flat}

  label .polyline.frame0.label \
    -anchor {w} \
    -text "Color"

  entry .polyline.frame0.entry \
    -relief {sunken} \
    -width 20

  button .polyline.frame0.button \
     -text "colors ..." \
     -relief raised \
     -padx 10 \
     -command {
                set color_name [getcolors]
                if { $color_name != "" } {
                   .polyline.frame0.entry delete 0 end
                   .polyline.frame0.entry insert 0 $color_name
                   set color $color_name
                }
              }
 
 
  pack append .polyline.frame0 \
    .polyline.frame0.label { top fillx } \
    .polyline.frame0.entry { left expand fill } \
    .polyline.frame0.button { right }


  bind .polyline.frame0.entry <Return> {
    set color [.polyline.frame0.entry get]
  }

  # ---------------------------
  # build .polyline.frame5
  # ---------------------------

  frame .polyline.frame5 \
    -borderwidth {2} \
    -relief {flat}

  label .polyline.frame5.label \
    -anchor {w} \
    -text "Fill"

  entry .polyline.frame5.entry \
    -relief {sunken} \
    -width 20

  button .polyline.frame5.button \
     -text "Symbol ..." \
     -relief raised \
     -padx 10 \
     -command {
                set symbol_name [dir_dialog]
                if { $symbol_name != "" } {
                   focus .polyline.frame5.entry
                   .polyline.frame5.entry delete 0 end
                   .polyline.frame5.entry insert 0 $symbol_name
                   set fill $symbol_name 
                }
              }
 
 
  pack append .polyline.frame5 \
    .polyline.frame5.label { top fillx } \
    .polyline.frame5.entry { left expand fill } \
    .polyline.frame5.button { right }


  bind .polyline.frame5.entry <Return> {
    set fill [.polyline.frame5.entry get]
  }

  # ---------------------------
  # build .polyline.frame1
  # ---------------------------

  frame .polyline.frame1 \
    -borderwidth {2} \
    -relief {flat}

  label .polyline.frame1.label \
    -anchor {w} \
    -text "Dash"

  entry .polyline.frame1.entry \
    -relief {sunken} \
    -width 20

  pack append .polyline.frame1 \
    .polyline.frame1.label { top fillx } \
    .polyline.frame1.entry { left }

  bind .polyline.frame1.entry <Return> {
    set dash [.polyline.frame1.entry get]
  }


  # ---------------------------
  # build .polyline.frame6
  # ---------------------------

  frame .polyline.frame6 \
    -borderwidth {2} \
    -relief {flat}

  scale .polyline.frame6.scale \
     -label "Line width:" \
     -from 0 \
     -to 10 \
     -length 200 \
     -orient {horizontal}

  pack append .polyline.frame6 \
    .polyline.frame6.scale { top frame center  }


  # ---------------------------
  # build .polyline.frame2
  # ---------------------------

  frame .polyline.frame2 \
    -borderwidth {2} \
    -relief {groove}


  frame .polyline.frame2.frame0 \
    -borderwidth {2}

  label .polyline.frame2.frame0.label0 \
    -text {lower limit}

  label .polyline.frame2.frame0.label1 \
    -text {Magnitude}

  radiobutton .polyline.frame2.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {down_scale}

  radiobutton .polyline.frame2.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {down_scale}

  radiobutton .polyline.frame2.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {down_scale}

  scale  .polyline.frame2.frame0.scale \
    -sliderlength {20}

  pack append .polyline.frame2.frame0 \
     .polyline.frame2.frame0.label0 { top frame center fillx } \
     .polyline.frame2.frame0.scale { right } \
     .polyline.frame2.frame0.label1 { top frame center } \
     .polyline.frame2.frame0.button0 { top frame nw } \
     .polyline.frame2.frame0.button1 { top frame nw } \
     .polyline.frame2.frame0.button2 { top frame nw }


  pack append .polyline.frame2 \
     .polyline.frame2.frame0 { top frame center }



  # ---------------------------
  # build .polyline.frame3
  # ---------------------------

  frame .polyline.frame3 \
    -borderwidth {2} \
    -relief {groove}


  frame .polyline.frame3.frame0 \
    -borderwidth {2}

  label .polyline.frame3.frame0.label0 \
    -text {upper limit}

  label .polyline.frame3.frame0.label1 \
    -text {Magnitude}

  radiobutton .polyline.frame3.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {up_scale}

  radiobutton .polyline.frame3.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {up_scale}

  radiobutton .polyline.frame3.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {up_scale}

  scale  .polyline.frame3.frame0.scale \
    -sliderlength {20}

  pack append .polyline.frame3.frame0 \
     .polyline.frame3.frame0.label0 { top frame center fillx } \
     .polyline.frame3.frame0.scale { right } \
     .polyline.frame3.frame0.label1 { top frame center } \
     .polyline.frame3.frame0.button0 { top frame nw } \
     .polyline.frame3.frame0.button1 { top frame nw } \
     .polyline.frame3.frame0.button2 { top frame nw }


  pack append .polyline.frame3 \
     .polyline.frame3.frame0 { top frame center }


  # ---------------------------
  # build .polyline.frame4
  # ---------------------------

  frame .polyline.frame4 \
    -borderwidth {2}

  frame .polyline.frame4.frame
  
  button .polyline.frame4.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                set color [.polyline.frame0.entry get]
                set fill [.polyline.frame5.entry get]
                set dash [.polyline.frame1.entry get]
                set width [.polyline.frame6.scale get]
                if { $down_scale != "" && $up_scale != "" } {
                   set down_scale [expr [.polyline.frame3.frame0.scale get]*$down_scale]
                   set up_scale [expr [.polyline.frame2.frame0.scale get]*$up_scale]
                }
                destroy .polyline
              }

  button .polyline.frame4.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command {
               set color ""
               set fill ""
               set dash ""
               set width ""
               set down_scale ""
               set up_scale ""
               destroy .polyline
             }

  pack append .polyline.frame4.frame \
    .polyline.frame4.frame.ok { left expand } \
    .polyline.frame4.frame.cancel { right expand }

  pack append .polyline.frame4 \
    .polyline.frame4.frame { bottom frame center fill }


  pack append .polyline \
    .polyline.frame0 { top expand fill } \
    .polyline.frame1 { top expand fill } \
    .polyline.frame5 { top expand fill } \
    .polyline.frame6 { top expand fill } \
    .polyline.frame4 { bottom expand fill } \
    .polyline.frame2 { right } \
    .polyline.frame3 { left }

  grab set .polyline
  tkwait window .polyline

  set cmd ""
  if { $color != "" } {
     set cmd "$cmd -color $color"
  }

  if { $fill != "" } {
     set cmd "$cmd -fill @$fill"
  }

  if { $dash != "" } {
     set cmd "$cmd -dash $dash"
  }

  if { $width != "" } {
     set cmd "$cmd -width $width"
  }

  if { $down_scale != "" } {
     set cmd "$cmd -downscale $down_scale"
  }

  if { $up_scale != "" } {
     set cmd "$cmd -upscale $up_scale"
  }

  return $cmd
}




#***********************************************************************
#
# PROCEDURE: rasters
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc rasters { } {

  global color_table
  set color_table "1"

  global up_scale
  set up_scale ""

  global down_scale
  set down_scale ""

  toplevel .rasters

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .rasters +100+100
  wm title .rasters {rasters}


  # ---------------------------
  # build .rasters.frame0
  # ---------------------------

  frame .rasters.frame0 \
    -borderwidth {2} \
    -relief {flat}

  label .rasters.frame0.label \
    -anchor {c} \
    -text "Color table"

  radiobutton .rasters.frame0.button0 \
    -relief {flat} \
    -text {float} \
    -value {1} \
    -variable {color_table}

  radiobutton .rasters.frame0.button1  \
    -relief {flat} \
    -text {fixed} \
    -value {2} \
    -variable {color_table}

  pack append .rasters.frame0 \
    .rasters.frame0.label { top } \
    .rasters.frame0.button0 { top } \
    .rasters.frame0.button1 { top }


  # ---------------------------
  # build .rasters.frame2
  # ---------------------------

  frame .rasters.frame2 \
    -borderwidth {2} \
    -relief {groove}


  frame .rasters.frame2.frame0 \
    -borderwidth {2}

  label .rasters.frame2.frame0.label0 \
    -text {lower limit}

  label .rasters.frame2.frame0.label1 \
    -text {Magnitude}

  radiobutton .rasters.frame2.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {down_scale}

  radiobutton .rasters.frame2.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {down_scale}

  radiobutton .rasters.frame2.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {down_scale}

  scale  .rasters.frame2.frame0.scale \
    -sliderlength {20}

  pack append .rasters.frame2.frame0 \
     .rasters.frame2.frame0.label0 { top frame center fillx } \
     .rasters.frame2.frame0.scale { right } \
     .rasters.frame2.frame0.label1 { top frame center } \
     .rasters.frame2.frame0.button0 { top frame nw } \
     .rasters.frame2.frame0.button1 { top frame nw } \
     .rasters.frame2.frame0.button2 { top frame nw }


  pack append .rasters.frame2 \
     .rasters.frame2.frame0 { top frame center }



  # ---------------------------
  # build .rasters.frame3
  # ---------------------------

  frame .rasters.frame3 \
    -borderwidth {2} \
    -relief {groove}


  frame .rasters.frame3.frame0 \
    -borderwidth {2}

  label .rasters.frame3.frame0.label0 \
    -text {upper limit}

  label .rasters.frame3.frame0.label1 \
    -text {Magnitude}

  radiobutton .rasters.frame3.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {up_scale}

  radiobutton .rasters.frame3.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {up_scale}

  radiobutton .rasters.frame3.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {up_scale}

  scale  .rasters.frame3.frame0.scale \
    -sliderlength {20}

  pack append .rasters.frame3.frame0 \
     .rasters.frame3.frame0.label0 { top frame center fillx } \
     .rasters.frame3.frame0.scale { right } \
     .rasters.frame3.frame0.label1 { top frame center } \
     .rasters.frame3.frame0.button0 { top frame nw } \
     .rasters.frame3.frame0.button1 { top frame nw } \
     .rasters.frame3.frame0.button2 { top frame nw }


  pack append .rasters.frame3 \
     .rasters.frame3.frame0 { top frame center }


  # ---------------------------
  # build .rasters.frame4
  # ---------------------------

  frame .rasters.frame4 \
    -borderwidth {2}

  frame .rasters.frame4.frame
  
  button .rasters.frame4.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                if { $down_scale != "" && $up_scale != "" } {
                   set down_scale [expr [.rasters.frame3.frame0.scale get]*$down_scale]
                   set up_scale [expr [.rasters.frame2.frame0.scale get]*$up_scale]
                }
                destroy .rasters
              }

  button .rasters.frame4.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command {
               set color_table ""
               set up_scale ""
               set down_scale ""
               destroy .rasters
             }

  pack append .rasters.frame4.frame \
    .rasters.frame4.frame.ok { left expand } \
    .rasters.frame4.frame.cancel { right expand }

  pack append .rasters.frame4 \
    .rasters.frame4.frame { bottom frame center fill }


  pack append .rasters \
    .rasters.frame0 { top expand fill } \
    .rasters.frame4 { bottom expand fill } \
    .rasters.frame2 { right } \
    .rasters.frame3 { left }

  grab set .rasters
  tkwait window .rasters

  set cmd ""
  if { $color_table != "" } {
     set cmd "$cmd -mode $color_table"
  }

  if { $down_scale != "" } {
     set cmd "$cmd -downscale $down_scale"
  }

  if { $up_scale != "" } {
     set cmd "$cmd -upscale $up_scale"
  }

  return $cmd
}




#***********************************************************************
#
# PROCEDURE: sites
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc sites { } {


  global fname
  set fname ""

  global cname
  set cname ""

  global sname
  set sname ""

  global up_scale
  set upper_limit ""

  global down_scale
  set lower_limit ""

  toplevel .sites

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .sites +100+100
  wm title .sites {sites}


  # ---------------------------
  # build .sites.frame0
  # ---------------------------

  frame .sites.frame0 \
    -borderwidth {2} \
    -relief {flat}

  label .sites.frame0.label \
    -anchor {w} \
    -text "Color"

  entry .sites.frame0.entry \
    -relief {sunken} \
    -width 20

  button .sites.frame0.button \
     -text "colors ..." \
     -relief raised \
     -padx 10 \
     -command {
                set color [getcolors]
                if { $color != "" } {
                   .sites.frame0.entry delete 0 end
                   .sites.frame0.entry insert 0 $color
                   set cname $color
                }
              }
 
 
  pack append .sites.frame0 \
    .sites.frame0.label { top fillx } \
    .sites.frame0.entry { left expand fill } \
    .sites.frame0.button { right }


  bind .sites.frame0.entry <Return> {
    set cname [.sites.frame0.entry get]
  }

  # ---------------------------
  # build .sites.frame1
  # ---------------------------

  frame .sites.frame1 \
    -borderwidth {2} \
    -relief {flat}

  label .sites.frame1.label \
    -anchor {w} \
    -text "Font"

  entry .sites.frame1.entry \
    -relief {sunken} \
    -width 20

  button .sites.frame1.button \
     -text "fonts ..." \
     -relief raised \
     -padx 10 \
     -command {
                set fonts [fonts]
                if { $fonts != "" } {
                   .sites.frame1.entry delete 0 end
                   .sites.frame1.entry insert 0 $fonts
                   set fname $fonts
                }
              }
 
 
  pack append .sites.frame1 \
    .sites.frame1.label { top fillx } \
    .sites.frame1.entry { left expand fill } \
    .sites.frame1.button { right }


  bind .sites.frame1.entry <Return> {
     set fname [.sites.frame1.entry get]
  }

  # ---------------------------
  # build .sites.frame5
  # ---------------------------

  frame .sites.frame5 \
    -borderwidth {2} \
    -relief {flat}

  label .sites.frame5.label \
    -anchor {w} \
    -text "Symbol"

  entry .sites.frame5.entry \
    -relief {sunken} \
    -width 20

  button .sites.frame5.button \
     -text "Symbol ..." \
     -relief raised \
     -padx 10 \
     -command {
                set symbol_name [dir_dialog]
                if { $symbol_name != "" } {
                   focus .sites.frame5.entry
                   .sites.frame5.entry delete 0 end
                   .sites.frame5.entry insert 0 $symbol_name
                   set sname $symbol_name 
                }
              }
 
 
  pack append .sites.frame5 \
    .sites.frame5.label { top fillx } \
    .sites.frame5.entry { left expand fill } \
    .sites.frame5.button { right }


  bind .sites.frame5.entry <Return> {
     set sname [.sites.frame5.entry get]
  }


  # ---------------------------
  # build .sites.frame2
  # ---------------------------

  frame .sites.frame2 \
    -borderwidth {2} \
    -relief {groove}

  frame .sites.frame2.frame0 \
    -borderwidth {2}

  label .sites.frame2.frame0.label0 \
    -text {lower limit}

  label .sites.frame2.frame0.label1 \
    -text {Magnitude}

  radiobutton .sites.frame2.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {down_scale}

  radiobutton .sites.frame2.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {down_scale}

  radiobutton .sites.frame2.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {down_scale}

  scale  .sites.frame2.frame0.scale \
    -sliderlength {20}

  pack append .sites.frame2.frame0 \
     .sites.frame2.frame0.label0 { top frame center fillx } \
     .sites.frame2.frame0.scale { right } \
     .sites.frame2.frame0.label1 { top frame center } \
     .sites.frame2.frame0.button0 { top frame nw } \
     .sites.frame2.frame0.button1 { top frame nw } \
     .sites.frame2.frame0.button2 { top frame nw }


  pack append .sites.frame2 \
     .sites.frame2.frame0 { top frame center }



  # ---------------------------
  # build .sites.frame3
  # ---------------------------

  frame .sites.frame3 \
    -borderwidth {2} \
    -relief {groove}


  frame .sites.frame3.frame0 \
    -borderwidth {2}

  label .sites.frame3.frame0.label0 \
    -text {upper limit}

  label .sites.frame3.frame0.label1 \
    -text {Magnitude}

  radiobutton .sites.frame3.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {up_scale}

  radiobutton .sites.frame3.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {up_scale}

  radiobutton .sites.frame3.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {up_scale}

  scale  .sites.frame3.frame0.scale \
    -sliderlength {20}

  pack append .sites.frame3.frame0 \
     .sites.frame3.frame0.label0 { top frame center fillx } \
     .sites.frame3.frame0.scale { right } \
     .sites.frame3.frame0.label1 { top frame center } \
     .sites.frame3.frame0.button0 { top frame nw } \
     .sites.frame3.frame0.button1 { top frame nw } \
     .sites.frame3.frame0.button2 { top frame nw }


  pack append .sites.frame3 \
     .sites.frame3.frame0 { top frame center }


  # ---------------------------
  # build .sites.frame4
  # ---------------------------

  frame .sites.frame4 \
    -borderwidth {2}

  frame .sites.frame4.frame
  
  button .sites.frame4.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                set cname [.sites.frame0.entry get]
                set fname [.sites.frame1.entry get]
                set sname [.sites.frame5.entry get]
                if { $down_scale != "" && $up_scale != "" } {
                   set down_scale [expr [.sites.frame3.frame0.scale get]*$down_scale]
                   set up_scale [expr [.sites.frame2.frame0.scale get]*$up_scale]
                }
                destroy .sites
              }

  button .sites.frame4.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command { 
               set cname ""
               set fname ""
               set sname ""
               set down_scale ""
               set up_scale ""
               destroy .sites
             }

  pack append .sites.frame4.frame \
    .sites.frame4.frame.ok { left expand } \
    .sites.frame4.frame.cancel { right expand }

  pack append .sites.frame4 \
    .sites.frame4.frame { bottom frame center fill }


  pack append .sites \
    .sites.frame0 { top expand fill } \
    .sites.frame1 { top expand fill } \
    .sites.frame5 { top expand fill } \
    .sites.frame4 { bottom expand fill } \
    .sites.frame2 { right } \
    .sites.frame3 { left }

  grab set .sites
  tkwait window .sites

  set cmd ""
  if { $cname != "" } {
     set cmd "$cmd -color $cname"
  }

  if { $fname != "" } {
     set cmd "$cmd -font $fname"
  }

  if { $sname != "" } {
     set cmd "$cmd -bitmap @$sname"
  }

  if { $down_scale != "" } {
     set cmd "$cmd -downscale $down_scale"
  }

  if { $up_scale != "" } {
     set cmd "$cmd -upscale $up_scale"
  }

  return $cmd
}




#***********************************************************************
#
# PROCEDURE: polygon
#
# ARGUMENTS: none 
#
# RETURNED VALUES: none
#
# DESCRIPTION :  
#
#***********************************************************************

proc polygon { } {

  global color
  set color ""

  global category
  set category ""

  global fill
  set fill ""

  global down_scale
  set down_scale ""

  global up_scale
  set up_scale ""

  toplevel .polygon

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .polygon +100+100
  wm title .polygon {polygon}


  # ---------------------------
  # build .polygon.frame0
  # ---------------------------

  frame .polygon.frame0 \
    -borderwidth {2} \
    -relief {flat}

  label .polygon.frame0.label \
    -anchor {w} \
    -text "Color"

  entry .polygon.frame0.entry \
    -relief {sunken} \
    -width 20

  button .polygon.frame0.button \
     -text "colors ..." \
     -relief raised \
     -padx 10 \
     -command {
                set color_name [getcolors]
                if { $color_name != "" } {
                   .polygon.frame0.entry delete 0 end
                   .polygon.frame0.entry insert 0 $color_name
                   set color $color_name
                }
              }
 
 
  pack append .polygon.frame0 \
    .polygon.frame0.label { top fillx } \
    .polygon.frame0.entry { left expand fill } \
    .polygon.frame0.button { right }


  bind .polygon.frame0.entry <Return> {
     set color [.polygon.frame0.entry get]
  }

  # ---------------------------
  # build .polygon.frame5
  # ---------------------------

  frame .polygon.frame5 \
    -borderwidth {2} \
    -relief {flat}

  label .polygon.frame5.label \
    -anchor {w} \
    -text "Fill"

  entry .polygon.frame5.entry \
    -relief {sunken} \
    -width 20

  button .polygon.frame5.button \
     -text "Symbol ..." \
     -relief raised \
     -padx 10 \
     -command {
                set symbol_name [dir_dialog]
                if { $symbol_name != "" } {
                   focus .polygon.frame5.entry
                   .polygon.frame5.entry delete 0 end
                   .polygon.frame5.entry insert 0 $symbol_name
                   set fill $symbol_name 
                }
              }
 
 
  pack append .polygon.frame5 \
    .polygon.frame5.label { top fillx } \
    .polygon.frame5.entry { left expand fill } \
    .polygon.frame5.button { right }


  bind .polygon.frame5.entry <Return> {
     set fill [.polygon.frame5.entry get]
  }

  # ---------------------------
  # build .polygon.frame1
  # ---------------------------

  frame .polygon.frame1 \
    -borderwidth {2} \
    -relief {flat}

  label .polygon.frame1.label \
    -anchor {w} \
    -text "Category"

  entry .polygon.frame1.entry \
    -relief {sunken} \
    -width 20

  pack append .polygon.frame1 \
    .polygon.frame1.label { top fillx } \
    .polygon.frame1.entry { left }

  bind .polygon.frame1.entry <Return> {
    set category [.polygon.frame1.entry get]
  }

  # ---------------------------
  # build .polygon.frame2
  # ---------------------------

  frame .polygon.frame2 \
    -borderwidth {2} \
    -relief {groove}


  frame .polygon.frame2.frame0 \
    -borderwidth {2}

  label .polygon.frame2.frame0.label0 \
    -text {lower limit}

  label .polygon.frame2.frame0.label1 \
    -text {Magnitude}

  radiobutton .polygon.frame2.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {down_scale}

  radiobutton .polygon.frame2.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {down_scale}

  radiobutton .polygon.frame2.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {down_scale}

  scale  .polygon.frame2.frame0.scale \
    -sliderlength {20}

  pack append .polygon.frame2.frame0 \
     .polygon.frame2.frame0.label0 { top frame center fillx } \
     .polygon.frame2.frame0.scale { right } \
     .polygon.frame2.frame0.label1 { top frame center } \
     .polygon.frame2.frame0.button0 { top frame nw } \
     .polygon.frame2.frame0.button1 { top frame nw } \
     .polygon.frame2.frame0.button2 { top frame nw }


  pack append .polygon.frame2 \
     .polygon.frame2.frame0 { top frame center }



  # ---------------------------
  # build .polygon.frame3
  # ---------------------------

  frame .polygon.frame3 \
    -borderwidth {2} \
    -relief {groove}


  frame .polygon.frame3.frame0 \
    -borderwidth {2}

  label .polygon.frame3.frame0.label0 \
    -text {upper limit}

  label .polygon.frame3.frame0.label1 \
    -text {Magnitude}

  radiobutton .polygon.frame3.frame0.button0 \
    -relief {flat} \
    -text {x 10000} \
    -value {10000} \
    -variable {up_scale}

  radiobutton .polygon.frame3.frame0.button1 \
    -relief {flat} \
    -text {x 100000} \
    -value {100000} \
    -variable {up_scale}

  radiobutton .polygon.frame3.frame0.button2 \
    -relief {flat} \
    -text {x 1000000} \
    -value {1000000} \
    -variable {up_scale}

  scale  .polygon.frame3.frame0.scale \
    -sliderlength {20}

  pack append .polygon.frame3.frame0 \
     .polygon.frame3.frame0.label0 { top frame center fillx } \
     .polygon.frame3.frame0.scale { right } \
     .polygon.frame3.frame0.label1 { top frame center } \
     .polygon.frame3.frame0.button0 { top frame nw } \
     .polygon.frame3.frame0.button1 { top frame nw } \
     .polygon.frame3.frame0.button2 { top frame nw }


  pack append .polygon.frame3 \
     .polygon.frame3.frame0 { top frame center }


  # ---------------------------
  # build .polygon.frame4
  # ---------------------------

  frame .polygon.frame4 \
    -borderwidth {2}

  frame .polygon.frame4.frame
  
  button .polygon.frame4.frame.ok \
     -text Ok \
     -relief raised \
     -padx 10 \
     -command {
                set color [.polygon.frame0.entry get]
                set fill [.polygon.frame5.entry get]
                set category [.polygon.frame1.entry get]
                if { $down_scale != "" && $up_scale != "" } {
                   set down_scale [expr [.polygon.frame3.frame0.scale get]*$down_scale]
                   set up_scale [expr [.polygon.frame2.frame0.scale get]*$up_scale]
                }
                destroy .polygon
              }

  button .polygon.frame4.frame.cancel \
    -text Cancel \
    -relief raised \
    -padx 10 \
    -command {
               set color ""
               set fill ""
               set category ""
               set down_scale ""
               set up_scale ""
               destroy .polygon
             }

  pack append .polygon.frame4.frame \
    .polygon.frame4.frame.ok { left expand } \
    .polygon.frame4.frame.cancel { right expand }

  pack append .polygon.frame4 \
    .polygon.frame4.frame { bottom frame center fill }


  pack append .polygon \
    .polygon.frame0 { top expand fill } \
    .polygon.frame1 { top expand fill } \
    .polygon.frame5 { top expand fill } \
    .polygon.frame4 { bottom expand fill } \
    .polygon.frame2 { right } \
    .polygon.frame3 { left }

  grab set .polygon
  tkwait window .polygon

  set cmd ""
  if { $color != "" } {
     set cmd "$cmd -color $color"
  }

  if { $fill != "" } {
     set cmd "$cmd -fill @$fill"
  }

  if { $category != "" } {
     set cmd "$cmd -category $category"
  }

  if { $down_scale != "" } {
     set cmd "$cmd -downscale $down_scale"
  }

  if { $up_scale != "" } {
     set cmd "$cmd -upscale $up_scale"
  }

  return $cmd
}





#***********************************************************************
#
# PROCEDURE: put_list_in_listbox
#
# ARGUMENTS: the_list : list
#            the_listbox : listbox
#
# RETURNED VALUES:
#
# DESCRIPTION:
#
#***********************************************************************

proc put_list_in_listbox { the_listbox the_list } {

  $the_listbox delete 0 end
  foreach i $the_list {
      $the_listbox insert end $i
  }

}



#***********************************************************************
#
# PROCEDURE: get_feature_list
#
# ARGUMENTS: path : search path
#
# RETURNED VALUES: feature_list : list of all feature 
#
# DESCRIPTION:
#
#***********************************************************************

proc get_feature_list { path } {

  # that's our list
  set feature_list ""

  # first, let's check if it exists,
  # if not, returns empty list
  if { [file isdirectory $path] != 1 } {
     return $feature_list
  }

  # save current directory
  set current_dir [exec pwd]

  cd $path
  foreach i [exec ls -a [exec pwd]] {
      if { [string compare $i "."] != 0 && [string compare $i ".."] != 0 } {
         lappend feature_list $i
      }
  }
  # go back to old directory  
  cd $current_dir

  return $feature_list
}




#***********************************************************************
#
# PROCEDURE: get_mapset_list
#
# ARGUMENTS: path : search path
#
# RETURNED VALUES: mapset_list : list of all mapset 
#
# DESCRIPTION:
#
#***********************************************************************

proc get_mapset_list { path } {

  # that's our list
  set mapset_list ""

  # first, let's check if it exists,
  # if not, returns empty list
  if { [file isdirectory $path] != 1 } {
     return $mapset_list
  }

  # save current directory
  set current_dir [exec pwd]

  cd $path
  foreach i [exec ls -a [exec pwd]] {
      if { [string compare $i "."] != 0 && [string compare $i ".."] != 0 && \
           [file isdirectory $i] } {
         lappend mapset_list $i
      }
  }
  # go back to old directory  
  cd $current_dir

  return $mapset_list
}




#***********************************************************************
#
# PROCEDURE: load_layer
#
# ARGUMENTS:
#
# RETURNED VALUES:
#
# DESCRIPTION:
#
#***********************************************************************

proc load_layer { view } {

  global file_name
  global file_type
  global mapset

  if { $file_name != "" } {
     case $file_type in {
          {Sites} {
                   set parameter [sites]
                   if { $parameter != "" } {
                      eval "$view addsites -name $file_name -mapset $mapset $parameter"
                   }
             
          }

          {Label} {
                   set parameter [labels]
                   if { $parameter != "" } {
                      eval "$view addlabel -name $file_name -mapset $mapset $parameter"
                   }
          }

          {Meta} {
                   $view addmeta -name $file_name -mapset $mapset
          }

          {Polyline} {
                   set parameter [polyline]
                   if { $parameter != "" } {
                      eval "$view addpolyline -name $file_name -mapset $mapset $parameter"
                   }
          }

          {Polygon} {
                   set parameter [polygon]
                   if { $parameter != "" } {
                      eval "$view addpolygon -name $file_name -mapset $mapset $parameter"
                   }
          }

          {Point} {
                   set parameter [points]
                   if { $parameter != "" } {
                      eval "$view addpoint -name $file_name -mapset $mapset $parameter"
                   }
          }

          {Rasteroverlay} {
                   set parameter [rasters]
                   if { $parameter != "" } {
                      eval "$view addoraster -name $file_name -mapset $mapset $parameter"
                   }
          }

          {Rasternonoverlay} {
                   set parameter [rasters]
                   if { $parameter != "" } {
                      eval "$view addraster -name $file_name -mapset $mapset $parameter"
                   }
          }
     }
  }
}




#***********************************************************************
#
# PROCEDURE: mapset_dialog
#
# ARGUMENTS: void
#
# RETURNED VALUES: void
#
# DESCRIPTION : 
#
#***********************************************************************

proc mapset_dialog { view } {

  global database
  global location
  global mapset

  global tcltkgrassbase

  global current_view
  set current_view $view

  global mapset_list

  # this will our return value
  global file_name
  set file_name ""

  global file_type
  set file_type ""

  global layer_type
  set layer_type ""

  toplevel .feature

  # -----------------------------
  # Window manager configurations
  # -----------------------------
  wm geometry .feature +100+100
  wm title .feature {spatial layer}
  wm group .feature .

  # ---------------------------
  # build .feature.frame0
  # ---------------------------

  frame .feature.frame0 \
    -borderwidth {2} \
    -relief {raised}

  label .feature.frame0.label \
    -anchor {w} \
    -text "Mapset : "

  entry .feature.frame0.mapset \
    -relief {sunken}

  menubutton .feature.frame0.mapsets \
    -bitmap "@$tcltkgrassbase/bitmap/arrow" \
    -menu {.feature.frame0.mapsets.pulldown}
   
  menu .feature.frame0.mapsets.pulldown

  set mapset_list [get_mapset_list "$database/$location"] 
  foreach i $mapset_list {
      .feature.frame0.mapsets.pulldown add command \
          -label $i \
          -command { 
                     set mapset [lindex $mapset_list \
                                [.feature.frame0.mapsets.pulldown index active] ] 
                     .feature.frame0.mapset delete 0 end
                     .feature.frame0.mapset insert 0 $mapset
                     .feature.frame1.type delete 0 end
                     .feature.frame2.listbox delete 0 end
                     set file_name ""
                   }
  }

  pack append .feature.frame0 \
    .feature.frame0.label { left } \
    .feature.frame0.mapset { left } \
    .feature.frame0.mapsets { right }


  # ---------------------------
  # build .feature.frame1
  # ---------------------------

  frame .feature.frame1 \
    -borderwidth {2} \
    -relief {raised}

  label .feature.frame1.label \
    -anchor {w} \
    -text "Type : "

  entry .feature.frame1.type \
    -relief {sunken}

  menubutton .feature.frame1.types \
    -bitmap "@$tcltkgrassbase/bitmap/arrow" \
    -menu {.feature.frame1.types.pulldown}
   
  menu .feature.frame1.types.pulldown
       .feature.frame1.types.pulldown add cascade \
          -label {Vector} \
          -menu .feature.frame1.types.vector    

       .feature.frame1.types.pulldown add cascade \
          -label {Raster} \
          -menu .feature.frame1.types.raster    

       .feature.frame1.types.pulldown add command \
          -label {Sites} \
          -command { set file_name ""
                     set file_type "Sites"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Sites"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/site_lists"]
                   }

       .feature.frame1.types.pulldown add command \
          -label {Label} \
          -command { set file_name ""
                     set file_type "Label"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Label"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/paint/labels"]
                   }

       .feature.frame1.types.pulldown add command \
          -label {Meta} \
          -command { set file_name ""
                     set file_type "Meta"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Meta"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/meta"]
                   }
  menu .feature.frame1.types.vector
       .feature.frame1.types.vector add command \
          -label {Polyline} \
          -command { set file_name ""
                     set file_type "Polyline"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Polyline"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/dig"]
                   }

       .feature.frame1.types.vector add command \
          -label {Polygon} \
          -command { set file_name ""
                     set file_type "Polygon"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Polygon"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/dig"]
                   }

       .feature.frame1.types.vector add command \
          -label {Point} \
          -command { set file_name ""
                     set file_type "Point"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Point"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/dig"]
                   }

  menu .feature.frame1.types.raster
       .feature.frame1.types.raster add command \
          -label {Overlay} \
          -command { set file_name ""
                     set file_type "Rasteroverlay"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Raster overlay"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/cell"]
                   }

       .feature.frame1.types.raster add command \
          -label {Non overlay} \
          -command { set file_name ""
                     set file_type "Rasternonoverlay"
                     .feature.frame1.type delete 0 end
                     .feature.frame1.type insert 0 "Raster non overlay"
                     put_list_in_listbox .feature.frame2.listbox \
                         [get_feature_list "$database/$location/$mapset/cell"]
                   }
  pack append .feature.frame1 \
    .feature.frame1.label { left } \
    .feature.frame1.type { left } \
    .feature.frame1.types { right }


  # ---------------------------
  # build .feature.frame2
  # ---------------------------

  frame .feature.frame2 \
    -borderwidth {2} \
    -relief {flat}

  listbox .feature.frame2.listbox \
    -relief {raised} \
    -yscrollcommand {.feature.frame2.vscrollbar set} \
    -xscrollcommand {.feature.frame2.hscrollbar set}

  scrollbar .feature.frame2.vscrollbar \
    -command {.feature.frame2.listbox yview} \
    -relief {raised}

  scrollbar .feature.frame2.hscrollbar \
    -command {.feature.frame2.listbox xview} \
    -relief {raised} \
    -orient {horizontal}

  pack append .feature.frame2 \
    .feature.frame2.vscrollbar { right filly } \
    .feature.frame2.hscrollbar { bottom fillx } \
    .feature.frame2.listbox { left expand fill}


  # ---------------------------
  # build .feature.frame3
  # ---------------------------

  frame .feature.frame3 \
    -borderwidth {2} \
    -relief {flat}

  button .feature.frame3.grid \
     -text {Add grid ...} \
     -relief raised \
     -padx 10 \
     -command {
                $current_view addgrid -unit 1000
                set list [$current_view list]
                .feature.frame4.listbox delete 0 end
                foreach i $list {
                    .feature.frame4.listbox insert end $i
                }
              }

  button .feature.frame3.layer \
     -text {Add layer ...} \
     -relief raised \
     -padx 10 \
     -command {
                load_layer $current_view
                set list [$current_view list]
                .feature.frame4.listbox delete 0 end
                foreach i $list {
                    .feature.frame4.listbox insert end $i
                }
              }

  pack append .feature.frame3 \
    .feature.frame3.grid { top frame center expand } \
    .feature.frame3.layer { bottom frame center expand }


  # ---------------------------
  # build .feature.frame4
  # ---------------------------

  frame .feature.frame4 \
    -borderwidth {4} \
    -relief {flat}

  listbox .feature.frame4.listbox \
    -relief {raised} \
    -geometry 30x10 \
    -yscrollcommand {.feature.frame4.vscrollbar set} \
    -xscrollcommand {.feature.frame4.hscrollbar set}

  scrollbar .feature.frame4.vscrollbar \
    -command {.feature.frame4.listbox yview} \
    -relief {raised}

  scrollbar .feature.frame4.hscrollbar \
    -command {.feature.frame4.listbox xview} \
    -relief {raised} \
    -orient {horizontal}

  pack append .feature.frame4 \
    .feature.frame4.vscrollbar { right filly } \
    .feature.frame4.hscrollbar { bottom fillx } \
    .feature.frame4.listbox { left expand fill}


  # ---------------------------
  # build .feature.frame5
  # ---------------------------

  frame .feature.frame5 \
    -borderwidth {2} \
    -relief {flat}

  button .feature.frame5.remove \
     -text {Remove} \
     -relief raised \
     -padx 10 \
     -command {
                set select [.feature.frame4.listbox curselection]
                if { $select != "" } {
                   $current_view delete $select
                   set list [$current_view list]
                   .feature.frame4.listbox delete 0 end
                   foreach i $list {
                      .feature.frame4.listbox insert end $i
                   }
                }
              }

  button .feature.frame5.up \
     -text {Up} \
     -relief raised \
     -padx 10 \
     -command { 
                set select [.feature.frame4.listbox curselection]
                if { $select != "" } {
                   $current_view up $select
                   set list [$current_view list]
                   .feature.frame4.listbox delete 0 end
                   foreach i $list {
                      .feature.frame4.listbox insert end $i
                   }
                }
              }

  pack append .feature.frame5 \
    .feature.frame5.remove { left } \
    .feature.frame5.up { left }


  # ---------------------------
  # build .feature.frame6
  # ---------------------------

  frame .feature.frame6 \
    -borderwidth {2} \
    -relief {flat}

  frame .feature.frame6.frame
  
  button .feature.frame6.frame.apply \
     -text {Apply ...} \
     -relief raised \
     -padx 10 \
     -command { 
                $current_view draw
              }

  button .feature.frame6.frame.exit \
    -text {Exit} \
    -relief raised \
    -padx 10 \
    -command { 
               destroy .feature
             }

  pack append .feature.frame6.frame \
    .feature.frame6.frame.apply { left frame center expand } \
    .feature.frame6.frame.exit { right frame center expand }

  pack append .feature.frame6 \
    .feature.frame6.frame { bottom fillx }


  pack append .feature \
    .feature.frame6 { bottom fill } \
    .feature.frame5 { bottom frame center expand fill } \
    .feature.frame4 { left fill } \
    .feature.frame3 { left fill } \
    .feature.frame0 { top fill } \
    .feature.frame1 { top fill } \
    .feature.frame2 { top fill }



  bind .feature.frame2.listbox <Button-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	set file_name [%W get [%W nearest %y]]
  }

  bind .feature.frame2.listbox <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	set file_name [%W get [%W nearest %y]]
  }

  bind .feature.frame2.listbox <Double-ButtonPress-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	set file_name [%W get [%W nearest %y]]
  }

  bind .feature.frame4.listbox <Button-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	set layer_name [%W get [%W nearest %y]]
  }

  bind .feature.frame4.listbox <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	set layer_name [%W get [%W nearest %y]]
  }

  bind .feature.frame4.listbox <Double-ButtonPress-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	set layer_name [%W get [%W nearest %y]]
  }

  # initialize widgets

  # default mapset
  .feature.frame0.mapset delete 0 end
  .feature.frame0.mapset insert 0 $mapset

  # type and listbox to nothing
  .feature.frame1.type delete 0 end
  .feature.frame2.listbox delete 0 end

                
  set list [$current_view list]
  if { $list != "" } {
     .feature.frame4.listbox delete 0 end
     foreach i $list {
        .feature.frame4.listbox insert end $i
     }
  }

#  grab set .feature
#  tkwait window .feature

}



#***********************************************************************
#
# PROCEDURE: setCurrentScale
#
# ARGUMENTS: view : current view
#            scale_entry : entry scale widget
#
# RETURNED VALUES: void
#
# DESCRIPTION:
#
#***********************************************************************

proc setCurrentScale { view scale_entry} {
   
  set curscale [$view scale]
  $scale_entry delete 0 end
  $scale_entry insert 0 $curscale

}



#***********************************************************************
#
# PROCEDURE: nameNewView
#
# ARGUMENTS:
#
# RETURNED VALUES:
#
# DESCRIPTION:
#
#***********************************************************************

proc nameNewView {} {

  global nbr_view

  incr nbr_view
  set new_name .v[expr $nbr_view]

  return $new_name
}



#***********************************************************************
#
# PROCEDURE: createView
#
# ARGUMENTS:
#
# RETURNED VALUES:
#
# DESCRIPTION:
#
#***********************************************************************

proc createView { new_name } {

  global database
  global location
  global mapset

  global tcltkgrassbase

  global wnd_name
  set wnd_name "$new_name"

  global view_wnd
  set view_wnd "$new_name.v"

  toplevel $wnd_name

  wm geometry $wnd_name 400x400
  wm geometry $wnd_name +100+100
  wm minsize $wnd_name 400 400
  wm title $wnd_name $wnd_name

  wm group $wnd_name .

  global cur_scale

  global scaleEntry 
  set scaleEntry $wnd_name.frame1.entry1


  View $view_wnd

  $view_wnd zoommode 1

  # ---------------------------
  # build $wnd_name.frame0
  # ---------------------------

  frame $wnd_name.frame0 \
    -borderwidth {2} \
    -relief {raised}


  # -----------------------------
  # build $wnd_name.frame0.frame2
  # -----------------------------

  frame $wnd_name.frame0.frame2 \
    -borderwidth {2} \
    -relief {flat}

  button $wnd_name.frame0.frame2.zoomin \
    -bitmap "@$tcltkgrassbase/bitmap/magnify_zoomin.xbm" \
    -command "$view_wnd zoom 0.5
              setCurrentScale $view_wnd $new_name.frame1.entry1"

  button $wnd_name.frame0.frame2.zoomout \
    -bitmap "@$tcltkgrassbase/bitmap/magnify_zoomout.xbm" \
    -command "$view_wnd zoom 2.0
              setCurrentScale $view_wnd $new_name.frame1.entry1"

  button $wnd_name.frame0.frame2.default \
    -bitmap "@$tcltkgrassbase/bitmap/earth.xbm" \
    -command "$view_wnd setdefregion 
              setCurrentScale $view_wnd $new_name.frame1.entry1"

  button $wnd_name.frame0.frame2.load \
    -bitmap "@$tcltkgrassbase/bitmap/layer.xbm" \
    -command "mapset_dialog $view_wnd
              focus $view_wnd"

  pack append $wnd_name.frame0.frame2 \
    $wnd_name.frame0.frame2.load {top frame center} \
    $wnd_name.frame0.frame2.default {top frame center} \
    $wnd_name.frame0.frame2.zoomin {top frame center} \
    $wnd_name.frame0.frame2.zoomout {top frame center}


  # -----------------------------
  # build $wnd_name.frame0.frame3
  # -----------------------------

  frame $wnd_name.frame0.frame3 \
    -borderwidth {2} \
    -relief {flat}

  button $wnd_name.frame0.frame3.down \
    -bitmap "@$tcltkgrassbase/bitmap/arrow_down.xbm" \
    -command "$view_wnd pany -0.5"

  button $wnd_name.frame0.frame3.left \
    -bitmap "@$tcltkgrassbase/bitmap/arrow_left.xbm" \
    -command "$view_wnd panx -0.5"

  button $wnd_name.frame0.frame3.right \
    -bitmap "@$tcltkgrassbase/bitmap/arrow_right.xbm" \
    -command "$view_wnd panx 0.5"

  button $wnd_name.frame0.frame3.up \
    -bitmap "@$tcltkgrassbase/bitmap/arrow_up.xbm" \
    -command "$view_wnd pany 0.5"

  pack append $wnd_name.frame0.frame3 \
    $wnd_name.frame0.frame3.up {top frame n} \
    $wnd_name.frame0.frame3.down {bottom frame s} \
    $wnd_name.frame0.frame3.left {left frame w} \
    $wnd_name.frame0.frame3.right {right frame e padx 20}

  pack append $wnd_name.frame0 \
    $wnd_name.frame0.frame2 {top frame center pady 19} \
    $wnd_name.frame0.frame3 {bottom frame center}


  # ----------------------
  # build $wnd_name.frame1
  # ----------------------

  frame $wnd_name.frame1 \
    -borderwidth {2} \
    -relief {raised}

  label $wnd_name.frame1.label1 \
    -anchor {w} \
    -relief {sunken}

  entry $wnd_name.frame1.entry1 \
    -relief {sunken}

  pack append $wnd_name.frame1 \
    $wnd_name.frame1.label1 {left expand fill} \
    $wnd_name.frame1.entry1 {right fill}


  pack append $wnd_name \
    $wnd_name.frame0 {left frame center filly } \
    $view_wnd {top frame center expand fill} \
    $wnd_name.frame1 {bottom frame s fill }

  bind $wnd_name.frame1.entry1 <Return> "
        set cur_scale \[%W get\]
        %W delete 0 end
        %W insert 0 \[\"$view_wnd\" scale \$cur_scale\]
  "

  bind $wnd_name.frame1.entry1 <Configure> "
      setCurrentScale \"$view_wnd\" %W
  "

  bind $view_wnd <ButtonPress> "
      setCurrentScale %W \"$scaleEntry\"
  "
  bind $view_wnd <ButtonRelease> "
      setCurrentScale %W \"$scaleEntry\"
  "


}





#***********************************************************************
#
# PROCEDURE: mainWindow
#
# ARGUMENTS:
#
# RETURNED VALUES:
#
# DESCRIPTION:
#
#***********************************************************************

proc mainWindow { mainWindowTitle } {

  # Window manager configurations

  wm geometry . +10+10
  wm title . $mainWindowTitle

  # ---------------------------
  # build .frame0
  # ---------------------------
  frame .frame0 \
    -borderwidth {2} \
    -relief {raised}


  menubutton .frame0.display \
    -text { Display } \
    -menu {.frame0.display.pulldown}
   
  menu .frame0.display.pulldown
       .frame0.display.pulldown add command \
          -label { Open a Super View } \
          -command { 
                     set viewname [nameNewView]
                     createView $viewname
                   }

       .frame0.display.pulldown add cascade \
          -label { Manage } \
          -menu .frame0.display.pulldown.manage

       .frame0.display.pulldown add cascade \
          -label { Display } \
          -menu .frame0.display.pulldown.display

  menu .frame0.display.pulldown.manage
       .frame0.display.pulldown.manage add command \
          -label { Control Display Monitor } \
          -command { source $tcltkgrassbase/module/d.mon }

       .frame0.display.pulldown.manage add command \
          -label { Erase Display Frame } \
          -command { source $tcltkgrassbase/module/d.erase }   

       .frame0.display.pulldown.manage add command \
          -label { Manage Display Frames } \
          -command { source $tcltkgrassbase/module/d.frame } 

       .frame0.display.pulldown.manage add command \
          -label { Establish Color Table } \
          -command { source $tcltkgrassbase/module/d.colormode } 


  menu .frame0.display.pulldown.display
       .frame0.display.pulldown.display add cascade \
          -label { Raster } \
          -menu {.frame0.display.pulldown.display.raster}

       .frame0.display.pulldown.display add cascade \
          -label { Vector } \
          -menu {.frame0.display.pulldown.display.vector}

       .frame0.display.pulldown.display add cascade \
          -label { Sites } \
          -menu {.frame0.display.pulldown.display.sites}

       .frame0.display.pulldown.display add cascade \
          -label { Text } \
          -menu {.frame0.display.pulldown.display.text}

       .frame0.display.pulldown.display add cascade \
          -label { Graphics } \
          -menu {.frame0.display.pulldown.display.graphics}

  menu .frame0.display.pulldown.display.raster
       .frame0.display.pulldown.display.raster add command \
          -label { Display Raster Maps } \
          -command { source $tcltkgrassbase/module/d.rast }

       .frame0.display.pulldown.display.raster add command \
          -label { Display HIS Values } \
          -command { source $tcltkgrassbase/module/d.his }

       .frame0.display.pulldown.display.raster add command \
          -label { Display Red, Green, Blue Overlays } \
          -command { source $tcltkgrassbase/module/d.rgb } 

       .frame0.display.pulldown.display.raster add command \
          -label { Display 3-Dimensional Images } \
          -command { source $tcltkgrassbase/module/d.3d }


  menu .frame0.display.pulldown.display.vector
       .frame0.display.pulldown.display.vector add command \
          -label { Display Vector Maps } \
          -command { source $tcltkgrassbase/module/d.vect }

       .frame0.display.pulldown.display.vector add command \
          -label { Display USGS DLG-3 files } \
	  -command { source $tcltkgrassbase/module/d.vect.dlg } 


  menu .frame0.display.pulldown.display.sites
       .frame0.display.pulldown.display.sites add command \
          -label { Display Sites Markers } \
          -command { source $tcltkgrassbase/module/d.sites } 

       .frame0.display.pulldown.display.sites add command \
          -label { Display Points Graphics } \
          -command { source $tcltkgrassbase/module/d.points } 

       .frame0.display.pulldown.display.sites add command \
          -label { Display Points As Icons } \
          -command { source $tcltkgrassbase/module/d.icons }

  menu .frame0.display.pulldown.display.text
       .frame0.display.pulldown.display.text add command \
          -label { Display Map Title } \
          -command { source $tcltkgrassbase/module/d.title }

       .frame0.display.pulldown.display.text add command \
          -label { Display Legend } \
          -command { source $tcltkgrassbase/module/d.legend }

       .frame0.display.pulldown.display.text add command \
          -label { Display Text Labels } \
          -command { source $tcltkgrassbase/module/d.label }  

       .frame0.display.pulldown.display.text add command \
          -label { Display Text Labels For Paint Output } \
          -command { source $tcltkgrassbase/module/d.paint.labels } 

       .frame0.display.pulldown.display.text add command \
          -label { Select Text Font } \
          -command { source $tcltkgrassbase/module/d.font } 

       .frame0.display.pulldown.display.text add command \
          -label { Draw Text } \
          -command { source $tcltkgrassbase/module/d.text } 

  menu .frame0.display.pulldown.display.graphics
       .frame0.display.pulldown.display.graphics add command \
          -label { Display Color Table } \
          -command { source $tcltkgrassbase/module/d.colortable } 

       .frame0.display.pulldown.display.graphics add command \
          -label { Display Geodesic Line } \
          -command { source $tcltkgrassbase/module/d.geodesic } 

       .frame0.display.pulldown.display.graphics add command \
          -label { Display Rhumbline } \
          -command { source $tcltkgrassbase/module/d.rhumbline }

       .frame0.display.pulldown.display.graphics add command \
          -label { Overlay Bar Scale and North Arrow } \
          -command { source $tcltkgrassbase/module/d.scale } 

       .frame0.display.pulldown.display.graphics add command \
          -label { Overlay Grid } \
          -command { source $tcltkgrassbase/module/d.grid } 
       
       .frame0.display.pulldown.display.graphics add command \
          -label { Display Histogram } \
          -command { source $tcltkgrassbase/module/d.histogram } 

       .frame0.display.pulldown.display.graphics add command \
          -label { Display Legend } \
          -command { source $tcltkgrassbase/module/d.legend } 


  menubutton .frame0.mapmanagement \
    -text { MapManagement} \
    -menu {.frame0.mapmanagement.pulldown}

  menu .frame0.mapmanagement.pulldown
       .frame0.mapmanagement.pulldown add command \
          -label { List } \
          -command { 
                     source $tcltkgrassbase/module/g.list
                   }
       .frame0.mapmanagement.pulldown add cascade \
          -label { Copy } \
          -menu .frame0.mapmanagement.pulldown.copy
       .frame0.mapmanagement.pulldown add cascade \
          -label { Rename } \
          -menu .frame0.mapmanagement.pulldown.rename
       .frame0.mapmanagement.pulldown add cascade \
          -label { Remove } \
          -menu .frame0.mapmanagement.pulldown.remove
       .frame0.mapmanagement.pulldown add command \
          -label { Create Imagery Group } \
          -command { exec xterm -title i.group -exec i.group }

  menu .frame0.mapmanagement.pulldown.copy
       .frame0.mapmanagement.pulldown.copy add command \
          -label { Raster } \
          -command { 
                     source $tcltkgrassbase/module/g.copyrast
                   }       
       .frame0.mapmanagement.pulldown.copy add command \
          -label { Vector } \
          -command { 
                     source $tcltkgrassbase/module/g.copyvect
                   }       
       .frame0.mapmanagement.pulldown.copy add command \
          -label { Sites } \
          -command { 
                     source $tcltkgrassbase/module/g.copysites
                   }       
       .frame0.mapmanagement.pulldown.copy add command \
          -label { Region } \
          -command { 
                     source $tcltkgrassbase/module/g.copyregion
                   }       
       .frame0.mapmanagement.pulldown.copy add command \
          -label { Labels } \
          -command { 
                     source $tcltkgrassbase/module/g.copylabels
                   }       
       .frame0.mapmanagement.pulldown.copy add command \
          -label { Icons } \
          -command { 
                     source $tcltkgrassbase/module/g.copyicon
                   }       
       .frame0.mapmanagement.pulldown.copy add command \
          -label { Group } \
          -command { 
                     source $tcltkgrassbase/module/g.copygroup
                   }       

  menu .frame0.mapmanagement.pulldown.rename
       .frame0.mapmanagement.pulldown.rename add command \
          -label { Raster } \
          -command { 
                     source $tcltkgrassbase/module/g.renamerast
                   }       
       .frame0.mapmanagement.pulldown.rename add command \
          -label { Vector } \
          -command { 
                     source $tcltkgrassbase/module/g.renamevect
                   }       
       .frame0.mapmanagement.pulldown.rename add command \
          -label { Sites } \
          -command { 
                     source $tcltkgrassbase/module/g.renamesites
                   }       
       .frame0.mapmanagement.pulldown.rename add command \
          -label { Region } \
          -command { 
                     source $tcltkgrassbase/module/g.renameregion
                   }       
       .frame0.mapmanagement.pulldown.rename add command \
          -label { Labels } \
          -command { 
                     source $tcltkgrassbase/module/g.renamelabels
                   }       
       .frame0.mapmanagement.pulldown.rename add command \
          -label { Icons } \
          -command { 
                     source $tcltkgrassbase/module/g.renameicon
                   }       
       .frame0.mapmanagement.pulldown.rename add command \
          -label { Group } \
          -command { 
                     source $tcltkgrassbase/module/g.renamegroup
                   }       

  menu .frame0.mapmanagement.pulldown.remove
       .frame0.mapmanagement.pulldown.remove add command \
          -label { Raster } \
          -command { 
                     source $tcltkgrassbase/module/g.removerast
                   }       
       .frame0.mapmanagement.pulldown.remove add command \
          -label { Vector } \
          -command { 
                     source $tcltkgrassbase/module/g.removevect
                   }       
       .frame0.mapmanagement.pulldown.remove add command \
          -label { Sites } \
          -command { 
                     source $tcltkgrassbase/module/g.removesites
                   }       
       .frame0.mapmanagement.pulldown.remove add command \
          -label { Region } \
          -command { 
                     source $tcltkgrassbase/module/g.removeregion
                   }       
       .frame0.mapmanagement.pulldown.remove add command \
          -label { Labels } \
          -command { 
                     source $tcltkgrassbase/module/g.removelabels
                   }       
       .frame0.mapmanagement.pulldown.remove add command \
          -label { Icons } \
          -command { 
                     source $tcltkgrassbase/module/g.removeicon
                   }       
       .frame0.mapmanagement.pulldown.remove add command \
          -label { Group } \
          -command { 
                     source $tcltkgrassbase/module/g.removegroup
                   }       

  menubutton .frame0.raster \
    -text { Raster } \
    -menu .frame0.raster.pulldown 

  menu .frame0.raster.pulldown 
       .frame0.raster.pulldown add cascade \
           -label { Analyze } \
           -menu .frame0.raster.pulldown.analyze  
       .frame0.raster.pulldown add cascade \
           -label { Convert } \
           -menu .frame0.raster.pulldown.convert  
       .frame0.raster.pulldown add cascade \
           -label { Develop } \
           -menu .frame0.raster.pulldown.develop 
       .frame0.raster.pulldown add cascade \
           -label { Report } \
           -menu .frame0.raster.pulldown.report 
 
  menu .frame0.raster.pulldown.analyze
       .frame0.raster.pulldown.analyze add command \
           -label { Query } \
           -command { source $tcltkgrassbase/module/r.what }
       .frame0.raster.pulldown.analyze add cascade \
           -label { Overlay } \
           -menu .frame0.raster.pulldown.analyze.overlay 
       .frame0.raster.pulldown.analyze add cascade \
           -label { Neighborhood } \
           -menu .frame0.raster.pulldown.analyze.neigh 
       .frame0.raster.pulldown.analyze add cascade \
           -label { Terrain } \
           -menu .frame0.raster.pulldown.analyze.terrain 
       .frame0.raster.pulldown.analyze add cascade \
           -label { Imagery } \
           -menu .frame0.raster.pulldown.analyze.imagery 

  menu .frame0.raster.pulldown.analyze.overlay
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Specify mask } \
            -command { exec xterm -title r.mask -exec r.mask }
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Cross product } \
            -command { source $tcltkgrassbase/module/r.cross  }
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Patch } \
            -command { source $tcltkgrassbase/module/r.patch }
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Inference Engine } \
            -command { source $tcltkgrassbase/module/r.infer  }
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Bayesian expert system } \
            -command { source $tcltkgrassbase/module/r.binfer  }
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Map calculator } \
            -command { exec xterm -title r.mapcalc -exec r.mapcalc  } \
            -state disabled
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Logical operation } \
            -command { exec xterm -title r.combine -exec r.combine  }
       .frame0.raster.pulldown.analyze.overlay add command \
            -label { Weighting } \
            -command { exec xterm -title r.weight -exec r.weight  }

  menu .frame0.raster.pulldown.analyze.neigh
       .frame0.raster.pulldown.analyze.neigh add command \
            -label { Neighborhood analysis } \
            -command { source $tcltkgrassbase/module/r.neighbors }
       .frame0.raster.pulldown.analyze.neigh add command \
            -label { Buffer zone } \
            -command { exec xterm -title r.buffer -exec r.buffer }
       .frame0.raster.pulldown.analyze.neigh add command \
            -label { Grow areas } \
            -command { source $tcltkgrassbase/module/r.grow }
       .frame0.raster.pulldown.analyze.neigh add command \
            -label { Thin linear features  } \
            -command { source $tcltkgrassbase/module/r.thin }


  menu .frame0.raster.pulldown.analyze.terrain
       .frame0.raster.pulldown.analyze.terrain add command \
            -label { Watershed subbasins } \
            -command { source $tcltkgrassbase/module/r.basins.fill }
       .frame0.raster.pulldown.analyze.terrain add command \
            -label { Watershed maps } \
            -command { source $tcltkgrassbase/module/r.watershed }
       .frame0.raster.pulldown.analyze.terrain add command \
            -label { Cost between 2 locations } \
            -command { source $tcltkgrassbase/module/r.cost }
       .frame0.raster.pulldown.analyze.terrain add command \
            -label { Trace flow } \
            -command { source $tcltkgrassbase/module/r.drain }
       .frame0.raster.pulldown.analyze.terrain add command \
            -label { Slope and aspect } \
            -command { source $tcltkgrassbase/module/r.slope.aspect }
       .frame0.raster.pulldown.analyze.terrain add command \
            -label { Line of sight } \
            -command { source $tcltkgrassbase/module/r.los }

  menu .frame0.raster.pulldown.analyze.imagery
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Canonical component }  \
            -command { source $tcltkgrassbase/module/i.cca }
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Principal component }  \
            -command { source $tcltkgrassbase/module/i.pca }
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Zero edge crossing detection }  \
            -command { source $tcltkgrassbase/module/i.zc }
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Fast Fourier Transform }  \
            -command { source $tcltkgrassbase/module/i.fft }
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Inverse Fast Fourier Transform }  \
            -command { source $tcltkgrassbase/module/i.ifft }
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { User defined matrix filter }  \
            -command { source $tcltkgrassbase/module/r.mfilter }
       .frame0.raster.pulldown.analyze.imagery add separator 
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Unsupervised classification }  \
            -command { exec xterm -title i.class -exec i.class } 
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Generate spectral signature }  \
            -command { source $tcltkgrassbase/module/i.gensig } \
            -state disabled
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Maximum likelyhood }  \
            -command { source $tcltkgrassbase/module/i.maxlik } \
            -state disabled
       .frame0.raster.pulldown.analyze.imagery add command \
            -label { Cluster }  \
            -command { source $tcltkgrassbase/module/i.cluster } \
            -state disabled

  menu .frame0.raster.pulldown.convert
       .frame0.raster.pulldown.convert add command \
            -label { to lines from a thinned raster } \
            -command { source $tcltkgrassbase/module/r.line }
       .frame0.raster.pulldown.convert add command \
            -label { to polygons } \
            -command { source $tcltkgrassbase/module/r.poly }
       .frame0.raster.pulldown.convert add command \
            -label { to contour lines } \
            -command { source $tcltkgrassbase/module/r.contour }

  menu .frame0.raster.pulldown.develop
       .frame0.raster.pulldown.develop add command \
            -label { Reclassify } \
            -command { source $tcltkgrassbase/module/r.reclass }
       .frame0.raster.pulldown.develop add command \
            -label { Clump } \
            -command { source $tcltkgrassbase/module/r.clump }
       .frame0.raster.pulldown.develop add command \
            -label { Resample } \
            -command { source $tcltkgrassbase/module/r.resample }
       .frame0.raster.pulldown.develop add command \
            -label { Rescale } \
            -command { source $tcltkgrassbase/module/r.rescale }
       .frame0.raster.pulldown.develop add command \
            -label { Average } \
            -command { source $tcltkgrassbase/module/r.average }
       .frame0.raster.pulldown.develop add command \
            -label { Compress/decompress } \
            -command { source $tcltkgrassbase/module/r.compress }
       .frame0.raster.pulldown.develop add command \
            -label { Randomly located sites } \
            -command { source $tcltkgrassbase/module/r.random }
       .frame0.raster.pulldown.develop add separator 
       .frame0.raster.pulldown.develop add command \
            -label { Create color table } \
            -command { source $tcltkgrassbase/module/r.colors }
       .frame0.raster.pulldown.develop add command \
            -label { Modify color table } \
            -command { exec xterm -title d.color -exec d.colors }
       .frame0.raster.pulldown.develop add separator 
       .frame0.raster.pulldown.develop add command \
            -label { Digitize } \
            -command { exec xterm -title d.color -exec r.digit }
       .frame0.raster.pulldown.develop add separator 
       .frame0.raster.pulldown.develop add command \
            -label { Interpolate using IDW } \
            -command { source $tcltkgrassbase/module/r.surf.idw }
       .frame0.raster.pulldown.develop add command \
            -label { Interpolate from contours } \
            -command { source $tcltkgrassbase/module/r.surf.contour }

  menu .frame0.raster.pulldown.report
       .frame0.raster.pulldown.report add command \
            -label { Area } \
            -command { source $tcltkgrassbase/module/r.report }
       .frame0.raster.pulldown.report add command \
            -label { Volume } \
            -command { source $tcltkgrassbase/module/r.volume }
       .frame0.raster.pulldown.report add command \
            -label { Mutual category occurences } \
            -command { source $tcltkgrassbase/module/r.coin }
       .frame0.raster.pulldown.report add command \
            -label { Values on transect line } \
            -command { source $tcltkgrassbase/module/r.transect }
       .frame0.raster.pulldown.report add command \
            -label { Covariance/correlation } \
            -command { source $tcltkgrassbase/module/r.covar }
       .frame0.raster.pulldown.report add separator 
       .frame0.raster.pulldown.report add command \
            -label { General statistics } \
            -command { source $tcltkgrassbase/module/r.stats }
       .frame0.raster.pulldown.report add command \
            -label { Basic information } \
            -command { source $tcltkgrassbase/module/r.info }
       .frame0.raster.pulldown.report add command \
            -label { Category labels and values } \
            -command { source $tcltkgrassbase/module/r.cats }
       .frame0.raster.pulldown.report add command \
            -label { Category range } \
            -command { source $tcltkgrassbase/module/r.describe }


  menubutton .frame0.region \
    -text { Region } \
    -menu .frame0.region.pulldown

   menu .frame0.region.pulldown
        .frame0.region.pulldown add command \
           -label { Select default region } \
           -command { exec g.region -d }
        .frame0.region.pulldown add command \
           -label { Zoom in the display monitor } \
           -command { exec xterm -title d.zoom -geometry 50x6 -exec d.zoom } 
        .frame0.region.pulldown add command \
           -label { Manage region interactively } \
           -command { exec xterm -title g.region -exec g.region } 

  menubutton .frame0.vector \
    -text { Vector } \
    -menu .frame0.vector.pulldown

  menu .frame0.vector.pulldown
       .frame0.vector.pulldown add command  \
            -label { Query } \
            -state disabled  
       .frame0.vector.pulldown add cascade  \
            -label { Convert } \
            -menu .frame0.vector.pulldown.convert 
       .frame0.vector.pulldown add cascade  \
            -label { Develop } \
            -menu .frame0.vector.pulldown.develop 
       .frame0.vector.pulldown add cascade  \
            -label { Report } \
            -menu .frame0.vector.pulldown.report 

  menu .frame0.vector.pulldown.convert
       .frame0.vector.pulldown.convert add command \
             -label { To raster } \
             -command { source $tcltkgrassbase/module/v.to.rast }
       .frame0.vector.pulldown.convert add command \
             -label { To sites } \
             -command { source $tcltkgrassbase/module/v.to.sites }

  menu .frame0.vector.pulldown.develop
       .frame0.vector.pulldown.develop add command \
             -label { Create topology } \
             -command { exec xterm -title v.digit -exec v.support } 
       .frame0.vector.pulldown.develop add command \
             -label { Prune } \
             -command { source $tcltkgrassbase/module/v.prune } \
             -state disabled
       .frame0.vector.pulldown.develop add command \
             -label { Spaghetti to topology } \
             -command { source $tcltkgrassbase/module/v.spag } \
             -state disabled
       .frame0.vector.pulldown.develop add command \
             -label { Clean dead lines } \
             -command { source $tcltkgrassbase/module/v.clean } \
             -state disabled
       .frame0.vector.pulldown.develop add command \
             -label { Trim small spurs } \
             -command { source $tcltkgrassbase/module/v.trim } \
             -state disabled
       .frame0.vector.pulldown.develop add command \
             -label { Change projection } \
             -command { source $tcltkgrassbase/module/v.proj }  \
             -state disabled
       .frame0.vector.pulldown.develop add command \
             -label { Coordinate tranformation } \
             -command { source $tcltkgrassbase/module/v.transform } \
             -state disabled
       .frame0.vector.pulldown.develop add separator 
       .frame0.vector.pulldown.develop add command \
             -label { Digitize } \
             -command { exec xterm -title v.digit -exec v.digit } 
       .frame0.vector.pulldown.develop add command \
             -label { Cut } \
             -command { source $tcltkgrassbase/module/v.cutter } \
             -state disabled
       .frame0.vector.pulldown.develop add command \
             -label { Create a grid } \
             -command { source $tcltkgrassbase/module/v.mkgrid } \
             -state disabled
       .frame0.vector.pulldown.develop add command \
             -label { Patch } \
             -command { source $tcltkgrassbase/module/v.patch } \
             -state disabled
  
  menu .frame0.vector.pulldown.report
       .frame0.vector.pulldown.report add command \
             -label { General information } \
             -command { source $tcltkgrassbase/module/v.stats } \
             -state disabled
       .frame0.vector.pulldown.report add command \
             -label { Area statistics } \
             -command { source $tcltkgrassbase/module/v.report } \
             -state disabled

  menubutton .frame0.paint \
    -text { Paint } \
    -state disabled   

  menubutton .frame0.import \
    -text { Import }   \
    -state disabled   

  menubutton .frame0.export \
    -text { Export }  \
    -state disabled   

  menubutton .frame0.quit \
    -text { Quit } \
    -menu {.frame0.quit.quit}

  menu .frame0.quit.quit
       .frame0.quit.quit add command \
          -label { Quit tcltkgrass } \
          -command { destroy . }

  pack append .frame0 \
    .frame0.display {left frame center} \
    .frame0.region {left frame center } \
    .frame0.mapmanagement {left frame center} \
    .frame0.raster {left frame center} \
    .frame0.vector {left frame center} \
    .frame0.paint {left frame center} \
    .frame0.import {left frame center} \
    .frame0.export {left frame center} \
    .frame0.quit {left frame center}
  
  pack append . \
    .frame0 { left expand fill }


} 


global nbr_view
set nbr_view 0

global current_entry
set current_entry ""

global file_selected
global file_name
global file_type

global tcl_prompt1
global tcl_prompt2

global database
global location
global mapset
set database $env(GISDBASE)
set location $env(LOCATION_NAME)
set mapset $env(MAPSET)

global tcltkgrassbase
set tcltkgrassbase $env(TCLTKGRASSBASE)

source $tcltkgrassbase/script/color.tcl
source $tcltkgrassbase/script/font.tcl
source $tcltkgrassbase/script/editor.tcl

set tcl_prompt1 "puts -nonewline stdout \"\n\rMAPSET <$mapset> in location <$location>\n\rGRASS-GRID > \" "
set tcl_prompt2 "puts -nonewline stdout \"\n\rMAPSET <$mapset> in location <$location>\n\rGRASS-GRID > \" "

mainWindow "tcltkgrass BETA \[DB : $database LOCATION : $location MAPSET : $mapset\]"




