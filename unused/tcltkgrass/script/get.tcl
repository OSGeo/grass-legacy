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

  pack append .mapset.frame0 \
    .mapset.frame0.label { left } \
    .mapset.frame0.mapset { left } 

  frame .mapset.frame1 \
    -borderwidth {2} \
    -relief {raised}

  listbox .mapset.frame1.listbox \
    -relief {sunken} \
    -width 20 -height 10 \
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

