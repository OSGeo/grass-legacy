#***********************************************************************
#
# Program : convert.tcl
# 
# Description : Convert xclip script to tcl/tk script
#
# !!! Dont`t sell this program, please
#
#***********************************************************************


#***********************************************************************
#
# PROCEDURE: get_feature
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc get_feature { fp } {

  puts $fp ""
  puts $fp ""
  puts $fp "proc get_feature \{ type \} \{"
  puts $fp ""
  puts $fp "  set dir \"\""
  puts $fp "  case \$type in \{"
  puts $fp ""
  puts $fp "       \{raster\} \{"
  puts $fp "                      set dir \"cell\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{vector\} \{"
  puts $fp "                      set dir \"dig\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{sites\} \{"
  puts $fp "                      set dir \"site_lists\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{label\} \{"
  puts $fp "                      set dir \"paint/labels\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{group\} \{"
  puts $fp "                      set dir \"group\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{icon\} \{"
  puts $fp "                      set dir \"paint/icons\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{region\} \{"
  puts $fp "                      set dir \"windows\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{dlg\} \{"
  puts $fp "                      set dir \"dlg\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "       \{dlg_ascii\} \{"
  puts $fp "                      set dir \"dlg_ascii\""
  puts $fp "                  \}"
  puts $fp ""
  puts $fp "  \}"
  puts $fp "  return \$dir"
  puts $fp ""
  puts $fp "\}"
  puts $fp ""
  puts $fp ""
}




#***********************************************************************
#
# PROCEDURE: put_list
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_list { fp } {

  puts $fp ""
  puts $fp ""
  puts $fp "proc put_list \{ the_listbox the_list \} \{"
  puts $fp ""
  puts $fp "  \$the_listbox delete 0 end"
  puts $fp "  foreach i \$the_list \{"
  puts $fp "      \$the_listbox insert end \$i"
  puts $fp "  \}"
  puts $fp "\}"
  puts $fp ""
  puts $fp ""
}





#***********************************************************************
#
# PROCEDURE: get_list
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc get_list { fp } {

  puts $fp ""
  puts $fp ""
  puts $fp "proc get_list \{ path \} \{"

  puts $fp "  set list \"\""

  puts $fp "  if \{ \[file isdirectory \$path\] != 1 \} \{"
  puts $fp "     return \$list"
  puts $fp "  \}"

  puts $fp "  set current_dir \[exec pwd\]"

  puts $fp "  cd \$path"
  puts $fp "  foreach i \[exec ls -a \[exec pwd\]\] \{"
  puts $fp "     if \{ \[string compare \$i \".\"\] != 0 && \[string compare \$i \"..\"\] != 0 \} \{"
  puts $fp "        lappend list \$i"
  puts $fp "     \}"
  puts $fp "  \}"
  puts $fp "  cd \$current_dir"

  puts $fp "  return \$list"
  puts $fp "\}"
  puts $fp ""
  puts $fp ""

}





#***********************************************************************
#
# PROCEDURE: mapset_listbox_widget 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc mapset_listbox_widget { fp } {


  puts $fp ""
  puts $fp ""
  puts $fp "proc mapset_listbox \{ type \} \{"

  puts $fp ""
  puts $fp "  global database"
  puts $fp "  global location"
  puts $fp "  global mapset"
  puts $fp "  global feature"

  puts $fp ""
  puts $fp "  global file_name"
  puts $fp "  set file_name \"\""

  puts $fp ""
  puts $fp "  toplevel .mapset"

  puts $fp ""
  puts $fp "  wm geometry .mapset +100+100"
  puts $fp "  wm title .mapset \{spatial layer\}"

  puts $fp ""
  puts $fp "  set feature \[get_feature \$type\]"
  puts $fp ""
  puts $fp "  global mapset_list"

  puts $fp ""
  puts $fp "  frame .mapset.frame0 \\"
  puts $fp "    -borderwidth \{2\} \\"
  puts $fp "    -relief \{flat\}"

  puts $fp ""
  puts $fp "  label .mapset.frame0.label \\"
  puts $fp "    -anchor \{w\} \\"
  puts $fp "    -text \"Mapset\""

  puts $fp ""
  puts $fp "  entry .mapset.frame0.mapset \\"
  puts $fp "    -relief \{sunken\} \\"
  puts $fp "   -width 20"

  puts $fp ""
  puts $fp "  menubutton .mapset.frame0.mapsets \\"
  puts $fp "    -bitmap \{@../bitmap/arrow\} \\"
  puts $fp "    -menu \{.mapset.frame0.mapsets.pulldown\}"
   
  puts $fp ""
  puts $fp "  menu .mapset.frame0.mapsets.pulldown"

  puts $fp ""
  puts $fp "  set mapset_list \[get_list \"\$database/\$location\"\]" 
  puts $fp "  foreach i \$mapset_list \{"
  puts $fp "      .mapset.frame0.mapsets.pulldown add command \\"
  puts $fp "          -label \$i \\"
  puts $fp "          -command \{" 
  puts $fp "                     set mapset \[lindex \$mapset_list \\"
  puts $fp "                               \[.mapset.frame0.mapsets.pulldown index active\] \]" 
  puts $fp "                     .mapset.frame0.mapset delete 0 end"
  puts $fp "                     .mapset.frame0.mapset insert 0 \$mapset"
  puts $fp "                     put_list .mapset.frame1.listbox \\"
  puts $fp "                         \[get_list \"\$database/\$location/\$mapset/\$feature\"\]"
  puts $fp "                     set file_name \"\""
  puts $fp "                   \}"
  puts $fp "  \}"

  puts $fp ""
  puts $fp "  pack append .mapset.frame0 \\"
  puts $fp "    .mapset.frame0.label \{ left \} \\"
  puts $fp "    .mapset.frame0.mapset \{ left \} \\"
  puts $fp "    .mapset.frame0.mapsets \{ right \}"


  puts $fp ""
  puts $fp "  frame .mapset.frame1 \\"
  puts $fp "    -borderwidth \{2\} \\"
  puts $fp "    -relief \{raised\}"

  puts $fp ""
  puts $fp "  listbox .mapset.frame1.listbox \\"
  puts $fp "    -relief \{sunken\} \\"
  puts $fp "    -geometry 20x10 \\"
  puts $fp "    -yscrollcommand \{.mapset.frame1.vscrollbar set\}"

  puts $fp ""
  puts $fp "  scrollbar .mapset.frame1.vscrollbar \\"
  puts $fp "    -command \{.mapset.frame1.listbox yview\}"

  puts $fp ""
  puts $fp "  pack append .mapset.frame1 \\"
  puts $fp "    .mapset.frame1.listbox \{ left expand fill \} \\"
  puts $fp "    .mapset.frame1.vscrollbar \{ right fill \}"


  puts $fp ""
  puts $fp "  frame .mapset.frame2 \\"
  puts $fp "    -borderwidth \{2\}"

  puts $fp ""
  puts $fp "  frame .mapset.frame2.frame"
  
  puts $fp ""
  puts $fp "  button .mapset.frame2.frame.ok \\"
  puts $fp "     -text Ok \\"
  puts $fp "     -relief raised \\"
  puts $fp "     -padx 10 \\"
  puts $fp "     -command \{ if \{ \$file_name != \"\" \} \{"
  puts $fp "                   destroy .mapset"
  puts $fp "               \}"
  puts $fp "              \}"

  puts $fp ""
  puts $fp "  button .mapset.frame2.frame.cancel \\"
  puts $fp "    -text Cancel \\"
  puts $fp "    -relief raised \\"
  puts $fp "    -padx 10 \\"
  puts $fp "    -command \{ set file_name \"\""
  puts $fp "               destroy .mapset"
  puts $fp "             \}"

  puts $fp ""
  puts $fp "  pack append .mapset.frame2.frame \\"
  puts $fp "    .mapset.frame2.frame.ok \{ left expand \} \\"
  puts $fp "    .mapset.frame2.frame.cancel \{ right expand \}"

  puts $fp ""
  puts $fp "  pack append .mapset.frame2 \\"
  puts $fp "    .mapset.frame2.frame \{ bottom frame center fill \}"


  puts $fp ""
  puts $fp "  pack append .mapset \\"
  puts $fp "    .mapset.frame0 \{ top expand fill \} \\"
  puts $fp "    .mapset.frame1 \{ top expand fill \} \\"
  puts $fp "    .mapset.frame2 \{ bottom expand fill \}"


  puts $fp ""
  puts $fp "  bind .mapset.frame1.listbox <Button-1> \{"
  puts $fp "        %W select from \[%W nearest %y\]"
  puts $fp "        %W select to \[%W nearest %y\]"
  puts $fp "	    set file_name \[%W get \[%W nearest %y\]\]"
  puts $fp "  \}"

  puts $fp ""
  puts $fp "  bind .mapset.frame1.listbox <ButtonRelease-1> \{"
  puts $fp "        %W select from \[%W nearest %y\]"
  puts $fp "        %W select to \[%W nearest %y\]"
  puts $fp "	    set file_name \[%W get \[%W nearest %y\]\]"
  puts $fp "  \}"

  puts $fp ""
  puts $fp "  bind .mapset.frame1.listbox <Double-ButtonPress-1> \{"
  puts $fp "        %W select from \[%W nearest %y\]"
  puts $fp "        %W select to \[%W nearest %y\]"
  puts $fp "	    set file_name \[%W get \[%W nearest %y\]\]"
  puts $fp "  \}"

  puts $fp ""
  puts $fp "  .mapset.frame0.mapset delete 0 end"
  puts $fp "  .mapset.frame0.mapset insert 0 \$mapset"

  puts $fp ""
  puts $fp "  .mapset.frame1.listbox delete 0 end"
  puts $fp "  put_list .mapset.frame1.listbox \\"
  puts $fp "             \[get_list \"\$database/\$location/\$mapset/\$feature\"\]"

  puts $fp ""
  puts $fp "  grab set .mapset"
  puts $fp "  tkwait window .mapset"
  puts $fp ""
  puts $fp "  return \$file_name"
  puts $fp ""
  puts $fp "\}"

  puts $fp ""
  puts $fp ""
}





#***********************************************************************
#
# PROCEDURE: put_env 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_env { fp } {

  puts $fp "global database"
  puts $fp "global location"
  puts $fp "global mapset"
  puts $fp "global feature"

  puts $fp "if \{ \[info exists env(GISDBASE)\] == 0 ||" 
  puts $fp "     \[info exists env(LOCATION_NAME)\] == 0 ||"
  puts $fp "     \[info exists env(MAPSET)\] == 0 \} \{"
  puts $fp "   puts stdout \"GISDBASE, LOCATION_NAME and MAPSET must be set !!!\""
  puts $fp "   return"
  puts $fp "\}"

  puts $fp "set database \$env(GISDBASE)"
  puts $fp "set location \$env(LOCATION_NAME)"
  puts $fp "set mapset \$env(MAPSET)"
  puts $fp "set feature \"\""

}





#***********************************************************************
#
# PROCEDURE: set_command_entry 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc set_command_entry { fp } {

  puts $fp ""
  puts $fp ""
  puts $fp "proc set_command_entry \{ \} \{"
  puts $fp ""

  puts $fp "  .cmd.frame0.entry configure -state normal"
  puts $fp "  .cmd.frame0.entry delete 0 end"
  puts $fp "  .cmd.frame0.entry insert 0 \[put_command\]"
  puts $fp "  .cmd.frame0.entry configure -state disabled"

  puts $fp ""
  puts $fp "\}"

  puts $fp ""
  puts $fp ""
}




#***********************************************************************
#
# PROCEDURE: get_exit_condition 
#
# ARGUMENTS: none 
#
# RETURNED VALUES: returns the condition
#
# DESCRIPTION : creates the condition to exit
#
#***********************************************************************

proc get_exit_condition  { } {

  global variable_list
  global optional_list

  global program_list

  set program_name [lindex [lindex $program_list 0] 1]

  set flag 0
  foreach i $optional_list {
      if { $i == 1 } {
         set flag 1
      }
  }
  if { $flag == 0 } {
     set condition "\t\tset cmd \[put_command\]\n"  
     set condition "$condition \t\tif \{ \$cmd != \"\"\ \} \{\n"
     set condition "$condition \t\t   eval \" exec xterm -title $program_name -geometry 50x5 -exec \$cmd \" \n" 
     set condition "$condition \t\t   destroy .cmd\n"
     set condition "$condition \t\t\}\n"
     return $condition
  }  

  set condition ""
  set k 0
  foreach i $variable_list {
      if { [lindex $optional_list $k] == 1 } {
         foreach j $i {  
             set var $j
             if { $condition == "" } {
                set condition " \$$var != \"\""
             } else {
                set condition "$condition && \$$var != \"\""
             }
         }
      }
      incr k
  }

  set condition "\n\t\tif \{ $condition \} \{\n"
  set condition "$condition \t\tset cmd \[put_command\]\n"  
  set condition "$condition \t\tif \{ \$cmd != \"\"\ \} \{\n"
  set condition "$condition \t\t   eval \" exec xterm -title $program_name -geometry 50x5 -exec \$cmd \" \n" 
  set condition "$condition \t\t   destroy .cmd\n"
  set condition "$condition \t\t\}\n"
  set condition "$condition \t\}\n"
  return $condition

}





#***********************************************************************
#
# PROCEDURE: put_command 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_command { fp } {

  global program_list

  set program_name [lindex [lindex $program_list 0] 1]

  global variable_list
  global parameter_list
  global optional_list

  puts $fp ""
  puts $fp ""
  puts $fp "proc put_command \{ \} \{"
  puts $fp ""

  put_global $fp

  puts $fp ""
  puts $fp "  set cmd \"\""

  set k 0
  foreach i $variable_list {
      set parm [lindex $parameter_list $k]
      set condition ""
      set cmd ""
      foreach j $i {  
          set var $j
          if { $condition == "" } {
             set condition " \$$var != \"\""
          } else {
             set condition "$condition || \$$var != \"\""
          }
          if { $cmd == "" } {
             set cmd "     set cmd \"\$cmd $parm\$$var"
          } else {
             if { $parm == "-" } {
                set cmd "$cmd\$$var"
             } else {
                set cmd "$cmd,\$$var"
             }
          }
      }
      set condition "\n  if \{ $condition \} \{\n    $cmd\"\n  \}"

      puts $fp $condition
      incr k
  }

  puts $fp ""
  puts $fp "  if \{ \$cmd != \"\" \} \{"
  puts $fp "     set cmd \"$program_name \$cmd\""
  puts $fp "  \}"
  puts $fp "\}"
  puts $fp ""
  puts $fp ""

}  





#***********************************************************************
#
# PROCEDURE: entry widget 
#
# ARGUMENTS: fp : file pointer
#            frame : label of current frame
#            label : label of entry widget
#            desc : description to appear in entry widget
#            var : name of variable to set if entry is complete
#
# RETURNED VALUES: none
#
# DESCRIPTION : creates an entry widget 
#
#***********************************************************************

proc entry_widget  { fp frame label desc var state } {

  puts $fp ""
  puts $fp "  frame $frame \\"
  puts $fp "    -relief \{flat\}"

  puts $fp ""
  puts $fp "  label $frame.label \\"
  puts $fp "    -anchor \{w\} \\"
  puts $fp "    -text \{$label\} \\"
  puts $fp "    -padx \{2\}"

  puts $fp ""
  puts $fp "  scrollbar $frame.hscrollbar \\"
  puts $fp "    -command \{$frame.entry view\} \\"
  puts $fp "    -orient \{horizontal\}"

  puts $fp ""
  puts $fp "  entry $frame.entry \\"
  puts $fp "    -relief \{sunken\} \\"
  puts $fp "    -width 50 \\"
  puts $fp "    -xscrollcommand \{$frame.hscrollbar set\}"

  puts $fp ""
  puts $fp "  pack append $frame \\"
  puts $fp "    $frame.label \{ top fillx \} \\"
  puts $fp "    $frame.entry \{ top fillx \} \\"
  puts $fp "    $frame.hscrollbar \{ bottom fillx \}"

  if { $desc != "" } {
     puts $fp ""
     puts $fp "  $frame.entry delete 0 end"
     puts $fp "  $frame.entry insert 0 \{$desc\}"
  }
  if { $state != "" } {
     puts $fp ""
     puts $fp "  $frame.entry configure -state $state"
  }


  if { $var != "" } {
     puts $fp ""
     puts $fp "  bind $frame.entry <Return> \{"
     puts $fp "       set $var \[%W get\] \}"
  }

  puts $fp ""
  puts $fp ""

}





#***********************************************************************
#
# PROCEDURE: checkbutton_widget 
#
# ARGUMENTS: fp : file pointer
#            frame : label of current frame
#            label : label of entry widget
#            var : name of variable to set if entry is complete
#            value_on : value to set 'var" when active
#            value_off : value to set 'var" when not active
#
# RETURNED VALUES: none
#
# DESCRIPTION : creates a checkbutton widget 
#
#***********************************************************************

proc checkbutton_widget  { fp frame label var value_on value_off } {

  puts $fp ""
  puts $fp "  frame $frame \\"
  puts $fp "    -relief \{flat\}"

  puts $fp ""
  puts $fp "  checkbutton $frame.checkbutton \\"
  puts $fp "     -text \"$label\" \\"
  puts $fp "     -relief flat \\"
  puts $fp "     -anchor \{w\} \\"
  puts $fp "     -onvalue \"$value_on\" \\"
  if { $value_off != "" } {
     puts $fp "     -offvalue \"$value_off\" \\"
  } else {   
     puts $fp "     -offvalue \"\" \\"
  }

  set list [split $frame "."]
  if { [lindex $list 1] == "cmd" } {
     puts $fp "     -command \{ set_command_entry \} \\"
  }  
  puts $fp "     -variable $var"
 
  puts $fp ""
  puts $fp "  pack append $frame \\"
  puts $fp "    $frame.checkbutton \{ left \}"

  puts $fp ""

}



  

#***********************************************************************
#
# PROCEDURE: scale_widget 
#
# ARGUMENTS: fp : file pointer
#            frame : label of current frame
#            label : label of entry> widget
#            var : name of variable to set if entry is complete
#            min : minimum value
#            max : maximum value
#            def : default value
#
# RETURNED VALUES: none
#
# DESCRIPTION : creates a scale widget 
#
#***********************************************************************

proc scale_widget  { fp frame label min max def} {

  puts $fp ""
  puts $fp "  frame $frame \\"
  puts $fp "    -relief \{flat\}"

  puts $fp ""
  puts $fp "  scale $frame.scale \\"
  puts $fp "     -label \"$label\" \\"
  puts $fp "     -from $min \\"
  puts $fp "     -to $max \\"
  puts $fp "     -length 350 \\"
  puts $fp "     -orient \{horizontal\}"
 
  puts $fp ""
  puts $fp "  pack append $frame \\"
  puts $fp "    $frame.scale \{ left expand fill \}"

  puts $fp ""
  puts $fp "  $frame.scale set $def"
  puts $fp ""
}
 




#***********************************************************************
#
# PROCEDURE: listbox_widget 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION : creates a listbox widget 
#
#***********************************************************************

proc listbox_widget  { fp frame label var list } {

  puts $fp ""
  puts $fp "  frame $frame \\"
  puts $fp "    -relief \{flat\}"

  puts $fp ""
  puts $fp "  label $frame.label \\"
  puts $fp "     -anchor \{w\} \\"
  puts $fp "     -text \{$label\} \\"
  puts $fp "     -padx \{2\}"

  puts $fp ""
  puts $fp "  listbox $frame.listbox \\"
  puts $fp "     -relief sunken \\"
  puts $fp "     -yscrollcommand \{$frame.vscrollbar set\}"

  puts $fp ""
  puts $fp "  scrollbar $frame.vscrollbar \\"
  puts $fp "     -command \{$frame.listbox yview\}"

  puts $fp ""
  puts $fp ""
  puts $fp "  pack append $frame \\"
  puts $fp "    $frame.label \{ top fillx \} \\"
  puts $fp "    $frame.listbox \{ left expand fill \} \\"
  puts $fp "    $frame.vscrollbar \{ right fill \}"

  puts $fp ""
  puts $fp "  bind $frame.listbox <Button-1> \{"
  puts $fp "       %W select from \[%W nearest %y\]"
  puts $fp "       %W select to \[%W nearest %y\]"
  puts $fp "       set $var \[%W get \[%W nearest %y\]\]"
  puts $fp "  \}"

  puts $fp ""
  puts $fp "  bind $frame.listbox <ButtonRelease-1> \{"
  puts $fp "       %W select from \[%W nearest %y\]"
  puts $fp "       %W select to \[%W nearest %y\]"
  puts $fp "       set $var \[%W get \[%W nearest %y\]\]"
  puts $fp "  \}"

  puts $fp ""
  puts $fp "  bind $frame.listbox <Double-ButtonPress-1> \{"
  puts $fp "       %W select from \[%W nearest %y\]"
  puts $fp "       %W select to \[%W nearest %y\]"
  puts $fp "       set $var \[%W get \[%W nearest %y\]\]"
  puts $fp "  \}"
  puts $fp ""

  puts $fp "  $frame.listbox delete 0 end"
  puts $fp "  foreach i \{$list\} \{"
  puts $fp "      $frame.listbox insert 0 \$i"
  puts $fp "  \}"

  puts $fp ""
}
  




#***********************************************************************
#
# PROCEDURE: button_widget 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION : creates a button widget 
#
#***********************************************************************

proc button_widget  { fp frame label } {

  puts $fp ""
  puts $fp "  button $frame \\"
  puts $fp "     -text \"$label ...\" \\"
  puts $fp "     -relief raised \\"
  puts $fp "     -padx 10 \\"
  puts $fp "     -command \"$label"
  puts $fp "                set_command_entry\""
 
  puts $fp ""
}
  




#***********************************************************************
#
# PROCEDURE: database element widget 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION : creates an database element widget 
#
#***********************************************************************

proc database_entry_widget  { fp frame label desc var button_label } {

  puts $fp ""
  puts $fp "  frame $frame \\"
  puts $fp "    -relief \{flat\}"

  puts $fp ""
  puts $fp "  label $frame.label \\"
  puts $fp "    -anchor \{w\} \\"
  puts $fp "    -text \{$label\} \\"
  puts $fp "    -padx \{2\}"

  puts $fp ""
  puts $fp "  scrollbar $frame.hscrollbar \\"
  puts $fp "    -command \{$frame.entry view\} \\"
  puts $fp "    -orient \{horizontal\}"

  puts $fp ""
  puts $fp "  entry $frame.entry \\"
  puts $fp "    -relief \{sunken\} \\"
  puts $fp "    -width 50 \\"
  puts $fp "    -xscrollcommand \{$frame.hscrollbar set\}"
 
  puts $fp ""
  puts $fp "  button $frame.button \\"
  puts $fp "    -relief \{raised\} \\"
  puts $fp "    -anchor \{n\} \\"
  puts $fp "    -text \{$button_label\} \\"
  puts $fp "    -command \{ set file \[mapset_listbox $button_label\]"
  puts $fp "                if \{ \$file != \"\" \} \{"
  puts $fp "                   set $var \$file"
  puts $fp "                   $frame.entry delete 0 end"
  puts $fp "                   $frame.entry insert 0 \$file"
  puts $fp "                   set_command_entry"
  puts $fp "                \}"
  puts $fp "             \}"
  
  puts $fp ""
  puts $fp ""
  puts $fp "  pack append $frame \\"
  puts $fp "    $frame.label \{ top fillx \} \\"
  puts $fp "    $frame.button \{ right frame n \} \\"
  puts $fp "    $frame.entry \{ top fill \} \\"
  puts $fp "    $frame.hscrollbar \{ top fillx \}"


  puts $fp ""

  if { $desc != "" } {
     puts $fp "  $frame.entry delete 0 end"
     puts $fp "  $frame.entry insert 0 \{$desc\}"
  }

  puts $fp ""

  if { $var != "" } {
     puts $fp ""
     puts $fp "  bind $frame.entry <Return> \{"
     puts $fp "       set $var \[%W get\] \}"
     puts $fp ""
  }
}
  




#***********************************************************************
#
# PROCEDURE: put_ok_cancel 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_ok_cancel { ofp frame main ok_list cancel_list binding_list } {

  puts $ofp ""
  puts $ofp "  frame $frame \\"
  puts $ofp "     -borderwidth \{2\}"

  puts $ofp ""
  puts $ofp "  button $frame.ok \\"
  puts $ofp "     -text Ok \\"
  puts $ofp "     -relief raised \\"
  puts $ofp "     -padx 10 \\"
  puts $ofp "     -command \{"
  
  foreach i $ok_list {
      puts $ofp "                  $i"
  }
  puts $ofp "                 destroy $main \}"
 
  puts $ofp ""
  puts $ofp "  button $frame.cancel \\"
  puts $ofp "     -text Cancel \\"
  puts $ofp "     -relief raised \\"
  puts $ofp "     -padx 10 \\"
  puts $ofp "     -command \{"

  foreach i $cancel_list {
      puts $ofp "                  $i"
  }
  puts $ofp "                 destroy $main \}"
 
  puts $ofp ""
  puts $ofp "  pack append $frame \\"
  puts $ofp "    $frame.ok \{ left expand \} \\"
  puts $ofp "    $frame.cancel \{ right expand \}"
  puts $ofp ""

  if { [llength $binding_list] != 1 } {
     set length [expr [llength $binding_list]-1]
     for {set i 0} {$i < $length} {incr i} {
         puts $ofp ""
         puts $ofp "bind [lindex $binding_list $i] <Return> \{" 
         puts $ofp "     focus [lindex $binding_list [expr $i+1]]"
         puts $ofp "\}"
     }
     puts $ofp ""
     puts $ofp "bind [lindex $binding_list $length] <Return> \{" 
     puts $ofp "     focus [lindex $binding_list 0]"
     puts $ofp "\}"

  }
  
}





#***********************************************************************
#
# PROCEDURE: split_command_string 
#
# ARGUMENTS: command_string : command to split.
#                                1. get all parameter
#                                2. get all variable name
#                                3. get optional list
#
# RETURNED VALUES: none
#
# DESCRIPTION : split command string to get all parameters
#               [IMPORTANT] this procedure sets three global variables
#                           variable_list, parameter_list, optional_list
#
#***********************************************************************

proc split_command_string  { command_string } {

  global variable_list
  global parameter_list
  global optional_list

  set list $command_string

  set variable_list ""
  set parameter_list ""
  set optional_list ""

  foreach i $list {

     if { [string match {\[*\]} $i] } {
        if { [string match {\[-*\]} $i] } { 
           lappend optional_list {-1}
           lappend parameter_list {-}
        } else {
           lappend optional_list {0}
           set first [string first ( $i]
           lappend parameter_list [string range $i 1 [expr $first-1]]
        }      
     } else {
        lappend optional_list {1} 
        set first [string first ( $i]
        lappend parameter_list [string range $i 0 [expr $first-1]]
     }

     set length [string length $i]
     set first [string first ( $i]
     set j [expr $first+1]
     set var ""
     set temp_list ""
     while { $j < $length } {
           if { [string index $i $j] == "(" } {
              set var ""
           } else {
              if { [string index $i $j] == ")" } {
                 lappend temp_list $var
              } else {
                 set var "$var[string index $i $j]"
              }
           }
           incr j
     }
     lappend variable_list $temp_list
  }
}





#***********************************************************************
#
# PROCEDURE: get_next_line 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION : 
#
#***********************************************************************

proc get_next_line { fp } {

  set thisline [gets $fp]
  while { [eof $fp] == 0 && $thisline == "" } {
        set thisline [gets $fp]
  }
  return [string trim $thisline]
}
  



#***********************************************************************
#
# PROCEDURE: get_header 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc get_header { ifp } {

  global program_list

  set token ""

  set thisline [get_next_line $ifp]
  while { $thisline != "\{" } {

        # get program name
        if { [scan $thisline "program:%s" token] } {
           set f [string first \" $thisline]
           set l [string last \" $thisline]
           set program_name [string range $thisline [expr $f+1] [expr $l-1]]
           lappend program_list "program_name $program_name"
        } else {
           # get title
           if { [scan $thisline "title:%s" token] } {
              set f [string first \" $thisline]
              set l [string last \" $thisline]
              set title_name [string range $thisline [expr $f+1] [expr $l-1]]
              lappend program_list "title \{$title_name\}"
           } else {
              # get commmand
              if { [scan $thisline "commandstring:%s" token] } {
                 set f [string first \" $thisline]
                 set l [string last \" $thisline]
                 set command_name [string range $thisline [expr $f+1] [expr $l-1]]
                 split_command_string $command_name
                 lappend program_list "command_string \{$command_name\}"
              } else {
                 # get description
                 if { [scan $thisline "description:%s" token] } {
                    set f [string first \" $thisline]
                    set l [string last \" $thisline]
                    set description_name [string range $thisline [expr $f+1] [expr $l-1]]
                    lappend program_list "description \{$description_name\}"
                 }
              }
          }
        }
        set thisline [get_next_line $ifp]
  }
}





#***********************************************************************
#
# PROCEDURE: get_parameter 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc get_parameter { ifp name terminator } {

  global is_there_a_dbelement

  set description_name ""
  set token ""

  set cmd_type ""

  set thisline [get_next_line $ifp]
  while { $thisline != $terminator } {

        if { [scan $thisline "description:%s" token] } {
             set f [string first \" $thisline]
             set l [string last \" $thisline]
             set description_name "\{[string range $thisline [expr $f+1] [expr $l-1]]\}" 
        } else {
             if { [scan $thisline "type:integer:%s" token] } {
                set f [string first \" $token]
                set l [string last \" $token]
                set token [string range $token [expr $f+1] [expr $l-1]] 
                set value_list [split $token :]
                set min [lindex $value_list 0]
                set max [lindex $value_list 1]
                set def [lindex $value_list 2]
                set type_name [string range $thisline [expr $f+1] [expr $l-1]]
                set cmd_type "scale $description_name $name $min $max $def"
             } else {
                  if { [scan $thisline "type:database_element:%s" token] } {
                     set f [string first : $token]
                     set type [lindex [split $thisline :] 2]
                     set cmd_type "database_entry $description_name $name $type"
                     set is_there_a_dbelement 1
                  } else {
                       if { [scan $thisline "type:enum:%s" token] } {
                          set f [string first \" $thisline]
                          set l [string last \" $thisline]
                          set list [split [string range $thisline [expr $f+1] [expr $l-1]] ,]
                          set cmd_type "enum $description_name $name \{$list\}"
                       } else {
                          if { [scan $thisline "type:%s" token] } {
                             set cmd_type "entry $description_name $name normal"
                          }
                       }
                  }
             }
        }
        set thisline [get_next_line $ifp]
  }

  return $cmd_type
}



#***********************************************************************
#
# PROCEDURE: get_flag 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc get_flag { ifp name terminator } {

  set cmd_type ""

  set token ""

  set thisline [get_next_line $ifp]
  while { $thisline != $terminator } {

        if { [scan $thisline "key:%s" token] } {
             set f [string first \" $thisline]
             set l [string last \" $thisline]
             set key_name [string range $thisline [expr $f+1] [expr $l-1]] 
        } else {
             if { [scan $thisline "description:%s" token] } {
                set f [string first \" $thisline]
                set l [string last \" $thisline]
                set description_name "\{[string range $thisline [expr $f+1] [expr $l-1]]\}" 
                set cmd_type "check_button $description_name $name $key_name"
             }
        }	
        set thisline [get_next_line $ifp]
  }

  return $cmd_type
}




#***********************************************************************
#
# PROCEDURE: get_dialog
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc get_dialog { ifp name terminator } {

  set cmd_type ""

  set token ""

  lappend cmd_type "dialog"
               
  set tmp_name [split $name "/"]
  set tmp_name [join $tmp_name "_"]

  lappend cmd_type $tmp_name

  set thisline [get_next_line $ifp]
  while { $thisline != $terminator } {
        
        if { [scan $thisline "parameter %s" token] != 0 } {
           lappend cmd_type [get_parameter $ifp $token ";"]
        } else {
             if { [scan $thisline "flag %s" token] != 0 } {
                lappend cmd_type [get_flag $ifp $token ";"]
             }
        }
        set thisline [get_next_line $ifp]
  }

  return $cmd_type
}




#***********************************************************************
#
# PROCEDURE: put_global 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION : writes all global variables
#
#***********************************************************************

proc put_global  { fp } {

  global variable_list

  puts $fp ""
  foreach i $variable_list {
      foreach j $i {
         puts $fp "  global $j"
      }
  }
  puts $fp "" 
}



#***********************************************************************
#
# PROCEDURE: put_set_global 
#
# ARGUMENTS: fp : file pointer
#
# RETURNED VALUES: none
#
# DESCRIPTION : writes all global variables and sets them
#
#***********************************************************************

proc put_set_global  { fp } {

  global variable_list

  puts $fp ""
  foreach i $variable_list {
      foreach j $i {
         puts $fp "global $j"
         puts $fp "set $j \"\""
         puts $fp ""
      }
  }
  puts $fp "" 
}





#***********************************************************************
#
# PROCEDURE: put_footer
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_footer { fp nbr_frame binding } {

  global program_list
  global dialog_list
  global dialog_pack

  set program_name [lindex [lindex $program_list 0] 1]

  set frame ".cmd.frame$nbr_frame"

  puts $fp ""
  puts $fp "  frame $frame \\"
  puts $fp "     -borderwidth \{2\}"

  puts $fp ""
  puts $fp "  button $frame.ok \\"
  puts $fp "     -text Ok \\"
  puts $fp "     -relief raised \\"
  puts $fp "     -padx 10 \\"
  puts $fp "     -command \{ [get_exit_condition] \}"
 
  puts $fp ""
  puts $fp "  button $frame.cancel \\"
  puts $fp "     -text Cancel \\"
  puts $fp "     -relief raised \\"
  puts $fp "     -padx 10 \\"
  puts $fp "     -command \{ destroy .cmd \}"
 
  puts $fp ""
  puts $fp "  pack append $frame \\"
  puts $fp "    $frame.ok \{ left expand \} \\"
  puts $fp "    $frame.cancel \{ right expand \}"


  if { $dialog_pack != "" } {
      puts $fp ""
      puts $fp "  pack append $dialog_pack \\"
      puts $fp "    $dialog_pack.label \{ top fillx \} \\"
  }

  for {set i 0} {$i < [expr [llength $dialog_list]-1]} {incr i} {
      puts $fp "    [lindex $dialog_list $i] \{ left \} \\"
  }
  if { [llength $dialog_list] != 0 } {
      puts $fp "    [lindex $dialog_list [expr [llength $dialog_list]-1]]  \{ left \}"
      puts $fp ""
  }

  puts $fp ""
  puts $fp ""
  puts $fp "  pack append .cmd \\"


  incr nbr_frame
  set nbr [expr $nbr_frame-1]
  for {set i 1} {$i < $nbr } {incr i} {
      puts $fp "    .cmd.frame$i \{ top expand fill \} \\"
  }
  set nbr_frame [expr $nbr_frame-1]

  puts $fp "    .cmd.frame0 \{ top expand fill \} \\"
  puts $fp "    $frame \{ bottom expand fill \}"
  puts $fp ""
  puts $fp ""

  foreach i $binding {
      puts $fp $i
      puts $fp ""
  }  

  puts $fp "  grab set .cmd"
  puts $fp "  tkwait window .cmd"

  puts $fp ""
  puts $fp "\}"

  put_set_global $fp

  put_env $fp

  puts $fp ""
  puts $fp "proc_$program_name"

}




#***********************************************************************
#
# PROCEDURE: put_main
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_main { fp } {

  global program_list


  global dialog_list
  set dialog_list ""

  global dialog_pack
  set dialog_pack ""

  global binding_list
  set binding_list ""

  set nbr_button 0

  set nbr_frame 2

  for {set i 4} {$i < [llength $program_list]} {incr i} {

      set widget [lindex $program_list $i]
      case [lindex $widget 0] in {

           {dialog} {
                            if { $dialog_pack == "" } {
                               set dialog_pack ".cmd.frame$nbr_frame"
                               puts $fp ""
                               puts $fp "  frame $dialog_pack \\"
                               puts $fp "    -relief \{flat\}"
                               puts $fp ""
                               puts $fp "  label $dialog_pack.label \\"
                               puts $fp "    -anchor \{w\} \\"
                               puts $fp "    -text \{Other options available:\} \\"
                               puts $fp "    -padx \{2\}"
                               incr nbr_frame
                            } 
                            set frame "$dialog_pack.button$nbr_button"
                            lappend dialog_list $frame
                            set label [lindex $widget 1]
                            button_widget $fp $frame $label
                            incr nbr_button
                            
           }


            {entry} { 
                            set frame ".cmd.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set state [lindex $widget 3]  
                            entry_widget $fp $frame $label "" $var $state
                            set binding "bind $frame.entry <KeyRelease> \{\n"
                            set binding "$binding\t\tset $var \[$frame.entry get\]\n"
                            set binding "$binding\t\tset_command_entry\n\}"
                            lappend binding_list "$binding"
                            set binding "bind $frame.entry <Return> \{\n"
                            set binding "$binding\t\tset $var \[$frame.entry get\]\n"
                            set binding "$binding\t\tset_command_entry\n\}"
                            lappend binding_list "$binding"
                            incr nbr_frame
           }


           {check_button} {
                            set frame ".cmd.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set value_on [lindex $widget 3]  
                            checkbutton_widget $fp $frame $label $var $value_on ""
                            incr nbr_frame
           }

           {scale} {
                            set frame ".cmd.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set min [lindex $widget 3]  
                            set max [lindex $widget 4]  
                            set def [lindex $widget 5]  
                            scale_widget $fp $frame $label $min $max $def
                            set binding "bind $frame.scale <ButtonPress-1> \{\n"
                            set binding "$binding\t\tset $var \[$frame.scale get\]\n"
                            set binding "$binding\t\tset_command_entry\n\}"
                            lappend binding_list "$binding"
                            set binding "bind $frame.scale <ButtonRelease-1> \{\n"
                            set binding "$binding\t\tset $var \[$frame.scale get\]\n"
                            set binding "$binding\t\tset_command_entry\n\}"
                            lappend binding_list "$binding"
                            incr nbr_frame
           }

           {enum} {
                            set frame ".cmd.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set list [lindex $widget 3]  
                            listbox_widget $fp $frame $label $var $list
                            set binding "bind $frame.listbox <ButtonPress-1> \{\n"
                            set binding "$binding %W select from \[%W nearest %y\]\n"
                            set binding "$binding %W select to \[%W nearest %y\]\n"
                            set binding "$binding\t\tset $var \[%W get \[%W nearest %y\]\]\n"
                            set binding "$binding\t\tset_command_entry\n\}"
                            lappend binding_list "$binding"
                            set binding "bind $frame.listbox <ButtonRelease-1> \{\n"
                            set binding "$binding %W select from \[%W nearest %y\]\n"
                            set binding "$binding %W select to \[%W nearest %y\]\n"
                            set binding "$binding\t\tset $var \[%W get \[%W nearest %y\]\]\n"
                            set binding "$binding\t\tset_command_entry\n\}"
                            lappend binding_list "$binding"
                            incr nbr_frame

           }

           {database_entry} {
                            set frame ".cmd.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set type [lindex $widget 3]  
                            database_entry_widget $fp $frame $label "" $var $type
                            incr nbr_frame
           }
      }
  }

  put_footer $fp $nbr_frame $binding_list
}





#***********************************************************************
#
# PROCEDURE: put_header 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_header { fp } {

  global program_list


  set program_name [lindex [lindex $program_list 0] 1]
  puts $fp "proc proc_$program_name \{ \} \{"
  puts $fp ""
  put_global $fp
  puts $fp ""
  puts $fp "  toplevel .cmd"
  puts $fp ""
  puts $fp "  wm geometry .cmd +100+20"
  puts $fp ""

  set title [lindex [lindex $program_list 1] 1]  
  puts $fp "  wm title .cmd \"$title\""
  puts $fp ""


  set command_string [lindex [lindex $program_list 2] 1]
  entry_widget $fp ".cmd.frame0" "Command:" $program_name "" "disabled"
 

  set description [lindex [lindex $program_list 3] 1]
  entry_widget $fp ".cmd.frame1" "Description:" "$description" "" "disabled"
 
}





#***********************************************************************
#
# PROCEDURE: put_dialogs
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc put_dialogs { fp } {

  global program_list

  global ok_list
  global cancel_list
  global binding_list

  for {set i 4} {$i < [llength $program_list]} {incr i} {

      # is it a dialog
      set line [lindex $program_list $i] 
      if { [lindex $line 0] == "dialog" } {

         set nbr_frame 0
         
         set ok_list ""
         set cancel_list ""
         set binding_list ""


         set name [lindex $line 1]
         puts $fp ""
         puts $fp ""
         puts $fp "proc $name \{ \} \{"
         puts $fp ""
         puts $fp ""
         puts $fp "  toplevel .$name"
         puts $fp ""
         puts $fp "  wm geometry .$name +100+20"
         puts $fp "  wm title .$name \"$name\""
         puts $fp ""
         puts $fp ""

         for {set j 2} {$j < [llength $line]} {incr j} {
             set widget [lindex $line $j]
             case [lindex $widget 0] in {

                  {entry} { 
                            set frame ".$name.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set state [lindex $widget 3]
                            puts $fp "  global $var"
                            puts $fp ""  
                            entry_widget $fp $frame $label "" $var $state
                            lappend ok_list "set $var \[$frame.entry get\]"
                            lappend cancel_list "set $var \"\""
                            lappend binding_list "$frame.entry" 
                            incr nbr_frame
                  }


                  {check_button} {
                            set frame ".$name.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set value_on [lindex $widget 3]  
                            puts $fp "  global $var"
                            puts $fp ""  
                            checkbutton_widget $fp $frame $label $var $value_on ""
                            lappend binding_list "$frame.checkbutton" 
                            incr nbr_frame
                  }

                  {scale} {
                            set frame ".$name.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set min [lindex $widget 3]  
                            set max [lindex $widget 4]  
                            set def [lindex $widget 5]  
                            puts $fp "  global $var"
                            puts $fp ""  
                            scale_widget $fp $frame $label $min $max $def
                            lappend ok_list "set $var \[$frame.scale get\]"
                            lappend cancel_list "set $var \"\"" 
                            lappend binding_list "$frame.scale" 
                            incr nbr_frame
                  }

                  {enum} {
                            set frame ".$name.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set list [lindex $widget 3]  
                            puts $fp "  global $var"
                            puts $fp ""  
                            listbox_widget $fp $frame $label $var $list
                            lappend binding_list "$frame.listbox" 
                            incr nbr_frame
                  }

                  {database_entry} {
                            set frame ".$name.frame$nbr_frame"
                            set label [lindex $widget 1]
                            set var [lindex $widget 2]
                            set type [lindex $widget 3]  
                            puts $fp "  global $var"
                            puts $fp ""  
                            database_entry_widget $fp $frame $label "" $var $type
                            lappend ok_list "set $var \[$frame.entry get\]"
                            lappend cancel_list "set $var \"\"" 
                            lappend binding_list "$frame.entry" 
                            incr nbr_frame
                  }
             }
         }
         set frame ".$name.frame$nbr_frame"
         put_ok_cancel $fp $frame ".$name" $ok_list $cancel_list $binding_list
         incr nbr_frame

         puts $fp ""
         puts $fp ""
         puts $fp "  pack append .$name \\"

         set nbr [expr $nbr_frame-1]
         for {set k 0} {$k < $nbr } {incr k} {
             puts $fp "    .$name.frame$k \{ top expand fill \} \\"
         }
         set nbr_frame [expr $nbr_frame-1]

         puts $fp "    .$name.frame$nbr_frame \{ bottom expand fill \}"
         puts $fp ""
         puts $fp ""

         puts $fp ""
         puts $fp "  grab set .$name"
         puts $fp "  tkwait window .$name"

         puts $fp ""
         puts $fp "\}"
         puts $fp ""
         puts $fp ""
     }
  }

}





#***********************************************************************
#
# PROCEDURE: convert 
#
# ARGUMENTS:
#
# RETURNED VALUES: none
#
# DESCRIPTION :
#
#***********************************************************************

proc convert { filename } {

  global program_list

  global is_there_a_dbelement
  set is_there_a_dbelement 0

  global variable_list
  global parameter_list
  global optional_list

  set token ""

  set program_list ""

  set ifp [open "../xclip/$filename" "r"]

  get_header $ifp

  set thisline [get_next_line $ifp]
  while { [eof $ifp] == 0 } {

          # is it a parameter
          if { [scan $thisline "parameter %s" token] != 0} {
             lappend program_list [get_parameter $ifp $token ";"]
          } else {
             # is it a dialog button
             if { [scan $thisline "dialog %s" token] != 0} {
                set f [string first \" $thisline]
                set l [string last \" $thisline]
                set tname [join [string range $thisline [expr $f+1] [expr $l-1]] "_"] 
                set tname [string tolower $tname]
                lappend program_list [get_dialog $ifp $tname \}]
             } else {
                # is it a flag button
                if { [scan $thisline "flag %s" token] != 0 } {
                   lappend program_list [get_flag $ifp $token ";"]
                }
             }
          }
          set thisline [get_next_line $ifp]
  }

  close $ifp

  set new_filename [string range $filename 2 end]
  set ofp [open "../convert/$filename" "w"]

  puts stdout $is_there_a_dbelement
  if { $is_there_a_dbelement == "1" } {
     get_feature $ofp     
     put_list $ofp
     get_list $ofp
     mapset_listbox_widget $ofp
  }

  put_dialogs $ofp 

  put_command $ofp
  set_command_entry $ofp

  put_header $ofp 

  put_main $ofp

  close $ofp

}





#***********************************************************************
#
#       main module 
#
#***********************************************************************


global program_list

global variable_list
global parameter_list
global optional_list

global is_there_a_dbelement

set current_dir [exec pwd]

cd "../xclip"
foreach i [exec ls -a [exec pwd]] {
      if { [string compare $i "."] != 0 && [string compare $i ".."] != 0 } {
           puts stdout $i
           convert $i
      }
}

cd $current_dir
