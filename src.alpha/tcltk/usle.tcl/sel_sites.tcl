#!/usr/local/bin/wish -f                           


# this is the default proc  called when "OK" is pressed
# to indicate yours, give it as the first arg to "fileselect"
#############################################################
set color black
set type x
#############################################################
proc fileselect.default.cmd {f} {
global color cmd size type color selected mapset 

puts $type
puts $color
puts $selected
puts $mapset

puts stdout [exec xterm -iconic -e d.sites site=$f@$mapset c=$color size=$size t=$type] 
set cmd d.sites
}

# this is the proc that creates the file selector box

proc fileselect {
    {cmd fileselect.default.cmd} 
    {purpose "Open Sites file:                   "} 
    {w .fileSelectWindow} } {  

    catch {destroy $w}

    toplevel $w
    grab $w
    wm title $w "Select File"
    wm geometry $w +600+177
    wm geometry $w 520x321
# path independent names for the widgets

global fileselect_entry fileselect_list fileselect_ok fileselect_cancel 
global colorselect_list colorselect_ok colorselect_entry color_list color 
global sizeselect sizeselect_list size typeselect typeselect_list type
global typeselect_entry sizeselect_entry mapsetselect_list mapsetselect_entry global mapsetselect GISDBASE LOCATION MAPSET LOCATION_NAME mapset


set mapsetselect_entry $w.mapset.eframe.entry
set mapsetselect_list $w.mapset.sframe.list
set mapsetselect_scroll $w.mapset.sframe.scroll

set fileselect_entry $w.list.eframe.entry
set fileselect_list $w.list.sframe.list
set fileselect_scroll $w.list.sframe.scroll
set fileselect_ok $w.file.okframe.ok
set fileselect_cancel $w.file.cancel

set color_list "red \
orange yellow green blue indigo white black brown magenta aqua gray grey"
set colorselect_entry $w.list.e2frame.entry
set colorselect_list $w.list.s2frame.list
set colorselect_scroll $w.list.s2frame.scroll

set type_list "x diamond box +"
set typeselect_entry $w.file.e4frame.entry
set typeselect_list $w.file.s4frame.list
set typeselect_scroll $w.file.s4frame.scroll
set sizeselect_entry $w.list.e3frame.entry

#################

# top level widgets

frame $w.mapset -bd 3  -relief ridge -bg LemonChiffon2
place  $w.mapset -x 0 -y 0 -height 250 -width 167

frame $w.list -bd 3  -relief ridge -bg LemonChiffon2 
place  $w.list -x 168 -y 0 -height 250 -width 350 

frame $w.file -bd 3  -relief ridge -bg LemonChiffon2
place $w.file -x 0 -y 250 -height 70 -width 518 
  
#####
# Create and pack the three entries for sites list and color.

#global MAPSET
frame $w.mapset.eframe -bd 3 -relief ridge -bg LemonChiffon2
place $w.mapset.eframe -x 0 -y 0 -height 47 -width 161
label $w.mapset.eframe.label -bg LemonChiffon2 -text "Select Mapset:"
place $w.mapset.eframe.label -x 0 -y 0.5
entry $w.mapset.eframe.entry -relief sunken -width 20 -bd 2 -textvariable MAPSET
place $w.mapset.eframe.entry -x 3 -y 20 -width 145 -height 20

frame $w.list.eframe -bd 3 -relief ridge -bg LemonChiffon2
place $w.list.eframe -x 0 -y 0 -height 47 -width 160

label $w.list.eframe.label -bg LemonChiffon2 -text $purpose -bg LemonChiffon2
place $w.list.eframe.label -x 0 -y 0.5 

entry $w.list.eframe.entry -relief sunken -width 15 -bd 2 
place $w.list.eframe.entry -x 3 -y 20 -width 145 -height 20

frame $w.list.e2frame -bd 3 -relief ridge -bg LemonChiffon2
place $w.list.e2frame -x 162 -y 0 -height 47 -width 100

label $w.list.e2frame.label -bg LemonChiffon2 -text "Select Color:"
place  $w.list.e2frame.label -x 0 -y 0

entry $w.list.e2frame.entry -relief sunken -bd 2 -textvariable color
place $w.list.e2frame.entry -x 3 -y 20 -width 80 -height 20

#####
#Create the scrollbar and file list

frame $w.mapset.sframe -borderwidth 3 -relief ridge 
    place $w.mapset.sframe -x 0.0 -y 48 -height 195 -width 161 

scrollbar $w.mapset.sframe.yscroll -relief sunken  \
-command "$w.mapset.sframe.list yview"
     pack $w.mapset.sframe.yscroll -side right -fill y

  listbox $w.mapset.sframe.list -relief sunken -borderwidth 2 \
-yscroll "$w.mapset.sframe.yscroll set"    
     pack $w.mapset.sframe.list -expand yes -fill both
    
frame $w.list.sframe -borderwidth 3 -relief ridge 
    place $w.list.sframe -x 0.0 -y 48 -height 195 -width 160 

scrollbar $w.list.sframe.yscroll -relief sunken  \
-command "$w.list.sframe.list yview"
     pack $w.list.sframe.yscroll -side right -fill y

  listbox $w.list.sframe.list -relief sunken -borderwidth 2 \
-yscroll "$w.list.sframe.yscroll set"    
     pack $w.list.sframe.list -expand yes -fill both 

    frame $w.list.s2frame -borderwidth 3 -relief ridge 
     place $w.list.s2frame -x 162 -y 48 -height 195 -width 100

scrollbar $w.list.s2frame.yscroll -relief sunken \
-command "$w.list.s2frame.list yview"
     pack $w.list.s2frame.yscroll -side right -fill y
 
  listbox $w.list.s2frame.list -relief sunken -geometry 10x10 \
 -exportselection false -borderwidth 2 -yscroll "$w.list.s2frame.yscroll set"
     pack $w.list.s2frame.list -expand yes -fill both 


frame $w.list.e3frame -bd 3 -relief ridge -bg LemonChiffon2
place $w.list.e3frame -x 263 -y 0 -width 81 -height 47

label $w.list.e3frame.label -bg LemonChiffon2 -text "Icon Size:"
place $w.list.e3frame.label -x 0 -y 0

entry $w.list.e3frame.entry -relief sunken -width 12 -borderwidth 2
place  $w.list.e3frame.entry -x 3 -y 20 -width 50 -height 20


frame $w.file.e4frame -bd 2 -relief ridge -bg LemonChiffon2
place $w.file.e4frame -x 0 -y 0 -height 64 -width 138

label $w.file.e4frame.label -bg LemonChiffon2 -text "Select Icon Type:" 
place $w.file.e4frame.label -x 0 -y 3

entry $w.file.e4frame.entry -relief sunken -width 12 -borderwidth 2 -textvariable type
place $w.file.e4frame.entry -x 3 -y 30 -height 20 -width 124

 frame $w.file.s4frame -bd 2 -relief ridge
place $w.file.s4frame -x 139 -y 0 -width 138 -height 64

listbox $w.file.s4frame.list -relief sunken -geometry 10x20 \
 -exportselection false -bd 2 
pack $w.file.s4frame.list -expand yes -fill both  

scrollbar $w.file.s4frame.list.yscroll -relief sunken  \
-command "$w.file.s4frame.list yview"
pack $w.file.s4frame.list.yscroll -side right -fill y 

####   
   
###################################################    

frame $w.list.s3frame -borderwidth 3 -relief ridge -bg LemonChiffon2
place $w.list.s3frame -x 263 -y 48 -height 195 -width 81

# Create the scale, and the entry for typing in a value.

frame $w.list.s3frame.s -relief ridge 
pack $w.list.s3frame.s 

frame $w.list.s3frame.s.1 -relief ridge 
pack  $w.list.s3frame.s.1


label $w.list.s3frame.s.label -textvariable label
scale $w.list.s3frame.s.scale -from 5 -to 20 -length 5c  \
       -orient vertical -font -Adobe-Courier-Medium-R-Normal-*-100-* \
       -tickinterval 5 -command update_entry

 
    button $w.list.s3frame.s.up -width 2 -text + -command "tc_inc 1 1"
    button $w.list.s3frame.s.down -width 2 -text - -command "tc_inc 1 -1"
    pack $w.list.s3frame.s.label 
    place $w.list.s3frame.s.down -x 30 -y 0   
    pack $w.list.s3frame.s.scale -side left
    place $w.list.s3frame.s.up -x 0.0 -y 0.0 

# This is the procedure which updates the list box with values generated by the
#scale. The old values are deleted as new values are created.

proc update_entry {text} {
         global sizeselect_entry size
        .fileSelectWindow.list.e3frame.entry delete 0 end
	.fileSelectWindow.list.e3frame.entry insert 0 $text
        set size [$sizeselect_entry get]
}


foreach i [exec echo $type_list] {
        if {[string length $i] != 0} {     
            $typeselect_list insert end $i
        }
    } 

# The procedure below handles the "+" and "-" buttons next to
# the adjustor scales.  They just increment or decrement the
# appropriate scale value.

proc tc_inc {scale inc} {
eval .fileSelectWindow.list.s3frame.s.scale \
set [expr [.fileSelectWindow.list.s3frame.s.scale get]+$inc]
}

#####################################################

 
# buttons
   
   frame $w.file.okframe -borderwidth 2 -relief sunken
    frame $w.file.cancel -borderwidth 2 -relief sunken
    frame $w.file.clear -borderwidth 2 -relief sunken

    button $w.file.okframe.ok -text OK -relief raised \
        -command "fileselect.ok.cmd $w $cmd"
    
    button $w.file.cancel.b -text Done -relief raised \
        -command "fileselect.cancel.cmd $w"
     
    button $w.file.clear.b -text "Clear Display" -relief raised  \
        -command {
puts [exec xterm -iconic -e d.erase color=white]
  }
  
     place $w.file.okframe -x 280 -y 34 -width 115 -height 31
     place $w.file.cancel -x 397 -y 34 -width 115 -height 31
     pack $w.file.okframe.ok -expand yes -fill both
     $w.file.okframe.ok configure -state disabled
     pack $w.file.cancel.b -expand yes -fill both
     place $w.file.clear -x 280 -y 1 -width 232 -height 31
     pack $w.file.clear.b -expand yes -fill both

    #######################################################################


  # Fill the listbox with a list of all the files in the directory (run
  # the "ls" command to get that information).
 
     # Fill the listbox with a list of all the mapsets.

    foreach i [exec ls $GISDBASE/$LOCATION_NAME] {
        if {[string length $i] != 0} {
            $mapsetselect_list insert end $i
         }
    }

   foreach i [exec ls $GISDBASE/$LOCATION_NAME/$MAPSET/site_lists] {
        if {[string length $i] != 0} {
            $fileselect_list insert end $i
          }
    }


   foreach i [exec echo $color_list] {
        if {[string length $i] != 0} {     
            $colorselect_list insert end $i
        }
    } 

     
     foreach i [exec echo $type_list] {
        if {[string length $i] != 0} {     
            $typeselect_list insert end $i
        }
    } 
    

    #Set up bindings for the file browser.
    
    tk_listboxSingleSelect $mapsetselect_list

        bind $mapsetselect_list <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	eval $mapsetselect_entry delete 0 end
	eval $mapsetselect_entry insert 0 [%W get [%W nearest %y]]
        #global mapset
        set mapset [$mapsetselect_entry get]        
        eval $fileselect_entry delete 0 end
        eval $fileselect_list delete 0 end
        foreach i [exec ls $GISDBASE/$LOCATION_NAME/$MAPSET/site_lists] {
       if {[string length $i] != 0} {
            $fileselect_list insert end $i
        
         }
     }
     
}

    bind $fileselect_entry <Return> {eval $fileselect_ok invoke}
    bind $fileselect_entry <Control-c> {eval $fileselect_cancel invoke}
    bind $w <Control-c> {eval $fileselect_cancel invoke}
    bind $w <Return> {eval $fileselect_ok invoke}


    tk_listboxSingleSelect $fileselect_list

    bind $fileselect_list <ButtonRelease-1> {
         #puts stderr "button 1"
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	eval $fileselect_entry delete 0 end
	eval $fileselect_entry insert 0 [%W get [%W nearest %y]]
        eval $fileselect_ok configure -state normal -foreground black
        set mapset [$mapsetselect_entry get]
}

      
     #Set up bindings for the color browser.
 
    tk_listboxSingleSelect $colorselect_list
     
        bind $colorselect_list <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
        eval $colorselect_entry delete 0 end
	eval $colorselect_entry insert 0 [%W get [%W nearest %y]]
        set color [$colorselect_entry get]
       #eval $colorselect_ok configure -state normal -foreground black
      }
  
      tk_listboxSingleSelect $typeselect_list
     
        bind $typeselect_list <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
        eval $typeselect_entry delete 0 end
	eval $typeselect_entry insert 0 [%W get [%W nearest %y]]
        set type [$typeselect_entry get]
    #eval $fileselect_ok configure -state normal -foreground black
 }


#######################################################################

# auxiliary button procedures
 
# set kbd focus to entry widget

    focus $fileselect_entry 
 
}

 proc fileselect.cancel.cmd {w} {
     puts stderr "Cancel"
    destroy $w
}

proc fileselect.ok.cmd {w cmd} {
  global fileselect_entry fileselect_dirlabel fileselect_list sizeselect_entry
  global selected 
set selected [$fileselect_entry get] 
     
       if {[file isdirectory $selected] != 0} {
	cd $selected
	set dir [exec pwd]
	eval $fileselect_dirlabel configure -text $dir
	eval $fileselect_entry delete 0 end
	eval $fileselect_list delete 0 end
	#set mapset [$mapsetselect_entry get]
        foreach i [exec ls -a $dir] {
	    if {[string compare $i "."] != 0} {
		eval $fileselect_list insert end $i    
         
            }
	}	
        return
    }
     
    #destroy $w
    eval $cmd $selected 
  
}








   
