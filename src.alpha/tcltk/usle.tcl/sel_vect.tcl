#!/usr/local/bin/wish -f                           


# this is the proc that creates the file selector box
# this is the default proc  called when "OK" is pressed
# to indicate yours, give it as the first arg to "fileselect"


 
proc fileselect.default.cmd {f} {
global color cmd selected MAPSET
puts stdout [exec d.vect map=$f@$MAPSET color=$color]
set cmd d.vect 
}


proc fileselect {
    {cmd fileselect.default.cmd} 
    {purpose "Open Vector file:                   "} 
    {w .fileSelectWindow} } {  

    catch {destroy $w}

    toplevel $w
    grab $w
    wm title $w "Select File"
    wm geometry $w +600+177 
    wm geometry $w 500x300
    

# path independent names for the widgets

global fileselect_entry fileselect_list fileselect_ok fileselect_cancel 
global colorselect_list colorselect_ok colorselect_entry color_list color
global GISDBASE LOCATION_NAME LOCATION
global fileselect_dirlabel mapsetselect_list mapsetselect_entry mapsetselect_ok global GISDBASE LOCATION MAPSET selected LOCATION_NAME mapset color 
set color black
set color_list "red \
orange yellow green blue indigo white black brown magenta aqua gray grey"

set fileselect_entry $w.file.rightentry.entry
set fileselect_list $w.file.rightscroll.sframe.list
set fileselect_scroll $w.file.rightscroll.sframe.scroll
set fileselect_ok $w.bframe.okframe.ok
set fileselect_cancel $w.bframe.cancel
set fileselect_dirlabel $w.file.dirlabel
set mapsetselect_entry $w.file.leftentry.entry
set mapsetselect_list $w.file.leftscroll.s2frame.list
set mapsetselect_scroll $w.file.leftscroll.s2frame.scroll
set mapsetselect_ok $w.bframe.okframe.ok2
set mapsetselect_cancel $w.bframe.cancel2
set mapsetselect_dirlabel $w.file.dirlabel2

set colorselect_entry $w.file.rightentry2.entry
set colorselect_list $w.file.rightscroll2.sframe.list
set colorselect_scroll $w.file.rightscroll2.sframe.scroll

# widgets

frame $w.file -bd 3 -bg LemonChiffon2 -relief ridge 
frame $w.file.leftentry -bd 3 -bg LemonChiffon2 -relief ridge
frame $w.file.rightentry -bd 3 -bg LemonChiffon2 -relief ridge
frame $w.file.rightentry2 -bd 3 -bg LemonChiffon2 -relief ridge
frame $w.file.leftscroll -bd 3 -bg LemonChiffon2 -relief ridge
frame $w.file.rightscroll -bd 3 -bg LemonChiffon2 -relief ridge
frame $w.file.rightscroll2 -bd 3 -bg LemonChiffon2 -relief ridge
frame $w.bframe -bd 3 -bg LemonChiffon2 -relief ridge
 
place $w.file -x 1 -y 0 -width 498 -height 300  
place $w.file.leftentry -x 0 -y 0 -width 152 -height 55
place $w.file.rightentry -x 152 -y 0 -width 200 -height 55
place $w.file.rightentry2 -x 352 -y 0 -width 140 -height 55

place $w.file.leftscroll -x 0 -y 55 -width 152 -height 180
place $w.file.rightscroll -x 152 -y 55 -width 200 -height 180
place $w.file.rightscroll2 -x 352 -y 55 -width 140 -height 180
place $w.bframe -x 4 -y 239 -width 492 -height 59

frame $w.file.rightentry.eframe -bd 2
frame $w.file.rightentry2.eframe -bd 2
frame $w.file.leftentry.e2frame -bd 2
frame $w.file.rightscroll.sframe -bd 2 -bg LemonChiffon2
frame $w.file.rightscroll2.sframe -bd 2 -bg LemonChiffon2
frame $w.file.leftscroll.s2frame -bd 2 -bg LemonChiffon2

pack $w.file.rightentry.eframe -side left -anchor sw
pack $w.file.rightentry2.eframe -side left -anchor sw
pack $w.file.leftentry.e2frame -side left -anchor sw
pack $w.file.rightscroll.sframe -fill both -expand yes
pack $w.file.rightscroll2.sframe -fill both -expand yes
pack $w.file.leftscroll.s2frame -fill both -expand yes

label $w.file.rightentry.label -text "Select File:" -bg LemonChiffon2
entry $w.file.rightentry.entry -relief sunken -width 23 -bd 2
label $w.file.rightentry2.label -text "Select Color:" -bg LemonChiffon2
entry $w.file.rightentry2.entry -relief sunken -width 15 -bd 2 -textvariable color
label $w.file.leftentry.label -text "Select Mapset:" -bg LemonChiffon2
entry $w.file.leftentry.entry -relief sunken -width 15 -bd 2 -textvariable MAPSET

place $w.file.rightentry.label -x 5 -y 0 
place $w.file.rightentry.entry -x 5 -y 23
place $w.file.rightentry2.label -x 5 -y 0 
place $w.file.rightentry2.entry -x 5 -y 23
place $w.file.leftentry.label -x 5 -y 0
place $w.file.leftentry.entry -x 5 -y 23


scrollbar $w.file.rightscroll.sframe.yscroll -relief sunken -bd 2 -command "$w.file.rightscroll.sframe.list yview" 
listbox $w.file.rightscroll.sframe.list -relief sunken -bd 2 -yscroll "$w.file.rightscroll.sframe.yscroll set"    
    pack $w.file.rightscroll.sframe -side left -fill both
    pack $w.file.rightscroll.sframe.yscroll -side right -fill y -padx 3
    pack $w.file.rightscroll.sframe.list -side left -expand yes -fill x

 listbox $w.file.rightscroll2.sframe.list -relief sunken -bd 2 -yscroll "$w.file.rightscroll.sframe.yscroll set"    
    pack $w.file.rightscroll2.sframe -side left -fill both
    pack $w.file.rightscroll2.sframe.list -side left -expand yes -fill x


scrollbar $w.file.leftscroll.s2frame.yscroll -relief sunken -bd 2 -command "$w.file.leftscroll.s2frame.list yview" 
listbox $w.file.leftscroll.s2frame.list -relief sunken -bd 2 -exportselection false -borderwidth 2 -yscroll "$w.file.leftscroll.s2frame.yscroll set"
    pack $w.file.leftscroll.s2frame -side right -fill both
    pack $w.file.leftscroll.s2frame.yscroll -side right -fill y 
    pack $w.file.leftscroll.s2frame.list -side left -expand yes -fill x
   


####   
 
# buttons
   
    frame $w.bframe.okframe -borderwidth 2 -relief sunken
    frame $w.bframe.cancel -borderwidth 2 -relief sunken
    frame $w.bframe.clear -borderwidth 2 -relief sunken
    frame $w.bframe.clearlast -borderwidth 2 -relief sunken
    button $w.bframe.okframe.ok -text OK -relief raised \
        -command "fileselect.ok.cmd $w $cmd"
    
    button $w.bframe.cancel.b -text Done -relief raised \
        -command "fileselect.cancel.cmd $w"
     
    button $w.bframe.clear.b -text "Clear Display" -relief raised  \
        -command {
puts [exec d.erase color=white]
  }
  
button $w.bframe.clearlast.b -text "Clear Display Last" -relief raised  \
        -command {
puts [exec d.vect map=$selected@$mapset color=white]
  }

     place $w.bframe.okframe -x 262 -y 15 -width 100 -height 25
     place $w.bframe.cancel -x 363 -y 15 -width 100 -height 25
     place $w.bframe.clear -x 161 -y 15 -width 100 -height 25
     place $w.bframe.clearlast -x 30 -y 15 -width 130 -height 25
      
     pack $w.bframe.cancel.b -expand yes -fill both
     pack $w.bframe.okframe.ok -expand yes -fill both
     $w.bframe.okframe.ok configure -state disabled
     pack $w.bframe.clear.b -expand yes -fill both
     pack $w.bframe.clearlast.b -expand yes -fill both


 #######################################################################


 # Fill the listbox with a list of all the mapsets.

  foreach i [exec ls $GISDBASE/$LOCATION_NAME] {
        if {[string length $i] != 0} {
            $mapsetselect_list insert end $i
         }
    }
  

foreach i [exec ls $GISDBASE/$LOCATION_NAME/$MAPSET/dig] {
        if {[string length $i] != 0} {
            $fileselect_list insert end $i
          }
    }


foreach i [exec echo $color_list] {
        if {[string length $i] != 0} {     
            $colorselect_list insert end $i
        }
    } 


#Set up bindings for the browsers.
    
    bind $fileselect_entry <Return> {eval $fileselect_ok invoke}
    bind $fileselect_entry <Control-c> {eval $fileselect_cancel invoke}

    bind $w <Control-c> {eval $fileselect_cancel invoke}
    bind $w <Return> {eval $fileselect_ok invoke}


    tk_listboxSingleSelect $mapsetselect_list

        bind $mapsetselect_list <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
	eval $mapsetselect_entry delete 0 end
	eval $mapsetselect_entry insert 0 [%W get [%W nearest %y]]
        set mapset [$mapsetselect_entry get]
        eval $fileselect_entry delete 0 end
        eval $fileselect_list delete 0 end
       foreach i [exec ls $GISDBASE/$LOCATION_NAME/$MAPSET/dig] {
       if {[string length $i] != 0} {
            $fileselect_list insert end $i
            }
        }
   }
    tk_listboxSingleSelect $fileselect_list
     
        bind $fileselect_list <ButtonRelease-1> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
        eval $fileselect_entry delete 0 end
	eval $fileselect_entry insert 0 [%W get [%W nearest %y]]
        eval $fileselect_ok configure -state normal -foreground black
     }


   tk_listboxSingleSelect $colorselect_list
     
        bind $colorselect_list <Button> {
        %W select from [%W nearest %y]
        %W select to [%W nearest %y]
        eval $colorselect_entry delete 0 end
	eval $colorselect_entry insert 0 [%W get [%W nearest %y]]
        set color [$colorselect_entry get]
       

    }


focus $fileselect_entry
 proc fileselect.ok.cmd {w cmd} {
    global fileselect_entry fileselect_dirlabel selected
    
    set selected [$fileselect_entry get]

    
    #destroy $w
    
eval $cmd $selected

        }
    }

# auxiliary button procedures

 proc fileselect.cancel.cmd {w} {
     puts stderr "Cancel"
    destroy $w
}

#######################################################################

#if {[info exists selected] !=0} {
#else {
#frame .c -relief ridge -width 200 -height 100 
#message .c.msg -font -Adobe-Times-Medium-R-Normal-*-180-* -width 420 \
            -relief raised -bd 2 -text "Please select a file."
#pack .c
#              }







   
