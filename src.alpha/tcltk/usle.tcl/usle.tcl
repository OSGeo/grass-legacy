#!/usr/local/bin/wish -f

# Grass variables
# set env(lockfile) $env(HOME)/.gislock
# set env(GISRC) $env(HOME)/.grassrc
# set env(PAINTER) preview

set GISBASE "$env(GISBASE)"
set HOME "$env(HOME)"
set GISDBASE "$env(GISDBASE)"
set LOCATION "$env(LOCATION)"
set LOCATION_NAME "$env(LOCATION_NAME)"
set MAPSET "$env(MAPSET)"
set PATH "$env(PATH)"
set MONITOR " "

global GISDBASE LOCATION MAPSET LOCATION_NAME

#--------------------------------------------------
set auto_path "tk_library/demos $auto_path"
wm title . "USLE Demonstration"
wm geometry . +600+0

frame .menu -relief raised -borderwidth 1

message .menu.msg -font -Adobe-times-medium-r-normal--*140* -relief ridge \
-width 500 -borderwidth 3 -background LemonChiffon2 -padx 20 -pady 20 -text "A basic TCL/TK interface (X windows tool) for GRASS developed by Bill Jackson of the Land Inventory and Monitoring Team and Keith Mitchell of the Threatened and Endangered Species Team in the EN Division at USACERL."


                                                                                                                                                                                                                    
pack .menu -side top -fill x
pack .menu.msg -side bottom -expand yes -fill both

exec /usr/bin/xterm -iconic -e ./start.xmon.sh

menubutton .menu.dis -text "Display" -menu .menu.dis.m -underline 0

menu .menu.dis.m 
.menu.dis.m add cascade -label "GRASS Display" -menu .menu.dis.m.grass -underline 0

.menu.dis.m add cascade -label "Map Selection" -menu .menu.dis.m.map \
-underline 0

menu .menu.dis.m.map
.menu.dis.m.map add radiobutton -label "Select Raster Map for Display" \
-command { 
puts stdout [source sel_rast.tcl
fileselect]
}

.menu.dis.m.map add radiobutton -label "Select Vector Map for Display" -command { 
puts stdout [source sel_vect.tcl
fileselect]
}
.menu.dis.m.map add radiobutton -label "Select Sites Map for Display" -command {
puts stdout [source sel_sites.tcl
fileselect]
}


.menu.dis.m add command -label "Zoom" -command { 
exec g.region save=last
puts stdout [source zoom.tcl] 
}

menu .menu.dis.m.grass
.menu.dis.m.grass add cascade -label "Start GRASS Display" -menu .menu.dis.m.grass.start -underline 0 

.menu.dis.m.grass add cascade -label "Select GRASS Display" -menu \ .menu.dis.m.grass.select -underline 0

.menu.dis.m.grass add cascade -label "Stop GRASS Display" -menu .menu.dis.m.grass.stop -underline 0  

.menu.dis.m.grass add command -label "Clear Display" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.erase color=white]
}

.menu.dis.m.grass add command -label "Clear Display and Reset Default Region" -command {
puts stdout [exec g.region -d]
puts stdout [exec d.erase color=white]
}

menu .menu.dis.m.grass.start
.menu.dis.m.grass.start add radiobutton -label "X0" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon start=x0 
puts "GRASS Display Ready"
exec d.erase color=white]
}
.menu.dis.m.grass.start add radiobutton -label "X1" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon start=x1 
puts "GRASS Display Ready"
exec d.erase color=white]
}
.menu.dis.m.grass.start add radiobutton -label "X2" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon start=x2 
puts "GRASS Display Ready"
exec d.erase color=white]
}
.menu.dis.m.grass.start add radiobutton -label "X3" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon start=x3 
puts "GRASS Display Ready"
exec d.erase color=white]
}
.menu.dis.m.grass.start add radiobutton -label "X4" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon start=x4 
puts "GRASS Display Ready"
exec d.erase color=white]
}
.menu.dis.m.grass.start add radiobutton -label "X5" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon start=x5 
puts "GRASS Display Ready"
exec d.erase color=white]
}
.menu.dis.m.grass.start add radiobutton -label "X6" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon start=x6 
puts "GRASS Display Ready"
exec d.erase color=white]
}


menu .menu.dis.m.grass.select 
.menu.dis.m.grass.select add radiobutton -label "X0" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon select=x0]
} 
.menu.dis.m.grass.select add radiobutton -label "X1" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon select=x1]
} 
.menu.dis.m.grass.select add radiobutton -label "X2" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon select=x2]
}
 .menu.dis.m.grass.select add radiobutton -label "X3" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon select=x3]
}
 .menu.dis.m.grass.select add radiobutton -label "X4" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon select=x4]
}
 .menu.dis.m.grass.select add radiobutton -label "X5" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon select=x5]
}
 .menu.dis.m.grass.select add radiobutton -label "X6" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon select=x6]
}

menu .menu.dis.m.grass.stop
.menu.dis.m.grass.stop add radiobutton -label "X0" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon stop=x0]
}
.menu.dis.m.grass.stop add radiobutton -label "X1" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon stop=x1]
}
.menu.dis.m.grass.stop add radiobutton -label "X2" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon stop=x2 ]
}
.menu.dis.m.grass.stop add radiobutton -label "X3" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon stop=x3]
}
.menu.dis.m.grass.stop add radiobutton -label "X4" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon stop=x4]
}
.menu.dis.m.grass.stop add radiobutton -label "X5" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon stop=x5]
}
.menu.dis.m.grass.stop add radiobutton -label "X6" -command {
puts stdout [exec /usr/bin/xterm -iconic -e d.mon stop=x6]
}


menubutton .menu.inquire -text "Query Tools" -menu .menu.inquire.m -underline 0
menu .menu.inquire.m
 
# .menu.inquire.m add cascade -label "Description and Category Value" -menu \ .menu.inquire.m.cats -underline 0
.menu.inquire.m add cascade -label "Description and Category Value" -menu \ .menu.inquire.m.cats 

menu .menu.inquire.m.cats
.menu.inquire.m.cats add radiobutton -label "Raster" -command {
global selected mapset
if {[info exists selected] !=0} {
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -T "Raster Category Value" -e d.what.rast]
} else { 
puts [exec xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177
puts "Please select raster map"]
   }
} 

.menu.inquire.m.cats add radiobutton -label "Vector" -command {
global selected
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -T "Vector Category Value" -e d.what.vect map=$selected]
}

.menu.inquire.m.cats add radiobutton -label "Sites" -command {
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black \
-geometry +600+177 -T "Sites Category Value" -e d.what.site]
}

.menu.inquire.m add cascade -label "Reports" -menu .menu.inquire.m.report -underline 0


#######################################################################
# raster report 
######################################################################
menu .menu.inquire.m.report
.menu.inquire.m.report add radiobutton -label "Raster" -command {
global selected mapset
if {[file exists tmp] == 1} {
puts stdout [exec rm tmp]
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -e r.report map=$selected u=h,a output=tmp]
puts stdout [exec /usr/bin/xterm -iconic -e textedit tmp]
} elseif {[file exists tmp] == 0} {
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -e r.report map=$selected u=h,a output=tmp]
puts stdout [exec /usr/bin/xterm -iconic -e textedit tmp]
} else {
  }
}

.menu.inquire.m.report add radiobutton -label "Vector" -command {
global selected mapset
if {[ file exists tmp ] == 1} {
puts stdout [exec rm tmp]
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -e v.report map=$selected type=line units=feet > tmp]
puts stdout [exec /usr/bin/xterm -iconic -e textedit tmp]
} elseif {[ file exists tmp ] == 0} {
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -e v.report map=$selected type=line units=feet > tmp]
puts stdout [exec /usr/bin/xterm -iconic -e textedit tmp ]
} else {
  }
}

.menu.inquire.m.report add radiobutton -label "Sites" -command {
global selected mapset
if {[ file exists tmp ] == 1} {
puts stdout [exec rm tmp]
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -e s.menu]
puts stdout [exec /usr/bin/xterm -iconic -e textedit tmp]
} elseif {[ file exists tmp ]==0} {
puts stdout [exec /usr/bin/xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+177 -e s.menu]
puts stdout [exec /usr/bin/xterm -iconic -e textedit tmp]
} else {
   }
}

button .menu.quit -relief flat -text "Quit" -command {
destroy .
}

pack .menu.dis -side left -padx 5
pack .menu.inquire -side left -padx 5
#pack .menu.shell -side left -padx 5
pack .menu.quit -side left -padx 5


