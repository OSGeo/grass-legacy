#!/usr/local/bin/wish -f


 catch {destroy .w}

    toplevel .w
    grab .w
    wm title .w "Zoom"
    wm geometry .w +600+177
    wm geometry .w 358x125

set auto_path "$tk_library/demos $auto_path"

frame .w.zoom -relief ridge -bd 2 -bg LemonChiffon2
place .w.zoom -x 1 -y 1 -width 356 -height 124
 
frame .w.zoom.bot -relief ridge -bd 2
place .w.zoom.bot -x 5 -y 3 -width 343 -height 30

frame .w.zoom.bot.zoom -relief ridge -bd 2
place .w.zoom.bot.zoom -x 0 -y 0 -width 76 -height 26

frame .w.zoom.bot.reset -relief ridge -bd 2
place .w.zoom.bot.reset -x 77 -y 0 -width 187 -height 26

frame .w.zoom.bot.done -relief ridge -bd 2
place .w.zoom.bot.done -x 265 -y 0 -width 76 -height 26



button .w.zoom.bot.zoom.left -text "Zoom" -command {
puts stdout [exec xterm -bw 2 -bd yellow -bg LemonChiffon2 -fg black -geometry +600+259 -T "Zoom" -e d.zoom]
exec d.erase white

global color cmd selected type size 

if {[info exists cmd] != 0} { 
if {$cmd == "d.rast"} {
puts stdout [exec xterm -iconic -e $cmd $selected]
} elseif {$cmd == "d.vect"} {
puts [exec xterm -iconic -e $cmd $selected color=$color]
} elseif {$cmd == "d.sites"} {
puts [exec xterm -iconic -e $cmd $selected color=$color size=$size type=$type]
       }
    }
 }

button .w.zoom.bot.reset.right -text "Return to Default Region" -command {
puts [exec xterm -iconic -e g.region -d
exec d.erase color=white]

global color cmd selected type size

if {[info exists cmd] != 0} { 
if {$cmd == "d.rast"} {
puts [exec xterm -iconic -e $cmd $selected]
} elseif {$cmd == "d.vect"} {
puts [exec xterm -iconic -e $cmd map=$selected color=$color]
} elseif {$cmd == "d.sites"} {
exec [exec xterm -iconic -e $cmd $selected color=$color size=$size type=$type]  
    }
 } 
}


button .w.zoom.bot.done.mid -text "Done" -command {destroy .w}
pack .w.zoom.bot.zoom.left -side left -fill both -expand yes 
pack .w.zoom.bot.reset.right -side left -fill both -expand yes
pack .w.zoom.bot.done.mid -side left -fill both -expand yes 



