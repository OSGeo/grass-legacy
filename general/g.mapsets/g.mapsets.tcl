#!/bin/sh
# the next line restarts using wish \
exec $GRASS_WISH "$0" "$@"

lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1

set env(GISDBASE) [exec g.gisenv get=GISDBASE]
set env(LOCATION_NAME) [exec g.gisenv get=LOCATION_NAME]
set env(MAPSET) [exec g.gisenv get=MAPSET]

proc set_mapsets { } {
    global ms_ch ms_name nms

    set first 1
    set cmd "g.mapsets mapset="
    for {set i 0} {$i < $nms} {incr i 1} {
        set ms $ms_name($i)
        if { $ms_ch($ms) } { 
            puts "new ms = $ms"
            if { !$first } {  append cmd "," }
            append cmd "$ms"
            set first 0
        }
    }
    eval exec $cmd
}

set sw [ScrolledWindow .sw -relief sunken -borderwidth 2]
set sf [ScrollableFrame .sf -width 150 -height 300]
$sw setwidget $sf
pack $sw $sf -fill both -expand yes
set sframe [$sf getframe]

set msts [split [exec g.mapsets -l] " \n"]
set nms 0
foreach ms $msts {
    if { [string length $ms] == 0 } { continue }
    set fr [frame $sframe.f$nms]
    pack $fr -side top -anchor w 
    set cb [checkbutton $fr.cb -text "$ms" -variable ms_ch($ms) -command set_mapsets]
    pack $cb -side left
    set ms_name($nms) $ms
    incr nms
}

# check in selected
set msts [split [exec g.mapsets -p] " \n"]
foreach ms $msts {
    if { [string length $ms] == 0 } { continue }
    set ms_ch($ms) 1
}

