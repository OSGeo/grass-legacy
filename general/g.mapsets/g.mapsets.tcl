#!/bin/sh
# the next line restarts using wish \
exec $GRASS_WISH "$0" "$@"

# Put current mapset and PERMANENT at the top of the list, mapsets are later written to SEARCH_PATH
# and searched in this order

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
            if { !$first } {  append cmd "," }
            append cmd "$ms"
            set first 0
        }
    }
    puts  $cmd
    eval exec $cmd
}

set mainw [ScrolledWindow .mainw -relief sunken -borderwidth 2]

set sw [ScrolledWindow .sw -relief sunken -borderwidth 2]
set sf [ScrollableFrame .sf -width 150 -height 300]
$sw setwidget $sf

pack $sw $sf -fill both -expand yes
set sframe [$sf getframe]


# Add current mapset and PERMANENT
set current [exec g.gisenv get=MAPSET]
set msts [list $current PERMANENT]

set tmp_msts [ lsort [split [exec g.mapsets -l] " \n"] ]
foreach ms $tmp_msts {
    if { $ms == $current || $ms == "PERMANENT" } { continue }
    lappend msts $ms 
}

set nms 0
foreach ms $msts {
    if { [string length $ms] == 0 } { continue }
    set fr [frame $sframe.f$nms]
    pack $fr -side top -anchor w 
    set cb [checkbutton $fr.cb -text "$ms" -variable ms_ch($ms) -command set_mapsets]
    if { $ms == $current } { 
	$cb configure -state disabled 
    }
    pack $cb -side left
    set ms_name($nms) $ms
    incr nms
}

# check in selected
set msts [split [exec g.mapsets -p] " \n"]
foreach ms $msts {
    if { $ms == $current } { 
        set ms_ch($ms) 1
        continue
    }
    if { [string length $ms] == 0 } { continue }
    set ms_ch($ms) 1
}

set close [button $mainw.close -text "Close" -command { exit } ]
pack $mainw
pack $close -side bottom
