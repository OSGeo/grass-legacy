#!/usr/bin/tclsh

global errorCode

set a [cd;pwd]
puts $a
set env(GIS_LOCK) [pid]
puts $env(HOME)
puts $env(GIS_LOCK)

catch {exec /home/grass/tmp/run} a
set b [string index $errorCode [expr [string length $errorCode] - 1]]
puts "$b = $errorCode = $a"

set hm $env(HOME)
set env(HOME) /usr/local/grassCVS/grass/src/general/gis
set env(TESTER) gfdert
exec xterm &
set env(HOME) $hm
