#!/bin/sh
# the next line restarts using tclsh \
exec $GRASS_WISH "$0" "$@"

global env
global argv
global URL
set URL $argv
puts "Using $URL"

source $env(GISBASE)/documents/nviz/help.tcl

proc make_help_window { w } {

        frame $w
	frame $w.f2
	frame $w.f1

        pack $w -side top -expand 1 -fill both
	pack $w.f1 -side top -expand 1 -fill both
	pack $w.f2 -side bottom -expand 0 -fill both

}



set w .help_menu

wm title . "Loading Files ... "

catch {make_help_window $w}
help::init $URL "" $w.f1 500 400

wm title . "NVIZ Help"