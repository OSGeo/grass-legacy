package require cmdline

# An OK button message box
proc okbox { msg } {
    tk_messageBox -icon info -message $msg  -type ok
}

source showamap.tcl

# Do this better as errors in finding 
# DLL's are otherwise not reported and/or
# dealt with leading to unexplained problems
# when DLL functions are referenced.
if {![string compare $tcl_platform(platform) windows]} {
    load libgis.dll libgis
} else {
    load ./libgis.so libgis
}

# okbox $env(HOME)

if { [catch {set a $env(HOME)} gisrcdir] } {
    okbox "You don't have a HOME environment variable, please specify where you want to keep GRASS initialisation information"
    set gisrcdir [tk_chooseDirectory -title "Select your Grass initialisation directory"]
    set env(HOME) $gisrcdir
}

# okbox "gisrcdir = $gisrcdir"
# okbox "HOME     = $env(HOME)"

if { [catch {set a $env(GISBASE)}] } {
    set env(GISBASE) [tk_chooseDirectory -title "Select your Grass installation directory"]
}

# set env(GISBASE) e:/cvs/grassmingw/dist.i686-pc-mingw32

# GIS_LOCK pid of startup shell script
# GISRC GRASS environment file
set env(GIS_LOCK) [pid]
set h $env(HOME)
set env(GISRC) $h/.grassrc5
# okbox "GIS_LOCK = $env(GIS_LOCK),  GISRC = $env(GISRC)"

set env(LOCATION_NAME) spearfish
set env(GISDBASE) e:/data/grass
set cmdname [::cmdline::getArgv0]

okbox "GISBASE = $env(GISBASE),  Command name to be used for init: $cmdname"
if { [catch {G_gisinit $cmdname} res ] } {
    okbox "G_gisinit exception: $res"
}
# okbox Initted

# GISBASE top level grass module dir

# $(HOME)/.grassrc5 holds next three vars
# GISDBASE - directory in which locations appear
#set dbmapset [G_gisdbase]/[G_location]/[G_mapset]
#okbox "Our mapset: $dbmapset"

showmap geology 1

# Path to current location 
# G_location_path

# Current and default region from WIND file
# G_get_window
# G_get_default_window

# descriptive name for location
# G_myname

# program name
# G_program_name

