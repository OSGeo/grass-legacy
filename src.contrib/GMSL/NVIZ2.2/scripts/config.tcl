# This script is setup by the toplevel configure shell script at
# installation time

 global src_boot

 set gisbase $env(GISBASE)
 set default_panel_path "$gisbase/etc/nviz2.2/scripts"
 set bit_map_path "$gisbase/etc/nviz2.2/bitmaps"

# Set up auto_path directories
if {[catch {set env(Nviz_PanelPath)} user_path]} then {
    set user_path [list]
} else {
    set user_path [split $user_path :]
}

# If the -path option was used then append that directory also
if {[catch {set NvizAltPath}] == 0} then {
    global NvizAltPath
    lappend user_path $NvizAltPath
}

if {[lsearch -exact $user_path "$default_panel_path"] == -1} then {
    set user_path [linsert $user_path -1 "$default_panel_path"]
}
foreach i $user_path {
    lappend auto_path $i
}

# add the execution directory to the path
set env(PATH) "$default_panel_path:$env(PATH)"

# Override bindings for tk widgets
source $src_boot/etc/nviz2.2/scripts/extra_bindings.tcl

##########################################################################
#  Resources
##########################################################################
option add *background gray90 widgetDefault
option add *activeBackground gray80 widgetDefault
option add *font -*-helvetica-medium-r-normal-*-12-*-iso8859-1 widgetDefault
option add *Label*font -*-helvetica-bold-r-normal-*-12-*-iso8859-1 widgetDefault
option add *Radiobutton*relief flat
option add *Checkbutton*relief flat
option add *Scrollbar*troughcolor gray90 widgetDefault
option add *Scrollbar*background gray90 widgetDefault
option add *Scrollbar*activeBackground gray99 widgetDefault



