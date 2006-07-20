###############################################################
# gmmenu.tcl - menu file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
###############################################################

# our job is simply to make a variable called descmenu

source "$env(GISBASE)/etc/gui/menus/menu.tcl"


global tmenu
global keyctrl
global execom 
global mon
global filename
global env

# Put this at the top of the file menu.
set GuiMenu::Menu_File_Top [subst  {
	{cascad {[G_msg "Workspace"]} {} "" $tmenu {			
		{command {[G_msg "Open..."]} {} "Open gis.m workspace file" {} -accelerator $keyctrl-O -command { Gm::OpenFileBox }}
		{command {[G_msg "Save"]} {} "Save gis.m workspace file" {} -accelerator $keyctrl-S -command { Gm::SaveFileBox }}
		{command {[G_msg "Save as..."]} {} "Save gis.m workspace file as new name" {} -command { set filename($mon) "" ; Gm::SaveFileBox }}
		{command {[G_msg "Close"]} {} "Close gis.m workspace" {} -accelerator $keyctrl-W -command { GmTree::FileClose {}}}
	}}
}]

# Put this at the bottom of the file menu.
set GuiMenu::Menu_File_Bottom [subst  {
	{separator}
	{command {[G_msg "Georectify"]} {} "Georectify raster map in xy location" {} -command { GRMap::startup }}
	{separator}
	{command {[G_msg "Create ps.map file for postscript printing"]} {} "ps.map" {} -command { execute ps.map }}
	{separator}
	{command {[G_msg "E&xit"]} {} "Exit GIS Manager" {} -accelerator $keyctrl-Q -command { exit } }
}]

# Get the big whopping tree of menu doom
set descmenu [GuiMenu::tree]

# add help menu to the end of the menubar 
lappend descmenu [G_msg "&Help"]
lappend descmenu all
lappend descmenu options
lappend descmenu $tmenu
lappend descmenu [subst {
	{command {[G_msg "GRASS help"]} {} "g.manual" {} -command { exec g.manual -i > /dev/null & } }
	{command {[G_msg "GIS Manager &help"]} {} {[G_msg "GIS Manager help"]} {} -command { exec g.manual gis.m > /dev/null & } }
	{command {[G_msg "About &GRASS"]} {} {[G_msg "About GRASS"]} {} -command { source $env(GISBASE)/etc/gm/grassabout.tcl} }
	{command {[G_msg "About &System"]} {} {[G_msg "About System"]} {} -command { exec $env(GRASS_WISH) $env(GISBASE)/etc/gm/tksys.tcl --tcltk & }}
 }]

