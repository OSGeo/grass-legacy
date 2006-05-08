# Updated 18-October-2005 by Michael Barton, Arizona State University
# menu.tcl
# produces menu bar for d.m

global tmenu
global keyctrl
global execom 

source "$env(GISBASE)/etc/gui/menus/menu.tcl"

# Put this at the bottom of the file menu.
set GuiMenu::Menu_File_Bottom [subst  {
	{separator}
	{cascad {[G_msg "Groups"]} {} "" $tmenu {			
		{command {[G_msg "New"]} {} {[G_msg "Create new group file"]} {} -accelerator $keyctrl-N -command { Dm::new}}
		{command {[G_msg "Open..."]} {} {[G_msg "Open group file"]} {} -accelerator $keyctrl-O -command { Dm::OpenFileBox {}}}
		{command {[G_msg "Save"]} {} {[G_msg "Save group file"]} {} -accelerator $keyctrl-S -command { Dm::SaveFileBox {}}}
		{command {[G_msg "Save as..."]} {} {[G_msg "Save group file as name"]} {} -command { catch {unset ::Dm::filename} ; Dm::SaveFileBox {}}}
		{command {[G_msg "Close"]} {} {[G_msg "Close group"]} {} -accelerator $keyctrl-W -command { Dm::FileClose {}}}
	}}
	{separator}
	{cascad {[G_msg "Save display to image file"]} {} "" $tmenu {			
		{command {[G_msg "XWD (Save display, selected with mouse, to map.xwd in home directory )"]} {} "" {} -command { spawn xwd -out map.xwd }}
		{command {[G_msg "Save displays to multiple graphic file formats"]} {} "d.out.file" {} -command { execute d.out.file }}
	}}
	{command {[G_msg "Save map to Postscript file"]} {} "ps.map" {} -command { execute ps.map }}
	{command {[G_msg "Print to default printer"]} {} {[G_msg "print"]} {} -accelerator $keyctrl-P -command {spawn print.sh} }
	{separator}
	{command {[G_msg "E&xit"]} {} {[G_msg "Exit Display Manager"]} {} -accelerator $keyctrl-Q -command { DmPrint::clean; exit } }
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
	{command {[G_msg "GIS Manager &help"]} {} {[G_msg "GIS Manager help"]} {} -command { exec g.manual d.m > /dev/null & } }
	{command {[G_msg "About &GRASS"]} {} {[G_msg "About GRASS"]} {} -command { source $env(GISBASE)/etc/dm/grassabout.tcl} }
	{command {[G_msg "About &System"]} {} {[G_msg "About System"]} {} -command { exec $env(GRASS_WISH) $env(GISBASE)/etc/dm/tksys.tcl --tcltk & }}
 }]

