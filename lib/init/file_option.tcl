#=====================================================================================
#   
#            FILE:  file_option.tcl
#   
#     DESCRIPTION:  creates location from georeferenced file
#  
#           NOTES:  ---
#          AUTHOR:  Michael Barton (Based on epsg_option.tcl by Antonello Andrea)
#         COMPANY:  Arizona State University
#       COPYRIGHT:  Copyright (C) 2006 Michael Barton and GRASS Development Team
#         VERSION:  1.0
#         CREATED:  23/04/2006
#        REVISION:  --- 
#=====================================================================================
#
#
# 
#  This library is free software; you can redistribute it and/or 
#  modify it under the terms of the GNU Library General Public 
#  License as published by the Free Software Foundation; either 
#  version 2 of the License, or (at your option) any later version. 
#
#  This library is distributed in the hope that it will be useful, 
#  but WITHOUT ANY WARRANTY; without even the implied warranty of 
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
#  Library General Public License for more details. 
#
#  You should have received a copy of the GNU Library General Public 
#  License along with this library; if not, write to the Free 
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 
#  USA 
#
#1. Redistributions of source code must retain the above copyright   
#   notice, this list of conditions and the following disclaimer.   
#2. Redistributions in binary form must reproduce the above copyright   
#   notice, this list of conditions and the following disclaimer in the   
#   documentation and/or other materials provided with the distribution.   
# 
#############################################################################

global browsedepsg 

# G_msg.tcl should be sourced first for internationalized strings.

# the frame used to set EPSG parameters 
proc fileLocCom args {
	#vars declaration
	global database
	global fileLocation 
	global filepath
	global env
	global thelocation
	global locpath
	
	set fileLocation "newLocation"
	set filepath ""
	set locpath $database
	set buttonstate "disabled"
	
	# creation of the parameter window
	toplevel .fileloc
	wm title .fileloc [G_msg "Define location using projection information in georeferenced file"]
	
	# put it in the middle of the screen
	update idletasks
	set winWidth [winfo reqwidth .fileloc]
	set winHeight [winfo reqheight .fileloc]
	set scrnWidth [winfo screenwidth .fileloc]
	set scrnHeight [winfo screenheight .fileloc]
	set x [expr ($scrnWidth - $winWidth) / 2-250]
	set y [expr ($scrnHeight  - $winHeight) / 2]
	wm geometry .fileloc +$x+$y
	wm deiconify .fileloc
	
	#create the form and buttons
	set loclab [label .fileloc.lab1 -text [G_msg "Name of new location"] -justify right -height 2]
	set locname [entry .fileloc.loc -textvariable fileLocation -width 35 -bg white]
	set dblab [label .fileloc.lab2 -text [G_msg "Path to new location"] -justify right -height 2]
	set dbpath [entry .fileloc.locpath -textvariable locpath -width 35 -bg white]
	set filelab [label .fileloc.lab3 -text [G_msg "Path to georeferenced file"] -justify right -height 2]
	set fpath [entry .fileloc.filepath -textvariable filepath  -width 35 -bg white]
	
	#browse for database path
	set dbbrowse [button .fileloc.dbbrow -justify center -width 12 \
		-text [G_msg "Browse..."] -command "set locpath \[tk_chooseDirectory -initialdir locpath -parent .fileloc \
		-title \[G_msg \"Choose path to new location\"\] -mustexist true\]" ]

        bind $dbpath <Leave> {
             if {$locpath == ""} {
                set locpath $database
            }
        }

																									
	set helpbutton [button .fileloc.help -justify center -bg honeydew2 -text [G_msg "Help"] \
		-command {infofileloc}]

	pack $helpbutton -side left -fill both -expand 0
	
	#define button to define location
	set locdefbutton [button .fileloc.def -justify center -width 15 \
		-text [G_msg "Define location"] -state $buttonstate \
		-command {
			set thelocation "$locpath/$fileLocation";
			if {[file exists $thelocation ]== 1} {
				DialogGen .wrnDlg [G_msg "WARNING: location exists"] warning \
				[G_msg "WARNING: The location '$thelocation' already exists, please try another name"] \
				0 OK;
			}
			if {[file exists $filepath]== 0} {
				DialogGen .wrnDlg [G_msg "WARNING: file not found"] warning \
				[G_msg "WARNING: The file was not found!"] \
				0 OK;
			}
			if {[file exists $filepath]== 1} {
				if {[file exists $thelocation ]==0} {  
					destroy .fileloc; 
					exec -- $env(GISBASE)/etc/grass-xterm-wrapper -T g.proj -n g.proj -e $env(GISBASE)/etc/grass-run.sh g.proj -c georef=$filepath location=$fileLocation
					DialogGen .wrnDlg [G_msg "WARNING: restart GRASS please"] warning \
					[G_msg "WARNING: Please restart GRASS in order find the created location in the list (closing it for you now)"] \
						0 OK; 
					puts stdout "exit";
					destroy . 
				}
			}
			set thelocation ""
		 }]
		 
	bind .fileloc.filepath <KeyRelease> {
		.fileloc.def configure -state active
	}

	#browse for georeferenced file
	set filebrowse [button .fileloc.fbrow -justify center -width 12 \
		-text [G_msg "Browse..."] -command {
				set filepath [tk_getOpenFile -parent .fileloc -title [G_msg "Choose georeferenced file"]]
				if {$filepath != ""} {
					.fileloc.def configure -state active
				}
			} ]

	pack $locdefbutton -side left -fill both -expand 0
	
	set cancelbutton [button .fileloc.cancel -justify center -width 15 -text [G_msg "Cancel"] \
					-command {destroy .fileloc}]
	pack $cancelbutton -side left -fill both -expand 0

	# geometry
	grid $loclab -row 0 -column 0 -sticky e
	grid $locname -row 0 -column 1 -columnspan 2
	grid $dblab -row 1 -column 0 -sticky e
	grid $dbpath -row 1 -column 1 -columnspan 2
	grid $filelab -row 2 -column 0 -sticky e
	grid $fpath -row 2 -column 1 -columnspan 2
	grid $dbbrowse -row 1 -column 3 
	grid $filebrowse -row 2 -column 3 
	grid $locdefbutton -row 3 -column 0
	grid $cancelbutton -row 3 -column 1
	grid $helpbutton -row 3 -column 2
}


# help for the EPSG Location creation
proc infofileloc args {

        toplevel .infoPopup
        wm title .infoPopup {Info}
        update idletasks
        set winWidth [winfo reqwidth .infoPopup]
        set winHeight [winfo reqheight .infoPopup]
        set scrnWidth [winfo screenwidth .infoPopup]
        set scrnHeight [winfo screenheight .infoPopup]
        set x [expr ($scrnWidth - $winWidth) / 2-230]
        set y [expr ($scrnHeight  - $winHeight) / 2]
        wm geometry .infoPopup +$x+$y
        wm deiconify .infoPopup
        
        text .infoPopup.text -width 40 -height 30\
                   -wrap word \
                   -relief raised \
                   -yscrollcommand ".infoPopup.vscroll set"
                   
        # tag configuration
        .infoPopup.text tag configure underline -underline 1
        .infoPopup.text tag configure title -relief raised -borderwidth 2 -background grey -justify center
        .infoPopup.text tag configure subtitle -relief flat -borderwidth 1 
        .infoPopup.text tag configure info -relief sunken -borderwidth 1 -background white 
    
        # the text to be inserted
        .infoPopup.text insert end [G_msg " \nCREATING A NEW GRASS LOCATION USING GEOREFERENCED FILE\n\n "] title
        .infoPopup.text insert end [G_msg "The file must have georeferencing information readable\n"] subtitle
        .infoPopup.text insert end [G_msg "by GDAL or OGR, and GRASS must be compiled with GDAL and OGR.\n\n"] subtitle
        .infoPopup.text insert end [G_msg "\n   Name of new location:\n\n"] subtitle
        .infoPopup.text insert end [G_msg "\nRequires as input the name of the new location to be created\n\n"] info
        .infoPopup.text insert end "\n" 
        .infoPopup.text insert end [G_msg "\n   Path to location:\n\n"] subtitle
        .infoPopup.text insert end [G_msg "\nThe folder (Grass database) in which the location should be created\n\n"] info
        .infoPopup.text insert end "\n" 
        .infoPopup.text insert end [G_msg "\n   Path to georeferenced file:\n\n"] subtitle
        .infoPopup.text insert end [G_msg "\nGeoreferenced file with projection information that can be read\n"] info
        .infoPopup.text insert end [G_msg "\nby GDAL (raster) or OGR (vector)\n\n"] info
        
        pack .infoPopup.text -side left -fill both 
        
        scrollbar .infoPopup.vscroll \
                    -relief raised \
                    -command ".infoPopup.text yview"
        pack .infoPopup.vscroll -side right -fill y
        
        button .infoPopup.ex -justify center -width 6 -text [G_msg "OK"] \
                             -command {destroy .infoPopup}			
        pack .infoPopup.ex -side bottom -expand 1 -fill both
}


