#=====================================================================================
#   
#            FILE:  file_option.tcl
#   
#     DESCRIPTION:  creates location from georeferenced file
#  
#           NOTES:  ---
#          AUTHOR:  Michael Barton
#         COMPANY:  Arizona State University
#       COPYRIGHT:  Copyright (C) 2007 Michael Barton and GRASS Development Team
#         VERSION:  1.2
#         CREATED:  23/04/2006
#        REVISION:  --- 
#       CHANGELOG:  1.0.1 08/12/2006 - Fixed directory choosing dialogs. Maris Nartiss.
#			     :	1.2 - 6 Jan 2007 - Fixed file creation for windows and reformatted
#					dialog widgets (Michael Barton).
#				 	Added check for return status of g.proj to catch failed location 
#					creation (by Maris Nartiss).
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
namespace eval fileOpt {
	variable fileLocation #name of new location to be created
	variable filepath  #path to georeferenced file
	global env
	global database 
	global mingw #test to see if we are running a windows version in mingw
	global refresh
}


# G_msg.tcl should be sourced first for internationalized strings.

# the frame used to set parameters 
proc fileOpt::fileLocCom args {
	#vars declaration
	variable filepath
	variable fileLocation
	global database
	global env
	
	set fileLocation "newLocation"
	set filepath ""
	set locpath $database
	set buttonstate "disabled"
	
	# creation of the parameter window
	set file_win [toplevel .fileloc]
	wm title $file_win [ G_msg "Define location using projection information in georeferenced file" ] 
	
	# put it in the middle of the screen
	update idletasks
	set winWidth [winfo reqwidth $file_win]
	set winHeight [winfo reqheight $file_win]
	set scrnWidth [winfo screenwidth $file_win]
	set scrnHeight [winfo screenheight $file_win]
	set x [expr ($scrnWidth - $winWidth) / 2-250]
	set y [expr ($scrnHeight  - $winHeight) / 2]
	wm geometry $file_win +$x+$y
	wm deiconify $file_win
	
	set row1 [frame $file_win.row1]
	set row2 [frame $file_win.row2]
	set row3 [frame $file_win.row3]
	set row4 [frame $file_win.row4]


	#create the form and buttons
	LabelEntry $row1.newloc -label [G_msg "Name of new location"] \
		-labeljustify right -labelanchor e -labelwidth 30 \
		-textvariable fileOpt::fileLocation -width 35 \
		-helptext [G_msg "Enter name of location to be created"]
		
	pack $row1.newloc -side left -expand 0 -fill x -padx 2

	LabelEntry $row2.filepath -label [G_msg "Path to georeferenced file"] \
		-labeljustify right -labelanchor e -labelwidth 30 \
		-textvariable fileOpt::filepath  -width 35 \
		-helptext [G_msg "Path to georeferenced file (format must be readable by GDAL/OGR)"]
		
	#browse for georeferenced file
	Button $row2.browsefile -justify center -width 10 -bd 1 -text [G_msg "Browse..."] \
		-helptext [G_msg "Browse to locate georeferenced file"] \
		-command "fileOpt::browse_file"
		
	pack $row2.filepath $row2.browsefile -side left -expand 0 -fill x -padx 2

	Button $row3.submit -justify center -width 15 -text [G_msg "Define location"] \
		-command "fileOpt::def_loc" -bd 1
				
	Button $row3.cancel -justify center -width 15 -text [G_msg "Cancel"] \
		-command {destroy .fileloc} -bd 1
		
	pack $row3.submit -side left -fill x -expand 0
	pack $row3.cancel -side right -fill x -expand 0
	
	pack $row1 $row2 $row3 -side top -fill both -expand 1 -padx 3 -pady 3

}

proc fileOpt::browse_file {} {
	global env
	variable filepath

	if { [info exists env(HOME)] } {
		set dir $env(HOME)
		set fileOpt::filepath [tk_getOpenFile -parent .fileloc -initialdir $dir \
			-title [ G_msg "Choose georeferenced file" ] -multiple false]
	} else {
		set fileOpt::filepath [tk_getOpenFile -parent .fileloc \
			-title [ G_msg "Choose georeferenced file" ] -multiple false]
	}
	
}


proc fileOpt::def_loc { } {
# define new location using georeferenced file readable by GDAL/OGR
	#vars declaration
	variable filepath
	variable fileLocation
	global database
	global env	

	if {$filepath==""} {return}

	if {$filepath==""} {
		tk_messageBox -type ok -icon error \
			-message [G_msg "WARNING: Please supply a\nvalid georeferenced file"] 
		return
	} 

        set fileLocation [ string trim $fileLocation ]

	if {[file exists $fileLocation ]== 1} {
		tk_messageBox -type ok -icon error \
			-message [G_msg "WARNING: The location '$fileLocation'\nalready exists, please try another name"] 
		set fileLocation ""
		return
	}

	if {[file exists $fileLocation ]==0} {  
		destroy .fileloc
		fileOpt::create_loc
		set refresh 1
		return
	}
}

proc fileOpt::create_loc { } {
# Create a new location using g.proj
# original bash code by M. Neteler

	variable filepath
	variable fileLocation
	global env database
	global location
	global mapset

	#test for valid WIND file
	if {[catch {exec g.region -p}] != 0} {

		# Create temporary location in order to run g.proj. For 1st time use

		set GRASSRC "grassrc6"
		set curr_gisrc $env(GISRC)

		set tempdir [pid]
		append tempdir ".tmp"
		file mkdir "$database/$tempdir/PERMANENT"
		
		# save existing .grassrc file
		if {[file exists "$env(HOME)/.$GRASSRC"] } {
			file copy "$env(HOME)/.$GRASSRC" "$database/$tempdir/$GRASSRC"
		}
		
		# create temporary .grassrc file to hold temporary location information
		set output [open "$env(HOME)/.$GRASSRC" w+]
			puts $output "LOCATION_NAME: $tempdir" 
			puts $output "MAPSET: PERMANENT"
			puts $output "DIGITIZER: none"
			puts $output "GISDBASE: $database" 
		close $output
		
		set env(GISRC) "$env(HOME)/.$GRASSRC"
				
		# Populate a temporary location with a minimal set of files
		set output [open "$database/$tempdir/PERMANENT/DEFAULT_WIND" w+]
			puts $output "proj:       3"
			puts $output "zone:       0"
			puts $output "north:      72N"
			puts $output "south:      27N"
			puts $output "east:       42E"
			puts $output "west:       11W"
			puts $output "cols:       6360"
			puts $output "rows:       5400"
			puts $output "e-w resol:  0:00:30"
			puts $output "n-s resol:  0:00:30"
		close $output
	
		set output [open "$database/$tempdir/PERMANENT/PROJ_INFO" w+]
			puts $output "name: Lat/Lon"
			puts $output "datum: wgs84"
			puts $output "towgs84: 0.000,0.000,0.000"
			puts $output "proj: ll"
			puts $output "ellps: wgs84"
		close $output
		
		set output [open "$database/$tempdir/PERMANENT/PROJ_UNITS" w+]
			puts $output "unit: degree"
			puts $output "units: degrees"
			puts $output "meters: 1.0"
		close $output
				
		# create new location from georeferenced file
		catch {exec g.proj -c georef=$filepath location=$fileLocation} errMsg
		if {[lindex $::errorCode 0] eq "CHILDSTATUS"} {
                        DialogGen .wrnDlg [G_msg "WARNING: Error creating new location"] warning \
				[format [G_msg "Error creating new location from georeferenced file. \
		                g.proj returned following message:\n\n%s"] $errMsg] \
				0 OK
		} else {
			set location $fileLocation
			set mapset "PERMANENT"
		}
	
		# restore previous .$GRASSRC
		if {[file exists "$database/$tempdir/$GRASSRC"]} {
			file copy -force "$database/$tempdir/$GRASSRC" "$env(HOME)/.$GRASSRC"
		}
		
		# cleanup
		catch {file delete -force "$database/$tempdir"}
		set env(GISRC) $curr_gisrc
	} else {
		# create new location from georeferenced file
		catch {exec g.proj -c georef=$filepath location=$fileLocation} errMsg
		if {[lindex $::errorCode 0] eq "CHILDSTATUS"} {
		        DialogGen .wrnDlg [G_msg "WARNING: Error creating new location"] warning \
				[format [G_msg "Error creating new location from georeferenced file. \
		                g.proj returned following message:\n\n%s"] $errMsg] \
				0 OK
		} else {
			set location $fileLocation
			set mapset "PERMANENT"
		}
	
	}

	return

}
