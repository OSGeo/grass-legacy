#=====================================================================================
#   
#            FILE:  epsg_option.tcl
#   
#     DESCRIPTION:  adds the utility to execute Netelers script to create a location 
#                   using the epsg codes
#  
#           NOTES:  ---
#          AUTHOR:  Antonello Andrea
#           EMAIL:  antonell ing.unitn.it
#         COMPANY:  Engineering, University of Trento / CUDAM
#       COPYRIGHT:  Copyright (C) 2004 University of Trento / CUDAM, ITALY, GPL
#         VERSION:  1.2
#         CREATED:  04/01/2004
#        REVISION:  22/04/2006 (Michael Barton, Arizona State University)
#       CHANGELOG:  20/12/2006 - EPSG code search and epsgOpt::create_loc. Michael Barton.
#                   08/12/2006 - Fixed directory choosing dialogs. Maris Nartiss.
#
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
#
#   part regarding to the creation of a new location using proj and 
#   the EPSG codes    (routines epsgLocCom and infoEpsg)
#
#############################################################################

namespace eval epsgOpt {
	variable browsedepsg #path to EPSG code file
	variable epsgLocation #name of new location to be created
	variable epsg_code  #EPSG code number
	variable searchterm #text string searched in EPSG file
	variable dir 
	variable start #starting index for searching EPSG text widget
	variable epsgtxt #text widget with EPSG definitions and codes
	variable dtnum #datum transformation number
	global env
	global database 
	global refresh
	global mingw #test to see if we are running a windows version in mingw


}
# G_msg.tcl should be sourced first for internationalized strings.

# the frame used to set EPSG parameters 
proc epsgOpt::epsgLocCom args {
	#Create main panel for setting location with EPSG code
	variable epsgLocation 
	variable epsg_code
	variable browsedepsg 
	variable searchterm
	variable dir
	variable start
	variable searchterm
	global database
	global env
	global mingw
		
	#initialize some variables
	set searchterm ""
	set dir "f"
	set start 1.0
	#Mac framework location for EPSG file
	#set env(/Library/Frameworks/PROJ.framework/Resources/proj) "/Library/Frameworks/PROJ.framework/Resources/proj"

	# NOTE: the epsg file is generated in GDAL for PROJ4 
	# with gdal/pymod/epsg_tr.py
	if { [ catch { set epsgOpt::browsedepsg "$env(GRASS_PROJSHARE)/epsg" } ] } {
		DialogGen .wrnDlg [G_msg "WARNING: cant get enviromental variable"] warning \
		[format [G_msg "Warning: Unable to get enviromental variable GRASS_PROJSHARE. \nThis is a GRASS installation error. \nSet enviromental variable GRASS_PROJSHARE to point to directory with Proj4 EPSG file. "]] \
		0 OK;
		return 0
	}
	set epsgOpt::epsgLocation "newLocation"
	set epsgOpt::epsg_code ""
        
	# creation of the parameter window
	set epsg_win [toplevel .optPopup]
	wm title $epsg_win [ G_msg "Define location using EPSG projection codes" ]
	# put it in the middle of the screen
	update idletasks
	set winWidth [winfo reqwidth $epsg_win]
	set winHeight [winfo reqheight $epsg_win]
	set scrnWidth [winfo screenwidth $epsg_win]
	set scrnHeight [winfo screenheight $epsg_win]
	set x [expr ($scrnWidth - $winWidth) / 2-250]
	set y [expr ($scrnHeight  - $winHeight) / 2]
	wm geometry $epsg_win +$x+$y
	wm deiconify $epsg_win
        
	#create the form and buttons
	
	set row1 [frame $epsg_win.row1]
	set row2 [frame $epsg_win.row2]
	set row3 [frame $epsg_win.row3]
	set row4 [frame $epsg_win.row4]
	
	LabelEntry $row1.newloc -label [G_msg "Name of new location"] \
		-labeljustify right -labelanchor e -labelwidth 30 -wraplength 200 \
		-textvariable epsgOpt::epsgLocation -width 35 \
		-helptext [G_msg "Enter name of location to be created"]
		
	pack $row1.newloc -side left -expand 0 -fill x -padx 2

	LabelEntry $row2.epsgpath -label [G_msg "Path to the EPSG-codes file"] \
		-labeljustify right -labelanchor e -labelwidth 30 -wraplength 200 \
		-textvariable epsgOpt::browsedepsg  -width 35 \
		-helptext [G_msg "Path to EPSG codes file"]
		
	#browse for epsg file
	Button $row2.browseepsgfile -justify center -width 10 -bd 1 -text [G_msg "Browse..."] \
		-helptext [G_msg "Browse to locate EPSG file"] \
		-command "set epsgOpt::browsedepsg \[tk_getOpenFile -initialdir epsgOpt::browsedepsg -initialfile epsg \
		-parent .optPopup -title \[ G_msg \"Choose EPSG file\" \] -multiple false\]" 
		
	pack $row2.epsgpath $row2.browseepsgfile -side left -expand 0 -fill x -padx 2

	#browse epsg codes in file
	LabelEntry $row3.code_entry -label [G_msg "EPSG code number of projection"] \
		-labeljustify right -labelanchor e -labelwidth 30 -wraplength 200 \
		-textvariable epsgOpt::epsg_code  -width 35 \
		-helptext [G_msg "Enter EPSG code for selected projection"]
        
	Button $row3.codebutton -justify center -width 10 -bd 1 -text [G_msg "Browse..."] \
		-helptext [G_msg "View EPSG codes and projection information."] \
		-command {
			if {[file exists $epsgOpt::browsedepsg]} {
				set epsgOpt::epsg_code ""
				epsgOpt::codesEpsg
			} else {
				DialogGen .wrnDlg [G_msg "WARNING: epsg-codes file not found"] warning \
				[G_msg "WARNING: The epsg-codes file was not found!"] \
				0 OK
				return 0
			}
		}
                                     
	pack $row3.code_entry $row3.codebutton -side left -fill x -expand 0 -padx 2
	
	Button $row4.submit -justify center -width 15 -text [G_msg "Define location"] \
		-command "epsgOpt::def_loc" -bd 1
				
	Button $row4.cancel -justify center -width 15 -text [G_msg "Cancel"] \
		-command {destroy .optPopup} -bd 1
		
	pack $row4.submit -side left -fill x -expand 0
	pack $row4.cancel -side right -fill x -expand 0
	
	pack $row1 $row2 $row3 $row4 -side top -fill both -expand 1 -padx 3 -pady 3
	return 1
}

proc epsgOpt::def_loc { } {
# define new location using EPSG code
	global refresh
	variable epsg_code
	variable epsgLocation

	if {$epsg_code==""} {return}

	if {![string is integer $epsg_code]} {
		tk_messageBox -type ok -icon error \
			-message [G_msg "WARNING: Please supply a\nvalid EPSG code (integer)"] 
		return
	} 

	set epsgLocation [ string trim $epsgLocation ]

	if {[file exists $epsgLocation ]== 1} {
		tk_messageBox -type ok -icon error \
			-message [G_msg "WARNING: The location '$epsgLocation'\nalready exists, please try another name"] 
		set epsgLocation ""
		return
	}

	if {[file exists $epsgLocation ]==0} {  
		destroy .optPopup
		epsgOpt::create_loc
		set refresh 1
		return 1
	}
}

proc epsgOpt::create_loc { } {
# Create a new location using g.proj
# original bash code by M. Neteler

	variable epsg_code
	variable epsgLocation
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
		
		
		# create new location from EPSG code
		epsgOpt::runproj	
		# restore previous .$GRASSRC
		if {[file exists "$database/$tempdir/$GRASSRC"]} {
			file copy -force "$database/$tempdir/$GRASSRC" "$env(HOME)/.$GRASSRC"
		}
		
		# cleanup
		catch {file delete -force "$database/$tempdir"}
		set env(GISRC) $curr_gisrc
	} else {
		# create new location from EPSG code
		epsgOpt::runproj	
	}

	return

}

proc epsgOpt::runproj {} {
	# first run g.proj to see if there are more than the default
	# parameters to choose from
	variable epsgLocation 
	variable epsg_code
	
	set dtrans ""
	
	catch {set dtrans [exec g.proj epsg=$epsg_code datumtrans=-1 2> /dev/null]} errMsg
	
	if {$dtrans==""} {
		 # if nothing returned, use default. 
		 catch {exec g.proj -c epsg=$epsg_code location=$epsgLocation datumtrans=1} errMsg
	 } else {
		 # user selects datum transform
		 #create dialog that lists datum transforms, asks user to enter a number and press OK
		 set dtnum [epsgOpt::sel_dtrans $dtrans]

		# operation canceled
		if {$dtnum == -9} {return}
		
		# create new location from epsg code
		catch {exec g.proj -c epsg=$epsg_code location=$epsgLocation datumtrans=$dtnum} errMsg
	 }
	 	 
	 #catch any other errors
	if {[lindex $::errorCode 0] eq "CHILDSTATUS"} {
		DialogGen .wrnDlg [G_msg "WARNING: Error creating new location"] warning \
		[format [G_msg "Error creating new location from EPSG code. \
		g.proj returned following message:\n\n%s"] $errMsg] \
		0 OK
	} else {
			set location $epsgLocation
			set mapset "PERMANENT"
	}

}

proc epsgOpt::sel_dtrans {dtrans} {

# Dialog for selecting optional datum transform parameters
# Argument is stdout from g.proj
	variable dtnum
	
	# default datum transformation
	set epsgOpt::dtnum 1 

    # Create a popup search dialog
    toplevel .dtrans_sel
    wm title .dtrans_sel [G_msg "Select datum transform"]
    set row1 [frame .dtrans_sel.frame1] 
    set row2 [frame .dtrans_sel.frame2] 
    set row3 [frame .dtrans_sel.frame3] 
    #set row4 [frame .dtrans_sel.frame4]
    
    set dt_text [text $row1.dttxt \
    	-wrap word -relief flat  \
    	-yscrollcommand "$row1.vscroll set"]
    $dt_text insert end $dtrans 
	scrollbar $row1.vscroll \
				-relief sunken \
				-command "$dt_text yview"
    pack $row1.dttxt -side left -fill both -expand 0 
    pack $row1.vscroll -side right -fill y -expand 0
    pack $row1 -side top -pady 3 -expand 1 -fill both
			      
    Label $row2.label -text [G_msg "Datum transform number: "] 
    set dt_entry [Entry $row2.enter -relief sunken \
    	-textvariable epsgOpt::dtnum -width 5 \
    	-helptext [G_msg "Enter number of datum transform selected"]]
    pack $row2.label $row2.enter -side left -fill x -expand 0 -anchor w
    pack $row2 -side top -padx 3 -pady 4 -expand 0 -fill x
        
    Button $row3.ok -text [G_msg "OK"] -width 8 -bd 1 \
    	-command "destroy .dtrans_sel"
    pack $row3.ok -side left -fill x -expand 0
    button $row3.cancel -text [G_msg "Cancel"] -width 8 -bd 1 \
    	-command "set epsgOpt::dtnum -9; destroy .dtrans_sel"
    pack $row3.cancel -side right -fill x -expand 0
    pack $row3 -side bottom -pady 3 -expand 0 -fill x
    
    tkwait window .dtrans_sel
    return $epsgOpt::dtnum

}


proc epsgOpt::codesEpsg args {
# text widget for listing EPSG codes

	variable browsedepsg
	variable epsgtxt
	variable epsg_code

	toplevel .infoPopup
	wm title .infoPopup {EPSG-codes}
	update idletasks
	wm geometry .infoPopup +250+10
	wm deiconify .infoPopup
	
	set epsgfr [frame .infoPopup.fr]
	
	set titlefr [frame $epsgfr.top -relief groove -bd 2 -bg white]
	label $titlefr.title1 -text [G_msg "EPSG CODES (from file: $epsgOpt::browsedepsg)"] \
		-fg mediumblue -bg white
	label $titlefr.title2 -bg white \
		-text [G_msg "You can select EPSG code (in <braces>) and copy it for later use." ]
	pack $titlefr.title1 $titlefr.title2 -side top
	pack $titlefr -side top -fill x -expand 0
	
	frame $epsgfr.mid
	set epsgtxt [text $epsgfr.mid.text \
			   -wrap word -exportselection 1 \
			   -relief flat -selectbackground lightgreen \
			   -yscrollcommand "$epsgfr.mid.vscroll set"]

	scrollbar $epsgfr.mid.vscroll \
				-relief sunken \
				-command "$epsgtxt yview"
			  
	# tag configuration
	$epsgtxt tag configure underline -underline 1
	$epsgtxt tag configure title -relief sunken -borderwidth 2 \
		-background white -foreground "mediumblue" -justify center
	#$epsgtxt tag configure lefttitle -relief flat -background beige
	# Do not set background color for subtitle. It will override selection background color!
	$epsgtxt tag configure subtitle -relief flat
			
	# open the file 
	set f [open $epsgOpt::browsedepsg "r"]
	set found ""
	while { [eof $f] == 0 } {
		set line [gets $f]
		set firstdash [string first # $line]
		set firstminor [string first < $line]
		if {$firstdash == "0"} {
			$epsgtxt insert end "\n$line\n" lefttitle
			set found "yes"
		}
		if {$firstminor == "0"} {
			$epsgtxt insert end "\n$line\n\n" subtitle
		}
		if {$firstminor != "0" && $firstdash != "0" && $found != "yes"} {
			$epsgtxt insert end [format "\n\n%s\n\n" [G_msg "GUESS THAT IS NOT THE EPSG FILE"]] title;
			break;
		}
	}
			
	set controls [frame .infoPopup.buttons]
	button $controls.search -text [G_msg "Search"] -command "epsgOpt::search_epsg $epsgtxt" \
		-bd 1
	
	Button $controls.grab -text [G_msg "Grab code"] -width 8 -bd 1 \
		-command "epsgOpt::grabcode"
	
	pack $controls.search $controls.grab -side left -fill x -expand 0

	button $controls.close -width 6 -text [G_msg "Close"] \
		-command {destroy .infoPopup} -bd 1
	pack $controls.close -side right -fill x -expand 0                  

	pack $controls -side bottom -fill x -expand 0 -padx 5 -pady 4      
	pack $epsgtxt -side left -fill both -expand 1
	pack $epsgfr.mid.vscroll -side right -fill both -expand 0
	pack $epsgfr.mid -side top -fill both -expand 1
        pack $epsgfr -fill both -expand 1
         
}

proc epsgOpt::search_epsg { epsgtxt } {
# Widget for searching EPSG file. Selects EPSG code associated with found search term.
# Argument is text widget

	variable searchterm
	variable dir
	variable start

    # Create a popup search dialog
    toplevel .search_epsg
    wm title .search_epsg [G_msg "Search"]
    set row1 [frame .search_epsg.frame1]
    set row2 [frame .search_epsg.frame2]
    set row3 [frame .search_epsg.frame3]
    set row4 [frame .search_epsg.frame4]
    
    Label $row1.label -text [G_msg "Search text: "] \
    	-helptext [G_msg "Search for entered text in EPSG file"]
    set searchentry [entry $row1.enter -relief sunken -textvariable epsgOpt::searchterm]
    pack $row1.label $row1.enter -side left -fill x -expand 0 -anchor w
    pack $row1 -side top -padx 3 -pady 4 -expand 1 -fill both
    
    radiobutton $row2.forward -text [G_msg "forward search"] -variable epsgOpt::dir -value "f"
    radiobutton $row2.backward -text [G_msg "backward search"] -variable epsgOpt::dir -value "b"
    $row2.forward select
    pack $row2.forward $row2.backward -side left \
    	-anchor w -fill x -expand 0
    pack $row2 -side top -padx 3 -expand 1 -fill both
    
    Button $row4.search -text [G_msg "Search"] -width 8 -bd 1 \
    	-command "epsgOpt::textsearch"
    pack $row4.search -side left -fill x -expand 0
    button $row4.cancel -text [G_msg "Close"] -width 8 -bd 1 -command "destroy .search_epsg"
    pack $row4.cancel -side right -fill x -expand 0
    pack $row4 -side top -pady 3 -expand 1 -fill both

}

proc epsgOpt::textsearch { } {
# Search for text in EPSG text widget and return the EPSG code
	variable epsgtxt
	variable searchterm
	variable dir
	variable start
	variable epsg_code
	set strlength 0

	catch {$epsgtxt tag remove sel [lindex [$epsgtxt tag ranges sel] 0] [lindex [$epsgtxt tag ranges sel] 1]}

	if {$dir == "f"} {
		catch {set start [$epsgtxt search -forwards -nocase -count strlength -- $epsgOpt::searchterm $start]}
		catch {set newstart [$epsgtxt index "$start +[expr 1+$strlength] c"]}
	} else {
		catch {set start [$epsgtxt search -backwards -nocase -count strlength -- $epsgOpt::searchterm $start]}
		catch {set newstart [$epsgtxt index "$start -1 c"]}
	}
	
	update idletasks
	
	if {$start != 0 && $strlength !=0} {
		$epsgtxt tag add sel "$start"
		$epsgtxt see $start
		catch {set start $newstart}
		set currpos [$epsgtxt index sel.first]
		if { [$epsgtxt get "$currpos linestart"] == "<" } {
			set codestart [$epsgtxt index "$currpos linestart +1c"]
			set codeend [$epsgtxt index [$epsgtxt search -forwards -- {>} $codestart]]
		} else {
			set codestart [$epsgtxt index "[$epsgtxt search -forwards -- {<} $currpos] +1c"]
			set codeend [$epsgtxt index [$epsgtxt search -forwards -- {>} $codestart]]
		}
		
		$epsgtxt tag remove sel [$epsgtxt index sel.first]
		$epsgtxt tag add sel "$codestart" "$codeend"
		set epsg_code [$epsgtxt get $codestart $codeend]
	} else {
		set start 1.0
		return
	}

}

proc epsgOpt::grabcode { } {
# put the code in the EPSG code entry and activate the define location button
# Will grab the code of the entry where the insertion cursor is located if nothing found search

	variable epsgtxt
	variable searchterm
	variable dir
	variable start
	variable epsg_code
	
	if { $epsg_code == "" } {
		set currpos [$epsgtxt index insert]
		if { [$epsgtxt get "$currpos linestart"] == "<" } {
			set codestart [$epsgtxt index "$currpos linestart +1c"]
			set codeend [$epsgtxt index [$epsgtxt search -forwards -- {>} $codestart]]
		} else {
			set codestart [$epsgtxt index "[$epsgtxt search -forwards -- {<} $currpos] +1c"]
			set codeend [$epsgtxt index [$epsgtxt search -forwards -- {>} $codestart]]
		}
		
		$epsgtxt tag remove sel [$epsgtxt index insert]
		$epsgtxt tag add sel "$codestart" "$codeend"
		set epsg_code [$epsgtxt get $codestart $codeend]
	}

	destroy .infoPopup
}

