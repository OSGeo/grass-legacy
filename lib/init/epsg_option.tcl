#=====================================================================================
#   
#            FILE:  epsg_option.tcl
#   
#     DESCRIPTION:  adds the utility to execute Netelers script to create a location 
#                   using the epsg codes
#  
#           NOTES:  ---
#          AUTHOR:  Antonello Andrea
#           EMAIL:  antonell@ing.unitn.it
#         COMPANY:  Engineering, University of Trento / CUDAM
#       COPYRIGHT:  Copyright (C) 2004 University of Trento / CUDAM, ITALY, GPL
#         VERSION:  1.0
#         CREATED:  04/01/2004
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
#
#   part regarding to the creation of a new location using proj and 
#   the EPSG codes    (routines epsgLocCom and infoEpsg)
#
#############################################################################

global browsedepsg 


# the frame used to set EPSG parameters 
proc epsgLocCom args {
        #vars declaration
        global database
        global epsgLocation 
        global epsg_code
        global env
        global thelocation
        global browsedepsg 
        
        set browsedepsg "/usr/local/share/proj/epsg"
        set epsgLocation "newLocation"
        set epsg_code ""
        
        # creation of the parameter window
        toplevel .optPopup
        wm title .optPopup {Epsg Settings}
        # put it in the middle of the screen
        update idletasks
        set winWidth [winfo reqwidth .optPopup]
        set winHeight [winfo reqheight .optPopup]
        set scrnWidth [winfo screenwidth .optPopup]
        set scrnHeight [winfo screenheight .optPopup]
        set x [expr ($scrnWidth - $winWidth) / 2-250]
        set y [expr ($scrnHeight  - $winHeight) / 2]
        wm geometry .optPopup +$x+$y
        wm deiconify .optPopup
        
        #create the form and buttons
        label .optPopup.input1_ppb -text "Name of new location" -justify center 
        entry .optPopup.input1_ppbEntry -textvariable epsgLocation -width 35
        label .optPopup.input2_ppb -text "Database" -justify center 
        entry .optPopup.input2_ppbEntry -textvariable database  -width 35
        label .optPopup.input3_ppb -text "Path to the EPSG-codes file" -justify center 
        entry .optPopup.input3_ppbEntry -textvariable browsedepsg  -width 35
        label .optPopup.input4_ppb -text "EPSG code number of projection" -justify center 
        entry .optPopup.input4_ppbEntry -textvariable epsg_code  -width 35
        
        button .optPopup.browseepsgfile -justify center -width 9 -text "browse" \
                                        -command "set browsedepsg \[tk_getOpenFile\]" 
                                        
        bind .optPopup.input3_ppbEntry <Leave> {
             if {$browsedepsg == ""} {
                set browsedepsg "/usr/local/share/proj/epsg"
            }
        }

        button .optPopup.codes 	-justify center -width 9 -text "EPSG-codes" \
                        -command {
                                  if {[file exists $browsedepsg]== 0} {
                                    DialogGen .wrnDlg "WARNING: epsg-codes file not found" warning "WARNING: \
                                    The epsg-codes file was not found!" \
                                    0 OK;
                                  }
                                  if {[file exists $browsedepsg]== 1} {
                                    codesEpsg;
                                  }
                                 }
                                
                                     
        pack .optPopup.codes -side left -fill both -expand 1
        
        button .optPopup.param 	-justify center -width 9 -text "?" \
                        -command {infoEpsg}
        pack .optPopup.param -side left -fill both -expand 1
        
        button .optPopup.submit 	-justify center -width 6 -text "ok" -state disabled\
                        -command {
                                  set thelocation "$database/$epsgLocation";
                                  if {[file exists $thelocation ]== 1} {
                                    DialogGen .wrnDlg "WARNING: location exists" warning "WARNING: \
                                    The location '$thelocation' already exists, please try another name" \
                                    0 OK;
                                  }
                                  if {[file exists $browsedepsg]== 0} {
                                    DialogGen .wrnDlg "WARNING: epsg-codes file not found" warning "WARNING: \
                                    The epsg-codes file was not found!" \
                                    0 OK;
                                  }
                                  if {[file exists $browsedepsg]== 1} {
                                      if {[file exists $thelocation ]==0} {  
                                        destroy .optPopup; 
                                        exec -- xterm -e $env(GISBASE)/etc/make_location_epsg_g57.sh \
                                            $epsg_code $epsgLocation $database >@stdout 2>@stderr; 
                                        DialogGen .wrnDlg "WARNING: restart Grass please" warning "WARNING: \
                                            Please restart Grass in order find the created location in the list (closing it for you now)" \
                                            0 OK; 
                                        set env(EPSGSCRIPT) {yes};
                                        puts stdout "exit";
                                        destroy . 
                                      }
                                  }
                                  set thelocation ""
                                 }

        bind .optPopup.input4_ppbEntry <KeyRelease> {
            .optPopup.submit configure -state active
        }

        pack .optPopup.submit -side left -fill both -expand 1
        
        button .optPopup.cancel -justify center -width 6 -text "cancel" \
                        -command {destroy .optPopup}
        pack .optPopup.submit -side left -fill both -expand 1

        # geometry
        grid .optPopup.input1_ppb 	-row 0 -column 0
        grid .optPopup.input1_ppbEntry 	-row 0 -column 1 -columnspan 2
        grid .optPopup.input2_ppb 	-row 1 -column 0
        grid .optPopup.input2_ppbEntry 	-row 1 -column 1 -columnspan 2
        grid .optPopup.input3_ppb 	-row 2 -column 0
        grid .optPopup.input3_ppbEntry 	-row 2 -column 1 -columnspan 2
        grid .optPopup.browseepsgfile   -row 2 -column 3 
        grid .optPopup.input4_ppb 	-row 3 -column 0
        grid .optPopup.input4_ppbEntry 	-row 3 -column 1 -columnspan 2
        grid .optPopup.codes 		-row 3 -column 3 
        grid .optPopup.submit 		-row 4 -column 0
        grid .optPopup.cancel 		-row 4 -column 1
        grid .optPopup.param 		-row 4 -column 3 
}

# listing of the EPSG codes
proc codesEpsg args {
    
        global browsedepsg
    
        toplevel .infoPopup
        wm title .infoPopup {EPSG-codes}
        update idletasks
        set winWidth [winfo reqwidth .infoPopup]
        set winHeight [winfo reqheight .infoPopup]
        set scrnWidth [winfo screenwidth .infoPopup]
        set scrnHeight [winfo screenheight .infoPopup]
        set x [expr ($scrnWidth - $winWidth) / 2-230]
        set y [expr ($scrnHeight  - $winHeight) / 2 - 110]
        wm geometry .infoPopup +$x+$y
        wm deiconify .infoPopup
        
        text .infoPopup.text -width 100 -height 50\
                   -wrap word \
                   -relief raised \
                   -yscrollcommand ".infoPopup.vscroll set"
                   
        # tag configuration
        .infoPopup.text tag configure underline -underline 1
        .infoPopup.text tag configure title -relief raised -borderwidth 2 -background grey -justify center
        .infoPopup.text tag configure lefttitle -relief raised -borderwidth 2 -background grey
        .infoPopup.text tag configure subtitle -relief raised -borderwidth 2 
    
        
        # the text to be inserted
        .infoPopup.text insert end " \nEPSG - code file $browsedepsg\n\n" title
        .infoPopup.text insert end " \n\n" subtitle
        
        # open the file 
        set f [open $browsedepsg "r"]
        set found ""
        while { [eof $f] == 0 } {
            set line [gets $f]
            set firstdash [string first # $line]
            set firstminor [string first < $line]
            if {$firstdash == "0"} {
                .infoPopup.text insert end "$line\n" lefttitle
                set found "yes"
            }
            if {$firstminor == "0"} {
                .infoPopup.text insert end "$line\n\n" subtitle
            }
            if {$firstminor != "0" && $firstdash != "0" && $found != "yes"} {
                .infoPopup.text insert end "\n\nGUESS THAT IS NOT THE EPSG FILE\n\n" title;
                break;
            }
        }
        
        pack .infoPopup.text -side left -fill y 
        
        scrollbar .infoPopup.vscroll \
                    -relief raised \
                    -command ".infoPopup.text yview"
        pack .infoPopup.vscroll -side right -fill y
        
        button .infoPopup.ex -justify center -width 5 -text "ok" \
                             -command {destroy .infoPopup}			
        pack .infoPopup.ex -side bottom -expand 1 -fill both
}

# help for the EPSG Location creation
proc infoEpsg args {

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
        .infoPopup.text tag configure subtitle -relief raised -borderwidth 2 
    
        # the text to be inserted
        .infoPopup.text insert end " \nCREATION OF A NEW grass LOCATION WITH EPSG\n " title
        .infoPopup.text insert end "- needed parameters -\n\n" title
        .infoPopup.text insert end "\n" 
        .infoPopup.text insert end "\n   Name of new location:\n\n" subtitle
        .infoPopup.text insert end "\nRequires as input the name of the new location to be created\n\n"
        .infoPopup.text insert end "\n" 
        .infoPopup.text insert end "\n   Database:\n\n" subtitle
        .infoPopup.text insert end "\nThe Grass database in which the location should be created\n\n"
        .infoPopup.text insert end "\n" 
        .infoPopup.text insert end "\n   EPSG code number of projection:\n\n" subtitle
        .infoPopup.text insert end "\nEPSG code number of projection (see /usr/local/share/proj/epsg or push the 'EPSG-codes' button)\n\n"
        
        pack .infoPopup.text -side left -fill both 
        
        scrollbar .infoPopup.vscroll \
                    -relief raised \
                    -command ".infoPopup.text yview"
        pack .infoPopup.vscroll -side right -fill y
        
        button .infoPopup.ex -justify center -width 5 -text "ok" \
                             -command {destroy .infoPopup}			
        pack .infoPopup.ex -side bottom -expand 1 -fill both
}


