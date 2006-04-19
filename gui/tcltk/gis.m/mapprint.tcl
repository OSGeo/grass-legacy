###############################################################
# mapprint.tcl - GRASS GIS Manager procedures for postscript and
# lpr printing, and pdf and eps output
# January 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################


namespace eval psprint {
	global array can # mon
	global pgwd
	global pght
	global paper
	global paper_preset
	global printmode
	global printer
	global gsexists
	global res
	global format
	global orient
	global epsfile
	global pdffile
	global tmpscript 
	global tmppsfile 
	global tmpppmfile
}


#initialize variables
proc psprint::init { } {
	global pgwd
	global pght
	global docwd
	global docht
	global paper
	global paper_preset
	global printmode
	global printer
	global gsexists
	global orient
	global res
	global mon
	global mleft
	global mright
	global mtop
	global mbottom
	global gsstate
	global ldevice
	global gsdevices

    set pgwd 8.5
    set pght 11
    set docwd 7.5
    set docht 10
    set paper "preset"
    set paper_preset "letter"
    set printer ""
    set gsexists 1
    set orient "landscape"
    set res 300
	set mleft 1
	set mright 1
	set mtop 1
	set mbottom 1
	
	# check for ghostscript
	if ![catch {set input [eval exec "gs -help"]}] {
		regexp ".*Available devices:(.*)Search path:" $input string gsdevices 
		set gsstate "normal"
		set printmode "lpr"
		regsub -all {   } $gsdevices { } gsdevices
		regsub -all { } $gsdevices \n gsdevices
		regsub -all \n\n $gsdevices \n gsdevices
	} else {
		set gsdevices ""
		set gsstate "disabled"
		set printmode "eps"
	}
}	

# calculate paper size and document size on paper (all in inches)  
proc psprint::paper { } {
	global paper
	global paper_preset
	global printmode
	global pgwd
	global pght
	global mleft
	global mright
	global mtop
	global mbottom
	global docwd
	global docht
	global orient
    
    # set paper dimensions
	if { $paper == "preset" } {
		switch $paper_preset {
			"11x17" {
				set pgwd 11
				set pght 17
			}
			"ledger" {
				set pgwd 17
				set pght 11
			}
			"legal" {
				set pgwd 8.5
				set pght 14
			}
			"letter" {
				set pgwd 8.5
				set pght 11
			}
			"a0" {
				set pgwd 33.0556
				set pght 46.7778
			}
			"a1" {
				set pgwd 23.3889
				set pght 33.055
			}
			"a2" {
				set pgwd 16.5278
				set pght 23.3889
			}
			"a3" {
				set pgwd 11.6944
				set pght 16.5278 
			}
			"a4" {
				set pgwd 8.26389 
				set pght 11.6944
			}
		}
	}

	set docwd [expr $pgwd - $mright - $mleft]
	set docht [expr $pght - $mtop - $mbottom]
	
	if { $orient == "landscape" && $printmode == "pdf" } {
		set docwd [expr $docwd + 2]
		set docht [expr $docht + 2]
	} else {
		set docwd [expr $docwd + 1]
		set docht [expr $docht + 1]
	}
		
	update
}

# initialize tmpfiles for poscript printing
proc psprint::init_tmpfiles { } {
	global tmpscript
	global tmppsfile
	global tmppngfile

    # get temporary file for postscript printing
    set pid [ pid ]
    set tmppsfile [ exec g.tempfile pid=$pid ]
    append tmppsfile ".ps"
    set pid [ pid ]
    set tmppngfile [ exec g.tempfile pid=$pid ]
    append tmppngfile ".png"
}

# show gs printer devices in output window
proc psprint::show_devices { } {
	global gsdevices

	set ah [monitor_annotation_start {} "Ghostscript Output Devices" {}]
	monitor_annotate $ah $gsdevices
}


# create printer options window
proc psprint::window { cm cv cx cy } {
	global pgwd
	global pght
	global paper
	global paper_preset
	global printmode
	global printer
	global gsexists
	global epsfile
	global pdffile
	global orient
	global res
	global pgwd
	global pght
	global mon
	global mleft
	global mright
	global mtop
	global mbottom
	global gspresent
	global ldevice
	global gsdevices
	global gsstate
	
    set mon $cm
	
    # check if opened
    if { [winfo exists .printwin] } {
        wm deiconify .printwin
        raise .printwin
        return
    }
    
    set PW [toplevel .printwin]
    wm title $PW [G_msg "Postscript and LPR printing of map display"]

    # Left part paper + output
    set PWid(left) [ frame $PW.left -padx 5 -pady 5]  
    pack $PWid(left) -side left -anchor w

    # paper size, scale
    set PWid(paper) [ frame $PWid(left).paper]  
    pack $PWid(paper) -side top -anchor w

	# preset paper sizes (from ghostscript)
    set row [ frame $PWid(paper).row1 ]
    radiobutton $row.a -variable paper -value "preset" \
		-highlightthickness 0 
    Label $row.b -anchor w -text [G_msg "Preset paper type"]
    ComboBox $row.c -label "" -width 20  -textvariable paper_preset \
		-values {"letter" "a4" "legal" "11x17" "a3" "ledger" "a0" "a1" "a2" } \
		-modifycmd psprint::paper
    pack $row.a $row.b $row.c -side left;
    pack $row -side top -fill x -expand no -anchor n

	# custom paper sizes
    set row [ frame $PWid(paper).row2 ]
    radiobutton $row.a -variable paper -value "custom" \
		-highlightthickness 0
    Label $row.b -anchor w -text [G_msg "Custom paper size"]
    Label $row.c -anchor w -text [G_msg "width:"]
    Entry $row.d -width 10 -textvariable pgwd
    Label $row.e -anchor w -text [G_msg "  height:"]
    Entry $row.f -width 10 -textvariable pght 
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left;
    pack $row -side top -fill x -expand no -anchor n
    
	#margins
    set row [ frame $PWid(paper).row3]
    Label $row.a -anchor w -text [G_msg "Margins  left:"]
    Entry $row.b -width 10 -textvariable mleft 
    Label $row.c -anchor w -text [G_msg " right:"] 
    Entry $row.d -width 10 -textvariable mright 
    Label $row.e -anchor w -text [G_msg " top:"]
    Entry $row.f -width 10 -textvariable mtop 
    Label $row.g -anchor w -text [G_msg " bottom:"]
    Entry $row.h -width 10 -textvariable mbottom 

    pack $row.a $row.b $row.c $row.d $row.e $row.f $row.g $row.h -side left;
	pack $row -side top -fill x -expand no -anchor n

    # portrait or landscape
    set row [ frame $PWid(paper).row4 ]
	LabelEntry $row.a -label [G_msg "Resolution (dpi) for printing and PDF "] \
		-textvariable res -width 4
    Label $row.b -anchor w -text "  "
    radiobutton $row.c -variable orient -value "landscape" \
		-text "landscape mode" -highlightthickness 0
    radiobutton $row.d -variable orient -value "portrait" \
		-text "portrait mode  " -highlightthickness 0
    pack $row.a $row.b $row.c $row.d -side left;
    pack $row -side top -fill x -expand no -anchor n

    # output options
    set PWid(output) [ frame $PWid(left).output ]  
    pack $PWid(output) -side top -anchor w

    # LPR printer
    set row [ frame $PWid(output).lpr ]
    radiobutton $row.a -variable printmode -value "lpr" \
    	-state $gsstate -highlightthickness 0
    Label $row.b -anchor w -text [G_msg "Send to LPR printer*"] \
    	-state $gsstate
    pack $row.a $row.b -side left;
    pack $row -side top -fill x -expand no -anchor n

    # Postscript printer
    set row [ frame $PWid(output).psprinter ]
    radiobutton $row.a -variable printmode -value "psprint" \
    	-state $gsstate -highlightthickness 0
    Label $row.b -anchor w -text [G_msg "Send to postscript device* "] \
    	-state $gsstate
    Entry $row.c -width 30 -textvariable printer  \
    	-state $gsstate
    Button $row.d -text [G_msg "list devices"] \
    	-command "psprint::show_devices" \
		-helptext [G_msg "list ghostscript output devices"] \
    	-state $gsstate
    pack $row.a $row.b $row.c $row.d -side left;
    pack $row -side top -fill x -expand no -anchor n

    # PDF file
    set row [ frame $PWid(output).pdffile]
    radiobutton $row.a -variable printmode -value "pdf" \
    	-state $gsstate -highlightthickness 0 
    Label $row.b -anchor w -text [G_msg "Save to PDF file*              "]  \
    	-state $gsstate 
    Entry $row.c -width 30 -textvariable pdffile  -state $gsstate
    Button $row.d -text [G_msg "Browse"]  -command { set pdffile \
		[tk_getSaveFile -title "Output PDF file" -defaultextension ".pdf"]} \
    	-state $gsstate
    pack $row.a $row.b $row.c $row.d -side left;
    pack $row -side top -fill x -expand no -anchor n

    # EPS file
    set row [ frame $PWid(output).epsfile ]
    radiobutton $row.a -variable printmode -value "eps" \
     	-highlightthickness 0 
    Label $row.b -anchor w -text [G_msg "Save to EPS file               "] 
    Entry $row.c -width 30 -textvariable epsfile 
    Button $row.d -text [G_msg "Browse"] -command { set epsfile \
           [ tk_getSaveFile -title "Output EPS file" -defaultextension ".eps"] }
    pack $row.a $row.b $row.c $row.d -side left;
    pack $row -side top -fill x -expand no -anchor n

	set row [ frame $PWid(output).gsmessage ]
    Label $row.a -anchor w -text [G_msg "*requires ghostscript to be installed and in path"]
    pack $row.a -side bottom;
    pack $row -side top -fill x -expand yes -anchor center

#    Buttons 
    set but [ frame $PWid(left).buttons ]  
    pack $but -side top

    Button $but.print -text [G_msg "Print"] -command "update; psprint::print $cv"
    Button $but.close -text [G_msg "Close"] -command { destroy .printwin }
    pack $but.print $but.close -side left 

}

proc psprint::print { cv } {
	global paper
	global paper_preset
	global printmode
	global printer
	global gsexists
	global res
	global format
	global orient
	global epsfile
	global pdffile
	global tmppsfile 
	global tmppngfile
	global gmpath
	global pgwd
	global pght
	global mon
	global docwd
	global docht
    
    psprint::init_tmpfiles
   	psprint::paper
   	update
    set landscape $gmpath/landscap.ps
    
    #change doc size to points
    set cdocwd [expr $docwd * 72]
    set cdocht [expr $docht * 72]
    
 	# set paper size for postscript printer and pdf files
	set w [expr round($pgwd * $res)]
	set h [expr round($pght * $res)]
	set format "-g$w"
	append format "x$h"

	# lpr printing		
    if { $printmode == "lpr" } {
		set printmap [open "$tmppsfile" w]
		if { $orient == "portrait" } {
			if { [expr $docht / $docwd] < [expr $pght / $pgwd] } {
				$cv postscript -pageheight $cdocht -channel $printmap
			} else {
				$cv postscript -pagewidth $cdocwd -channel $printmap
			}
		} else {
			if { [expr $docht / $docwd] < [expr $pght / $pgwd] } {
				$cv postscript -rotate 1 -pageheight $cdocht -channel $printmap
			} else {
				$cv postscript -rotate 1 -pagewidth $cdocwd -channel $printmap
			}
		}		
		after 500
		close $printmap
		eval exec "cat $tmppsfile | gs  $format -sDEVICE=png16m -r$res -sNOPAUSE -sOutputFile=$tmppngfile -dBATCH - " 
		eval exec "lpr $tmppngfile" 
    }

	# postsript printing via ghostsript
    if { $printmode == "psprint" && $printer != "" } {
		set printmap [open "$tmppsfile" w]
		if { $orient == "portrait" } {
			if { [expr $docht / $docwd] < [expr $pght / $pgwd] } {
				$cv postscript -pageheight $cdocht -channel $printmap
			} else {
				$cv postscript -pagewidth $cdocwd -channel $printmap
			}
		} else {
			if { [expr $docht / $docwd] < [expr $pght / $pgwd] } {
				$cv postscript -rotate 1 -pageheight $cdocht -channel $printmap
			} else {
				$cv postscript -rotate 1 -pagewidth $cdocwd -channel $printmap
			}
		}		
		after 500
		close $printmap
		eval exec "cat $tmppsfile | gs  $format -sDEVICE=$printer -r$res -sNOPAUSE -dBATCH - " 
	}

	# output to pdf file via ghostscript	
	if { $printmode == "pdf" && $pdffile != "" } {
		set printmap [open "$tmppsfile" w]
		if { $orient == "portrait" } {
			if { [expr $docht / $docwd] < [expr $pght / $pgwd] } {
				$cv postscript -pageheight $cdocht -channel $printmap
			} else {
				$cv postscript -pagewidth $cdocwd -channel $printmap
			}
		} else {
			if { [expr $docht / $docwd] < [expr $pght / $pgwd] } {
				$cv postscript -rotate 1 -pageheight $cdocht -channel $printmap
			} else {
				$cv postscript -rotate 1 -pagewidth $cdocwd -channel $printmap
			}
		}		
		after 500
		close $printmap
		eval exec "cat $tmppsfile | gs  $format -sDEVICE=pdfwrite -r$res -sNOPAUSE -sOutputFile=$pdffile -dBATCH - " 
	}

	# output to eps file
	if { $printmode == "eps" && $epsfile != "" } {
		if { $orient == "portrait" } {
			$cv postscript -file "$epsfile"
		} else {
			$cv postscript -file "$epsfile" -rotate 1
		}
	}
	
	psprint::clean
}


proc psprint::set_option { key value } {
    global PWid PVar PPap PView

    set PVar($key) $value

}

# Delete temporary files
proc psprint::clean {  } {
    global tmppsfile
    global tmppngfile

    file delete $tmppsfile
    file delete $tmppngfile

}

