###############################################################
# mapprint.tcl - GRASS GIS Manager procedures for postscript and
# lpr printing, and pdf and eps output
# January 2006 Michael Barton, Arizona State University
###############################################################


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

    set pgwd 8.5
    set pght 11
    set docwd 7.5
    set docht 10
    set paper "preset"
    set paper_preset "letter"
    set printmode "lpr"
    set printer ""
    set gsexists 1
    set orient "landscape"
    set res 300
	set mleft 1
	set mright 1
	set mtop 1
	set mbottom 1
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

    global bgcolor
	
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
    set PWid(left) [ frame $PW.left -bg $bgcolor -padx 5 -pady 5]  
    pack $PWid(left) -side left -anchor w

    # paper size, scale
    set PWid(paper) [ frame $PWid(left).paper -bg $bgcolor]  
    pack $PWid(paper) -side top -anchor w

	# preset paper sizes (from ghostscript)
    set row [ frame $PWid(paper).row1 -bg $bgcolor ]
    radiobutton $row.a -variable paper -value "preset" \
		-bg $bgcolor
    Label $row.b -anchor w -text [G_msg "Preset paper type"] -bg $bgcolor
    ComboBox $row.c -label "" -entrybg white\
    	-width 20  -textvariable paper_preset \
		-values {"letter" "a4" "legal" "11x17" "a3" "ledger" "a0" "a1" "a2" } \
		-modifycmd psprint::paper
    pack $row.a $row.b $row.c -side left;
    pack $row -side top -fill x -expand no -anchor n

	# custom paper sizes
    set row [ frame $PWid(paper).row2  -bg $bgcolor]
    radiobutton $row.a -variable paper -value "custom" \
		-bg $bgcolor
    Label $row.b -anchor w -text [G_msg "Custom paper size"] -bg $bgcolor
    Label $row.c -anchor w -text [G_msg "width:"] -bg $bgcolor
    Entry $row.d -width 10 -textvariable pgwd -bg white 
    Label $row.e -anchor w -text [G_msg "  height:"] -bg $bgcolor
    Entry $row.f -width 10 -textvariable pght -bg white
    pack $row.a $row.b $row.c $row.d $row.e $row.f -side left;
    pack $row -side top -fill x -expand no -anchor n
    
	#margins
    set row [ frame $PWid(paper).row3 -bg $bgcolor]
    Label $row.a -anchor w -text [G_msg "Margins  left:"] -bg $bgcolor
    Entry $row.b -width 10 -textvariable mleft -bg white
    Label $row.c -anchor w -text [G_msg " right:"] -bg $bgcolor 
    Entry $row.d -width 10 -textvariable mright -bg white
    Label $row.e -anchor w -text [G_msg " top:"] -bg $bgcolor
    Entry $row.f -width 10 -textvariable mtop -bg white
    Label $row.g -anchor w -text [G_msg " bottom:"] -bg $bgcolor
    Entry $row.h -width 10 -textvariable mbottom -bg white

    pack $row.a $row.b $row.c $row.d $row.e $row.f $row.g $row.h -side left;
	pack $row -side top -fill x -expand no -anchor n

    # portrait or landscape
    set row [ frame $PWid(paper).row4 -bg $bgcolor ]
	LabelEntry $row.a -label [G_msg "Resolution (dpi) for printing and PDF "] \
		-textvariable res -width 4 -entrybg white -bg $bgcolor
    Label $row.b -anchor w -text "  " -bg $bgcolor
    radiobutton $row.c -variable orient -value "landscape" \
		-text "landscape mode" -bg $bgcolor
    radiobutton $row.d -variable orient -value "portrait" \
		-text "portrait mode  " -bg $bgcolor
    pack $row.a $row.b $row.c $row.d -side left;
    pack $row -side top -fill x -expand no -anchor n

    # output options
    set PWid(output) [ frame $PWid(left).output  -bg $bgcolor]  
    pack $PWid(output) -side top -anchor w

    # LPR printer
    set row [ frame $PWid(output).lpr  -bg $bgcolor]
    radiobutton $row.a -variable printmode -value "lpr" -bg $bgcolor
    Label $row.b -anchor w -text [G_msg "Send to LPR printer"] -bg $bgcolor

    pack $row.a $row.b -side left;
    pack $row -side top -fill x -expand no -anchor n

    # Postscript printer
    set row [ frame $PWid(output).psprinter  -bg $bgcolor]
    radiobutton $row.a -variable printmode -value "psprint" -bg $bgcolor
    Label $row.b -anchor w -text [G_msg "Send to postscript printer*"] -bg $bgcolor
    Entry $row.c -width 30 -textvariable printer -bg white

    pack $row.a $row.b $row.c -side left;
    pack $row -side top -fill x -expand no -anchor n

    # PDF file
    set row [ frame $PWid(output).pdffile  -bg $bgcolor]
    radiobutton $row.a -variable printmode -value "pdf" -bg $bgcolor
    Label $row.b -anchor w -text [G_msg "Save to PDF file*              "] -bg $bgcolor
    Entry $row.c -width 30 -textvariable pdffile -bg white
    Button $row.d -text [G_msg "Browse"]  -command { set pdffile \
           [ tk_getSaveFile -title "Output PDF file" -defaultextension ".pdf"] }

    pack $row.a $row.b $row.c $row.d -side left;
    pack $row -side top -fill x -expand no -anchor n

    # EPS file
    set row [ frame $PWid(output).epsfile  -bg $bgcolor]
    radiobutton $row.a -variable printmode -value "eps" -bg $bgcolor
    Label $row.b -anchor w -text [G_msg "Save to EPS file               "] -bg $bgcolor
    Entry $row.c -width 30 -textvariable epsfile -bg white
    Button $row.d -text [G_msg "Browse"] -command { set epsfile \
           [ tk_getSaveFile -title "Output EPS file" -defaultextension ".eps"] }

    pack $row.a $row.b $row.c $row.d -side left;
    pack $row -side top -fill x -expand no -anchor n

	set row [ frame $PWid(output).gsmessage  -bg $bgcolor]
    Label $row.a -anchor w -text [G_msg "*requires ghostscript to be installed and in path"] -bg $bgcolor
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
		close $printmap
		eval exec "cat $tmppsfile | gs  $format -sDEVICE=pdfwrite -r$res -sNOPAUSE -sOutputFile=$pdffile -dBATCH - " 
	}

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

