#!/bin/sh
# the next line restarts using tclsh \
exec $GRASS_WISH "$0" "$@"

global env
global argv
global URL
set URL $argv
puts "Using $URL"

source $env(GISBASE)/documents/nviz/html_library.tcl

proc make_help_window { w } {

        frame $w
	frame $w.f2
	frame $w.f1
        button $w.f2.quit  -command exit  -text Close -relief raised -bd 3
        scrollbar $w.f1.scrollbar  -command "$w.f1.text yview"  -orient v
        scrollbar $w.f1.scrollbar1  -command "$w.f1.text xview"  -orient h
        text $w.f1.text  -yscrollcommand "$w.f1.scrollbar set" \
        -xscrollcommand "$w.f1.scrollbar1 set" \
	-padx 3 -pady 3 -takefocus 0 \
	-background white -foreground black

        pack $w -side top -expand 1 -fill both
	pack $w.f1 -side top -expand 1 -fill both
	pack $w.f2 -side bottom -expand 0 -fill both
        pack $w.f2.quit -side bottom -fill x -expand 0
	pack $w.f1.text -side right -fill both -expand 1
	pack $w.f1.scrollbar -side left -expand 0 -fill y
	pack $w.f1.scrollbar1 -side bottom -expand 0 -fill x -before $w.f1.text

}

proc render {file targ} {
        global HM$targ Url
        global Running message

        set fragment ""
        regexp {([^#]*)#(.+)} $file dummy file fragment
        if {$file == "" && $fragment != ""} {
                HMgoto $targ $fragment
                return
        }
        HMreset_win $targ
        set Running busy
        set message "Displaying $file"
        update idletasks
        if {$fragment != ""} {
                HMgoto $targ $fragment
        }
        set Url $file
        HMparse_html [get_html $file] {HMrender .help_menu.f1.text}
        set Running ready
        HMset_state $targ -stop 1       ;# stop rendering previous page if busy
        set message ""
}

proc get_html {file} {
        global Home
        if {[catch {set fd [open $file]} msg]} {
                return "
                        <title>Bad file $file</title>
                        <h1>Error reading $file</h1><p>
                        $msg<hr>
                        <a href=$Home>Go home</a>
                "
        }
        set result [read $fd]
        close $fd
        return $result
}

proc HMlink_callback {win href} {
        global Url

        if {[string match #* $href]} {
                render $href $win
                return
        }
        if {[string match /* $href]} {
                set Url $href
        } else {
                set Url [file dirname $Url]/$href
        }
        update
        render $Url $win
}

proc HMset_image {win handle src} {
        global Url message
        if {[string match /* $src]} {
                set image $src
        } else {
                set image [file dirname $Url]/$src
        }
        set message "fetching image $image"
        update
        if {[string first " $image " " [image names] "] >= 0} {
                HMgot_image $handle $image
        } else {
                set type photo
                if {[file extension $image] == ".bmp"} {set type bitmap}
                catch {image create $type $image -file $image} image
                HMgot_image $handle $image
        }
}


proc HMtag_base {win param text} {
        global Url
        upvar #0 HM$win var
        HMextract_param $param href Url
}



set w .help_menu

catch {make_help_window $w}
HMinit_win $w.f1.text
HMset_state $w.f1.text -size 4
HMset_indent $w.f1.text 1.2
render $URL $w.f1.text
