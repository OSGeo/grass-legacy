#!/bin/sh
# the next line restarts using wish \
exec $GRASS_WISH "$0" "$@"

lappend auto_path $env(GISBASE)/bwidget
package require -exact BWidget 1.2.1

set env(GISDBASE) [exec g.gisenv get=GISDBASE]
set env(LOCATION_NAME) [exec g.gisenv get=LOCATION_NAME]
set env(MAPSET) [exec g.gisenv get=MAPSET]

set gisdbase [exec g.gisenv get=GISDBASE]
set location_name [exec g.gisenv get=LOCATION_NAME]
set mapset [exec g.gisenv get=MAPSET]

set labwidth 12
set entrywidth 60
set vecterr 1
set shperr 1

proc selshp { } {
    global shapefile

    set types { {{Shape Files} { {.shp} {.SHP} } }
                 {{All Files}        *           } }

    set shp [ tk_getOpenFile -title "Select shapefile" -filetypes $types ]

    if {$shp != ""} { 
        set shapefile $shp 
        check_shp
    }
}

proc conf_butt { } {
    global vecterr shperr efok resultmsg

    if { $vecterr || $shperr } {
        $efok configure -state disabled
    } else {
        $efok configure -state normal
    }

    set resultmsg ""
}

# check vector name
proc check_vect {  } {
    global vector vecterr vecterrmsg gisdbase location_name mapset
 
    set v [ string trim $vector " " ]
    
    set vecterrmsg ""
    set vecterr 0
 
    if { $v  == ""} { 
        set vecterr 1
    } else {
	set vf "$gisdbase/$location_name/$mapset/vector/$v"
	if { [ file exists $vf ] } {
	    set vecterrmsg "Vector already exists"
            set vecterr 1
	}
    }
    conf_butt
}

# check shapefile name
proc check_shp {  } {
    global shapefile shperr shperrmsg
 
    set s [ string trim $shapefile " " ]
    
    set shperrmsg ""
    set shperr 0
 
    if { $s  == ""} { 
        set shperr 1
    } else {
        set shape ""
        regexp {^/.*/([a-zA-Z][^/.]+)\.[Ss][Hh][Pp]} $s x shape
	if { $shape  == "" } {
	    set shperrmsg "Wrong shapefile name (relative path or first char digit or dot in name or not shape)"
            set shperr 1
	}
    }
    conf_butt
}

# NOTEBOOK
set nb [NoteBook .nb]
#$nb configure -width 300 -height 500
$nb configure 
pack .nb -fill both -expand yes

# SHAPEFILE
set efpage [$nb insert end efpage -text "External format (shapefile)"]
$nb raise efpage
set eff [ frame $efpage.frm ]
set efsw [ScrolledWindow $eff.sw -relief sunken -borderwidth 2]
set efframe [ ScrollableFrame $eff.frm -height 5 -width 20 ]
$efsw setwidget $efframe
pack $eff $efsw $efframe -fill both -expand yes

# vector name
set row [ frame $efframe.vect ]
Label $row.a -text "Vector name:" -justify left -width $labwidth 
set vectentry [Entry $row.b -width $entrywidth -text "" -textvariable vector -command check_vect]
bind $vectentry <KeyRelease> check_vect
pack $row.a $vectentry -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.vecterr ]
Label $row.a -text "" -justify left -textvariable vecterrmsg -fg red
pack $row.a -side left
pack $row -side top -fill both -expand yes

# shapefile path
set row [ frame $efframe.file ]
Label $row.a -text "Shapefile:" -justify left -width $labwidth 
set shpentry [ Entry $row.b -width $entrywidth -text "" -textvariable shapefile ]
bind $shpentry <KeyRelease> check_shp
Button $row.c -text "Browse" -command { selshp }
pack $row.a $row.b $row.c -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.shperr ]
Label $row.a -text "" -justify left -textvariable shperrmsg -fg red
pack $row.a -side left
pack $row -side top -fill both -expand yes

# HEADER
set row [ frame $efframe.header ]
Label $row.a -text "Header informations (optional)" -justify center
pack $row.a 
pack $row -side top -fill both -expand yes

set row [ frame $efframe.org ]
Label $row.a -text "Organization:" -justify left -width $labwidth 
Entry $row.b -width $entrywidth -text "" -textvariable header(organization)
pack $row.a $row.b -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.dd ]
Label $row.a -text "Digit date:" -justify left -width $labwidth 
Entry $row.b -width 20 -text "" -textvariable header(digit_date)
pack $row.a $row.b -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.dn ]
Label $row.a -text "Digit name:" -justify left -width $labwidth 
Entry $row.b -width $entrywidth -text "" -textvariable header(digit_name)
pack $row.a $row.b -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.mn ]
Label $row.a -text "Map name:" -justify left -width $labwidth 
Entry $row.b -width $entrywidth -text "" -textvariable header(map_name)
pack $row.a $row.b -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.md ]
Label $row.a -text "Map date:" -justify left -width $labwidth 
Entry $row.b -width 20 -text "" -textvariable header(map_date)
pack $row.a $row.b -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.ms ]
Label $row.a -text "Map scale:" -justify left -width $labwidth 
Entry $row.b -width 20 -text "" -textvariable header(map_scale)
pack $row.a $row.b -side left
pack $row -side top -fill both -expand yes

set row [ frame $efframe.oi ]
Label $row.a -text "Other info:" -justify left -width $labwidth 
Entry $row.b -width $entrywidth -text "" -textvariable header(info)
pack $row.a $row.b -side left
pack $row -side top -fill both -expand yes

proc create_vect { } {
    global env gisdbase location_name mapset vector shapefile header resultmsg
 
    set v [ string trim $vector " " ]
    set vdir "$gisdbase/$location_name/$mapset/vector/$v"
    file mkdir $vdir

    set fpath "$vdir/head"
    set f [open $fpath w]
    puts $f "ORGANIZATION: $header(organization)"
    puts $f "DIGIT DATE: $header(digit_date)"
    puts $f "DIGIT NAME: $header(digit_name)"
    puts $f "MAP NAME: $header(map_name)"
    puts $f "MAP DATE: $header(map_date)"
    puts $f "MAP SCALE: $header(map_scale)"
    puts $f "OTHER INFO: $header(info)"
    puts $f "ZONE: 0"
    puts $f "MAP THRESH: 0.0"
    close $f

    set fpath "$vdir/frmt"
    set f [open $fpath w]
    puts $f "FORMAT: shape"
    puts $f "SHAPE: $shapefile"
    close $f

    set cmd "v.build map=$v"
    set shell $env(SHELL)
    eval "exec echo \"$cmd\" | $shell >@stdout 2>@stdout"

    check_vect
    set resultmsg "Vector created"
}

# BUTTONS
set efok [Button $efframe.ok -text "Create vector" -command create_vect ]
pack $efok 

set row [ frame $efframe.res ]
Label $row.a -text "" -justify left -textvariable resultmsg
pack $row.a -side left
pack $row -side top -fill both -expand yes

conf_butt

