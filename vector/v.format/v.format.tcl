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
	    set vecterrmsg "Vector map '$v' already exists"
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
        regexp {^/.*/([a-zA-Z][^/.]+)\.[Ss][Hh][Pp]$} $s x shape
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
bind $vectentry <Motion> check_vect
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
Label $row.a -text "Map scale: 1:" -justify left -width $labwidth 
Entry $row.b -width 20 -text "10000" -textvariable header(map_scale)
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
        
    regexp {(.*/[a-zA-Z][^/.]+)\.[Ss][Hh][Pp]$} $shapefile x baseshape
    puts $f "FORMAT: shape"
    puts $f "SHAPE: $baseshape"
    close $f

    set cmd "v.build map=$v"
    eval "exec $cmd >@stdout 2>@stdout"

    set vector ""
    set resultmsg "Vector map '$v' created"
}

# BUTTONS
set efbut [ frame $efframe.buttons] 
pack $efbut -side top 

set efok [Button $efbut.ok -text "Create vector" -command create_vect ]
pack $efok -side left

set help [button $efbut.help -text "Help" -command { exec $env(GRASS_HTML_BROWSER) $env(GISBASE)/docs/html/v.format.html } ]
pack $help -side left

set close [button $efbut.close -text "Close" -command { exit } ]
pack $close -side right

set row [ frame $efframe.res ]
Label $row.a -text "" -justify left -textvariable resultmsg
pack $row.a -side left
pack $row -side top -fill both -expand yes

conf_butt

# OUTPUT FORMAT
set ofpage [$nb insert end ofpage -text "Output format (Native or PostGIS)"]
set off [ frame $ofpage.frm ]
set ofsw [ScrolledWindow $off.sw -relief sunken -borderwidth 2]
set offrame [ ScrollableFrame $off.frm -height 5 -width 20 ]
$ofsw setwidget $offrame
pack $off $ofsw $offrame -fill both -expand yes

set format [exec g.gisenv store=mapset get=GV_FORMAT] 
if { $format == "" } { set format "NATIVE" }

set row [ frame $offrame.format ]
ComboBox $row.a -label "Output format:  " -width 20  -textvariable format \
                -values {"NATIVE" "POSTGIS"} 

pack $row.a -side left
pack $row -side top -fill x -anchor w

set pgdb [exec g.gisenv store=mapset get=GV_PGIS_DATABASE] 

set row [ frame $offrame.pgdb ]
Label $row.a -text "PostGIS database:" -justify left
set pgdbentry [Entry $row.b -width 80 -textvariable pgdb -command set_pgdb]
pack $row.a $pgdbentry -side left -anchor w
pack $row -side top -fill x -anchor w  

set row [ frame $offrame.pgdbhelp ]
Label $row.a -text "PostGIS database example: host=srv1,dbname=grass1,user=Cimrman" -justify left
pack $row.a -side left -anchor w
pack $row -side top -fill x -anchor w  

proc set_format { } {
    global format pgdb

    set cmd "g.gisenv store=mapset set=GV_FORMAT=$format"
    puts $cmd
    eval exec $cmd
    
    set cmd "g.gisenv store=mapset set=GV_PGIS_DATABASE=$pgdb"
    puts $cmd
    eval exec $cmd

} 

# BUTTONS
set ofbut [ frame $offrame.buttons] 
pack $ofbut -side top 

set ofok [Button $ofbut.ok -text "Apply" -command set_format ]
pack $ofok -side left

set ofclose [button $ofbut.close -text "Close" -command { exit } ]
pack $ofclose -side right

