# This is a general file browser
# Call create_file_browser to get a file or -1 
# if the user cancels

global file_browser
global last_dir
set last_dir $env(GISDBASE)/$env(LOCATION_NAME)/$env(MAPSET)/images

if {![file isdirectory $last_dir]} {
    set last_dir $env(HOME)
    if {![file isdirectory $env(HOME)]} {
        set last_dir "."
    }
}

proc set_file_browser_filename {w name} {
    $w.filename delete 0 end
    if {[string length $name] > 0} {
	$w.filename insert 0 ${name}
	set_selection_from_file_browser_filename $w
    }
}

proc set_file_browser_directories {w name} {
    global file_browser

    if {$name != ""} then {
	cd $name
	set file_browser($w,cur_dir) [exec pwd]
    }

    refresh_file_browser $w
}


proc set_selection_from_file_browser_filename { w } {
    $w.filename selection from 0 
    $w.filename selection to end 
}

proc create_file_browser {{w .file_browser} {mode 0} {no_top 0}} {
    global file_browser Nv_
    global last_dir
    
    catch {destroy $w}
    
    if $no_top {
	frame $w
	pack $w
    } else {
	# Figure out where the toplevel widget is so we can put
	# ourselves over that window (this avoids popping up on top of
	# the rendering window which can be annoying if we are placing
	# labels, etc.)
	set geom [split [wm geometry $Nv_(APP)] +]
	set geom_x [lindex $geom 1]
	set geom_y [lindex $geom 2]
	toplevel $w 
	wm geometry $w "+$geom_x+$geom_y"
    }

    if {![file isdirectory $last_dir]} {
    	file mkdir $last_dir
    }

#   set file_browser($w,cur_dir) [exec pwd]
    set file_browser($w,cur_dir) $last_dir

#   set file_browser($w,start_dir) [exec pwd]
    set file_browser($w,start_dir) $last_dir

    set file_browser($w,Answer) ""
    
    entry $w.filename -bd 2 -relief sunken
    bind $w.filename <Return> "set_selection_from_file_browser_filename $w"
    frame $w.main
    frame     $w.main.directories
    label     $w.main.directories.label -text DIRECTORIES
    frame     $w.main.directories.f
    listbox   $w.main.directories.f.list -bd 2 -relief sunken \
	-exportselection no -selectbackground LightYellow1 \
	-yscroll "$w.main.directories.f.scroll set" -selectmode single
    scrollbar $w.main.directories.f.scroll -command \
	"$w.main.directories.f.list yview"
    
    bind $w.main.directories.f.list <ButtonRelease-1> \
	"file_browser_select_directories  %W %y $w"
    
    frame     $w.main.files
    label     $w.main.files.label -text FILES
    frame     $w.main.files.f
    listbox   $w.main.files.f.list -bd 2 -relief sunken -exportselection no \
	-selectbackground LightYellow1  -yscroll "$w.main.files.f.scroll set" \
	-selectmode single
    scrollbar $w.main.files.f.scroll -command "$w.main.files.f.list yview"

    bind $w.main.files.f.list <ButtonRelease-1> "file_browser_select_file %W %y $w"
    
    button $w.accept -text Accept -command "fileBrowser_accept_cmd $w"
    button $w.cancel -text Cancel -command "fileBrowser_cancel_cmd $w"
    
    frame $w.cur_directory
    label $w.cur_directory.label -text "CURRENT:"
    label $w.cur_directory.entry -relief sunken -textvariable file_browser($w,cur_dir) 
    
    pack $w.filename -side top -expand yes -fill x
    pack $w.main     -side top
    
    pack $w.main.directories -side left
    pack $w.main.directories.label -side top
    pack $w.main.directories.f -side top
    pack $w.main.directories.f.list -side left
    pack $w.main.directories.f.scroll -side left -expand yes -fill y
    
    pack $w.main.files -side left
    pack $w.main.files.label -side top
    pack $w.main.files.f -side top
    pack $w.main.files.f.list -side left
    pack $w.main.files.f.scroll -side left -expand yes -fill y

    pack $w.cur_directory.label $w.cur_directory.entry -side left -expand yes
    pack $w.cur_directory
    pack $w.accept $w.cancel -side left -expand 1
    
    refresh_file_browser $w

# global to hold most recent directory ?
# would be nice to make recent dir for each different file browser
    
#   set_file_browser_directories $w {} 
    set_file_browser_directories $w $last_dir
    if {$no_top == 0} {
	wm title $w "File Browser"
	wm protocol $w WM_DELETE_WINDOW "destroy $w"
    }
    
    if {$mode} {grab $w}
    tkwait window $w
    
    return $file_browser($w,Answer)
    
}

proc fileBrowser_accept_cmd  {w} {
    global file_browser 
    
    cd $file_browser($w,start_dir)
    
    # Make sure a file has been selected first
    set temp [$w.filename get]
    if {$temp != ""} then {
	set last_dir $file_browser($w,cur_dir)
	set file_browser($w,Answer) $file_browser($w,cur_dir)/[$w.filename get]
	destroy $w
    } else {	
	return
    }
}

proc fileBrowser_cancel_cmd {w} {
    global file_browser 
    
    cd $file_browser($w,start_dir)
    
    set file_browser($w,Answer) ""
    destroy $w
    set file_browser($w,Answer) -1
    
    return 
}

proc file_browser_select_file {W y w} {
    set near [ $W nearest $y ]
    $W selection set $near $near
    eval set_file_browser_filename $w {[$W get $near]}
}

proc file_browser_select_directories {W y w} {
    set near [ $W nearest $y ]
    $W selection set $near $near
    eval set_file_browser_directories $w {[$W get $near]}
}


proc refresh_file_browser { w } {
    global file_browser
    
    set cur_dir $file_browser($w,cur_dir)
    
    $w.main.directories.f.list delete 0 end
    $w.main.files.f.list delete 0 end
    $w.main.directories.f.list insert end "."
    $w.main.directories.f.list insert end ".."
    foreach i [exec ls $cur_dir] {
	if {[file isdir $i]} then {
	    $w.main.directories.f.list insert end $i
	} else {
	    $w.main.files.f.list insert end $i
	}
    }
}





