if ![info exists env(GISBASE)] {
    puts stderr "\nTCLTKGRASS must be installed from the GRASS shell.\n"
    exit 1
}

set tcltkgrass $env(GISBASE)/bin/tcltkgrass

if [catch {open main/tcltkgrass.tcl r} input] {
    puts "\n$input\n"
    exit 1
}

if [catch {open $tcltkgrass w} output] {
    puts "\n$output\n"
    exit 1
}

set dir [pwd]
while {[gets $input line] >= 0} {
    regsub DIR $line $dir line
    puts $output $line
}

close $input
close $output

exec chmod ugo+rx $tcltkgrass
