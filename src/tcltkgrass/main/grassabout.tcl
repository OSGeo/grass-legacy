#
# Program : gis_intro.tcl
# 
#open g.version and read in 
set text [exec $env(GISBASE)/etc/bin/main/inter/g.version]
help {About} {-width 75} {-justify center} $text
