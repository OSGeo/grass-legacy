#
# Program : gis_intro.tcl
# 
#open g.version and read in 
set text [exec g.version -c]
help {About} {-width 75} {-justify center} $text
