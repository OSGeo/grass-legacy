#
# Program : monscale.tcl
# 
#start d.scale -i: prints map scale of GRASS Monitor
set text [exec d.scale -i]
help {Current scale in GRASS Monitor} {-width 75} {-justify center} $text
