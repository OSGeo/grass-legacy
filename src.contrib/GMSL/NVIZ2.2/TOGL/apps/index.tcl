# index.tcl

# An Tk/OpenGL widget demo using color-index mode.
# Brian Paul  (brianp@ssec.wisc.edu)



proc setup {} {
    wm title . "Color index demo"

    togl .win -width 200 -height 200  -rgba false  -privatecmap false -double false

    pack .win -expand true -fill both
}



# Execution starts here!
setup

