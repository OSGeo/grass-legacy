:
#
#   Display primary colors and the text that refers to them
#   Written by Dave Gerdes, CERL  12/90

if [ $# -ge 1 -a $1. = help. ]
then
    echo ""
    echo "This program will display each of the primary colors"
    echo "    available in GRASS along with its text name."
    echo ""
    exit 1
fi

#  Clear screen
d.frame -e

#  set arguments for 'for' loop
#  Note this was really done this way so I could count them w/ '$#'

set red orange yellow green blue indigo white black gray brown magenta aqua

# Determine vertical size based on number of colors
i=0
size=`echo foo | awk "{ print 100.0 / $# }"`

for col 
do
    i=`expr $i + 1`

#   Awk is the only tool available for floating point math
    y1=`echo foo | awk "{ print $size * $i }"`
    y2=`echo foo | awk "{  print $size * ($i - 1.0) }"`

#  Create new window and erase using primary color
    d.frame -c at=$y2,$y1,0,100 frame=$col
    d.frame -s frame=$col
    d.erase color=$col

#  Draw name of text inside given window
    textcol=white
    if [ $col = white ]
    then
        textcol=black
    fi
#    textcol=black
#    if [ $col = black ]
#    then
#        textcol=white
#    fi

#  Bold command is only available on input stream of Dtext
    (echo .B 1;echo $col) | d.text size=75 color=$textcol
done

#  Reselect full_screen before exit,  so user is not stuck w/ tiny window
d.frame frame=full_screen

exit 0
