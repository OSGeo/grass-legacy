a=`Dmenu << EOFEOF
# set the background color
.B brown
# set the text color
.C yellow
# set the text size in % of entire screen height
.S 3
# set the top edge
.T 10
# set the LEFT edge
.L 10
# The menu Title
Sample Menu
# the options
option 1
option 2
option 3
option 4
option 5
option 6
EOFEOF
`

echo You have just chosen option $a
