GIS.M README
17 January 2006

Prototype new TclTk GIS Manager 2 for GRASS 6

gis.m uses TclTk canvases instead of x-displays for display of maps and display management. Based on this number of other updated interface features have been implemented. This includes better display management, independent layer trees and zoom (region settings) for each map display opened, a new save display to file routine, and a new output console. Note that display management controls are attached to the display windows now. Also, the print button saves a postscript (*.eps) copy of the display to your home folder for printing. It is the first stage of an overall roadmap to develop a next generation GIU for GRASS 6 and beyond.

Installation

You've already unzipped and untarred the gism.tgz package. So now you only have to

1) put the shell script gis.m into the $GISBASE/scripts folder and
2) put the gm folder into the $GISBASE/etc folder

To start gis.m, simply type gis.m & from the GRASS command line. 

Enjoy!
Michael Barton (ASU)
michael.barton@asu.edu