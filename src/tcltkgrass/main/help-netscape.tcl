# help-netscape.tcl
# displays help screen for netscape
# created by al

help {html-browser} {-width 55} {-justify left} \
{
external browser for html manuals in GRASS
---------------------------------------------------

This utility allows the use of an external, 
configurable browser for viewing the html-
manuals for the GRASS GIS. 

You can load the manual page to your browser by
pressing the Control Key (Ctrl/Strg) and the middle
mouse button over the Run button in any TclTkGRASS
module. 

This works with all browsers that support the
-remote openURL() option. The browser must be in 
your path and running under our user id on your
display. 

You must first start your browser and configure the
path to the html-files of your GRASS installation.

Use Config->Options->Configure html-browser to
set the options for your system. The settings are 
saved and reloaded on start. 

Auto raise means that the browser's window pops
up on loading of the manual. You can configure
if the manual pages should be shown in a new 
window of the browser or if the window should
be reused (so that you can cycle through different 
pages in one window). 

Use $(GISBASE)/documents/html as path and file:
as prefix if you  have installed the html-manuals 
in the default directory. 

If you have your manpages installed on a local 
web server, please use http:// as prefix and the 
URL of your intranet location (without preceding
slash!). 


This was written by Andreas Lange, 
(andreas.lange@rhein-main.de). If you find bugs 
please contact the GRASS maintainers.

Use at your own risk. 
All copyrights reserved, 
May 2000, Andreas Lange

}

