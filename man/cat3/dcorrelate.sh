


dcorrelate.sh <scriptsGRASS Reference Manu<scripts> dcorrelate.sh



NAME
     dcorrelate.sh - Graphically displays the correlation raster
     map layers	in the active frame on the graphics monitor.
     (GRASS Shell Script)

SYNOPSIS
     dcorrelate.sh layer1 layer2 [layer3 [layer4]]

DESCRIPTION
     dcorrelate.sh is a	C-shell	(csh(1)) script	that graphically
     displays the results of an	r.stats	run on two raster map
     layers.  This shell script	is useful for highlighting the
     correlation (or lack of it) among data layers.

     The results are displayed in the active display frame on the
     user's graphics monitor.  dcorrelate.sh erases the	active
     frame before displaying results.

     Parameters:

     layer1 layer2 [layer3 [layer4]]
		       The names of from two to	four existing
		       raster map layers to be included	in the
		       correlation.

NOTES
     This is a shell script that uses r.stats and the UNIX awk
     command to	calculate the correlation among	data layers, and
     uses d.text and d.graph to	display	the results.

     If	three or four map layers are specified,	the correlation
     among each	combination of two data	layers is displayed.

     This command is written for /bin/csh.  If your system
     doesn't support this shell, don't install this script.

FILES
     This program is simply a shell script.  Users are encouraged
     to	make their own shell script programs using similar
     techniques.  See $GISBASE/scripts/dcorrelate.sh.

SEE ALSO
     The UNIX awk command

     d.text, d.graph, r.coin, r.stats

AUTHOR
     Michael Shapiro, U.S. Army	Construction Engineering Research
     Laboratory






GRASS 4.2		Baylor University			1



