


grass.logo.sh <scriptsGRASS Reference Manu<scripts> grass.logo.sh



NAME
     grass.logo.sh - Displays a	GRASS/Army Corps of Engineers
     logo in the active	display	frame on the graphics monitor.
     (GRASS Shell Script)

SYNOPSIS
     grass.logo.sh

DESCRIPTION
     grass.logo.sh is primarily	a demonstration	of the GRASS
     d.graph program in	a UNIX Bourne shell macro that generates
     the U.S. Army Corps of Engineers logo.  Users are encouraged
     to	generate their own unique logos	by writing similar macro
     shell scripts.  Examine the contents of the input file
     called grass.logo.sh located in the GRASS shell script
     command directory ($GISBASE/scripts) to see how the GRASS
     logo was generated	using d.graph graphics commands.  The
     coordinates for this logo were taken from a drawing done on
     graph paper.

     To	view the graphics described by this file use d.graph or
     d.mapgraph, making	sure that a copy of this file is either
     in	your current directory or is given by its full path name.

NOTES
     grass.logo.sh, like d.rast, will overwrite	(not overlay)
     whatever display appears in the active graphics frame.

     This program requires no command line arguments.

SEE ALSO
     d.font, d.graph, d.mapgraph, d.rast

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory



















GRASS 4.2		Baylor University			1



