


3d.view.sh <scripts> GRASS Reference Manual  <scripts> 3d.view.sh



NAME
     3d.view.sh	- Displays several 3-dimensional views of a
     landscape on the user's graphics monitor.
     (GRASS Shell Script)

SYNOPSIS
     3d.view.sh
     3d.view.sh	help
     3d.view.sh	file=mapname  ef=mapname  vh=viewing_height
     sv=sink_value  exag=exag  lf=line_frequency
     back=background_color

DESCRIPTION
     3d.view.sh	is a Bourne shell (sh(1)) script that displays
     several 3-dimensional views of a landscape	on the user's
     graphics monitor.	It erases the graphics monitor and then
     prepares it for the display of nine equally-sized frames.
     The user-specified	raster map layer (given	by file=name) is
     displayed using d.rast in the middle frame.  The remaining
     frames are	then used to display 3-d perspective views.  The
     top middle	panel is a view	from the north,	the top	right
     from the north-east, the right from the east, and so on.
     Each is drawn with	a call to the d.3d program.  The viewing
     angles are	calculated automatically.  If options are not
     stated on the command line, default values	will be	used.
     These values are listed under Parameters, below.

     Parameters:

     file=mapname      Name of raster map layer	to be displayed.
		       Default:	 elevation

     ef=mapname	       Name of raster map layer	whose category
		       values will supply the elevation	values
		       used to generate	3-d perspective	views.
		       Default:	 elevation

     vh=viewing_height Height (in meters) of the location from
		       which scenes will be viewed.
		       Default:	 30000

     sv=sink_value     Sink factor value, causing the image to be
		       displayed lower,	or higher, on the
		       graphics	screen.
		       Default:	 0

     exag=vertical_exaggeration
		       Vertical	exaggeration factor of the values
		       in the elevation	file.
		       Default:	 3

     lf=line_frequency Contour intervals at which vector grid



GRASS 4.2		Baylor University			1






3d.view.sh <scripts> GRASS Reference Manual  <scripts> 3d.view.sh



		       lines will be drawn, in meters.
		       Default:	 20

     back=background_color
		       Color of	the background of the display
		       frames.
		       Options:	 red, orange, yellow, green,
		       blue, indigo, violet, magenta, brown,
		       gray, white, and	black
		       Default:	 black

NOTES
     In	the spearfish sample data base,	the user must specify a
     viewing height when running 3d.view.sh.  Note also	that the
     raster elevation map layers in the	PERMANENT mapset under
     spearfish are named elevation.dem and elevation.dma.

     This program will not prompt the user for inputs;	if the
     user types	3d.view.sh without program arguments on	the
     command line, default values will be used.

FILES
     This program is simply a shell script.  Users are encouraged
     to	make their own shell scripts using similar techniques.
     See $GISBASE/scripts/3d.view.sh.

SEE ALSO
     d.3d, d.rast

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory























GRASS 4.2		Baylor University			2



