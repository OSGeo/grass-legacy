


slide.show.sh <scriptsGRASS Reference Manu<scripts> slide.show.sh



NAME
     slide.show.sh - Displays a	series of raster map layers
     existing in the user's current mapset search path on the
     graphics monitor.
     (GRASS Shell Script)

SYNOPSIS
     slide.show.sh [across=value] [down=value] [mapsets=list]

DESCRIPTION
     slide.show.sh is a	UNIX Bourne shell macro	which clears the
     entire screen, creates a series of	display	frames on the
     graphics monitor, and displays in slideshow format	each of
     the raster	map layers listed in the user-specified	mapsets.
     This is a shell script example which makes	extensive use of
     GRASS and UNIX commands.  Users are encouraged to examine
     this macro	and develop similar on-line demos using	their own
     data files.


     Parameters:

     across=value	 The number of display frames across the
			 graphics monitor screen to be used for
			 map display.
			 Default:  4

     down=value		 The number of display frames down the
			 graphics monitor screen to be used for
			 map display.
			 Default:  3

     mapsets=list	 The names of the mapsets under	the
			 user's	current	location whose raster map
			 layers	are to be displayed, separated by
			 commas.
			 Default:  All mapsets listed in the
			 user's	current	mapset search path.

FILES
     See the file slide.show.sh	under $GISBASE/scripts.

SEE ALSO
     d.display,	d.erase, d.text, g.mapsets, 3d.view.sh,	demo.sh,
     grass.logo.sh, show.fonts.sh

AUTHOR
     James Westervelt, U.S. Army Construction Engineering
     Research Laboratory






GRASS 4.2		Baylor University			1



