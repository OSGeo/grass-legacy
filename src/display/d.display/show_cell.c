#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "gis.h"
#include "D.h"
#include "variables.h"

int show_cell()
{
	FILE *fptr ;
	struct Cell_head wind ;
	char buff[256] ;

/* Show the map name in name window */
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	Dchoose(NAM.name) ;
	Derase("black") ;
	R_close_driver();

	if (NULL != (fptr = popen("d.text", "w")))
	{
		fprintf(fptr, ".S 60\n.C white\n.B\n") ;
		fprintf(fptr, "%s\n", G_get_cell_title (mapname, mapset)) ;
		pclose(fptr) ;
	}

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

/* Draw cell map */
	G_get_window(&wind) ;
	G_set_window(&wind) ;
	Dchoose(MAP.name) ;
	Derase("black") ;
	fprintf (stdout,"\n\n  Displaying map %s from mapset %s\n", mapname, mapset);
	Dcell(mapname, mapset, 0) ;

/* Draw tiny map */
	G_get_default_window(&wind) ;
	G_set_window(&wind) ;
	Dchoose(REF.name) ;
	Dcell(mapname, mapset, 0) ;
	R_close_driver();

/* Draw outline of current window on tiny map */
	Draw_outline() ;

	return 0;
}

int Draw_outline()
{
	struct Cell_head wind ;
	FILE *fptr ;

	/* G__get_window called here because we need to read the current window
	 * which may have changed.  G_get_window reads thw window only once.
	 */
	G__get_window (&wind,"","WIND",G_mapset()) ;

	if (NULL != (fptr = popen("d.mapgraph", "w")))
	{
		double dx, dy, x, y ;
		dx = wind.east - wind.west ;
		dy = wind.north - wind.south ;
		fprintf(fptr, "move %.2f %.2f\n", wind.west, wind.south) ;
		x = wind.west ;
		y = wind.south + .25 * dy ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		y = wind.north - .25 * dy ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		y = wind.north ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		x = wind.west + dx * .25 ;
		fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		x = wind.east - dx * .25 ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		x = wind.east ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		y = wind.north - dy * .25 ;
		fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		y = wind.south + dy * .25 ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		y = wind.south ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		x = wind.east - dx * .25 ;
		fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		x = wind.west + dx * .25 ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		x = wind.west ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2f %.2f\n", x, y) ;
		pclose(fptr) ;
	}

	return 0;
}
