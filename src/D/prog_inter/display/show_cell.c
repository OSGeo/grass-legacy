#include "windows.h"
#include "gis.h"
#include "variables.h"

show_cell()
{
	FILE *popen() ;
	FILE *fptr ;
	struct Cell_head wind ;
	char buff[256] ;

/* Show the map name in name window */
	R_open_driver();
	Dchoose(NAM.name) ;
	Derase(D_translate_color("black")) ;
	R_close_driver();

	if (NULL != (fptr = popen("Dtext", "w")))
	{
		fprintf(fptr, ".S 60\n.C white\n.B\n") ;
		fprintf(fptr, "%s\n", G_get_cell_title (mapname, mapset)) ;
		pclose(fptr) ;
	}

	R_open_driver();

/* Draw cell map */
	G_get_window(&wind) ;
	G_set_window(&wind) ;
	Dchoose(MAP.name) ;
	Derase(D_translate_color("black")) ;
	printf("\n\n  Displaying map %s from mapset %s with %d categories\n",
		mapname, mapset, G_number_of_cats (mapname, mapset) ) ;
	Dcell(mapname, mapset, 0) ;

/* Draw tiny map */
	G_get_default_window(&wind) ;
	G_set_window(&wind) ;
	Dchoose(REF.name) ;
	Dcell(mapname, mapset, 0) ;
	R_close_driver();

/* Draw outline of current window on tiny map */
	Draw_outline() ;
}

Draw_outline()
{
	struct Cell_head wind ;
	FILE *popen() ;
	FILE *fptr ;

	/* G__get_window called here because we need to read the current window
	 * which may have changed.  G_get_window reads thw window only once.
	 */
	G__get_window (&wind,"","WIND",G_mapset()) ;

	if (NULL != (fptr = popen("Dmapgraph", "w")))
	{
		double dx, dy, x, y ;
		dx = wind.east - wind.west ;
		dy = wind.north - wind.south ;
		fprintf(fptr, "move %.2lf %.2lf\n", wind.west, wind.south) ;
		x = wind.west ;
		y = wind.south + .25 * dy ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		y = wind.north - .25 * dy ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		y = wind.north ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		x = wind.west + dx * .25 ;
		fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		x = wind.east - dx * .25 ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		x = wind.east ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		y = wind.north - dy * .25 ;
		fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		y = wind.south + dy * .25 ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		y = wind.south ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		x = wind.east - dx * .25 ;
		fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		x = wind.west + dx * .25 ;
		fprintf(fptr, "color white\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		x = wind.west ;
		fprintf(fptr, "color black\n") ; fprintf(fptr, "draw %.2lf %.2lf\n", x, y) ;
		pclose(fptr) ;
	}
}
