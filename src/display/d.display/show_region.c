#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "gis.h"
#include "D.h"

int show_region()
{
	FILE *fptr ;
	struct Cell_head wind ;
	char wbuf[50], ebuf[50], nbuf[50], sbuf[50];
	char nsbuf[50], ewbuf[50];

	if (-1 == G_get_set_window(&wind))
		return(-1) ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	Dchoose(COO.name) ;
	Derase("black") ;
	R_close_driver();

	if (NULL != (fptr = popen("d.text", "w")))
	{
		G_format_northing (wind.south, sbuf, wind.proj);
		G_format_northing (wind.north, nbuf, wind.proj);
		G_format_easting  (wind.east,  ebuf, wind.proj);
		G_format_easting  (wind.west,  wbuf, wind.proj);
		G_format_resolution  (wind.ns_res,  nsbuf, wind.proj);
		G_format_resolution  (wind.ew_res,  ewbuf, wind.proj);
		fprintf(fptr, ".S 15\n.C white\n") ;
		fprintf(fptr, "REGION\n") ;
		fprintf(fptr, "  S:%11s W:%11s\n", sbuf, wbuf);
		fprintf(fptr, "  N:%11s E:%11s\n", nbuf, ebuf);
		fprintf(fptr, "\nCELL-SIZE\n") ;
		fprintf(fptr, "  n-s:%7s   e-w:%7s\n", nsbuf, ewbuf);
		pclose(fptr) ;
	}

	return 0;
}
