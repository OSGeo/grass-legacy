#include "windows.h"
#include "gis.h"

show_window()
{
	FILE *popen() ;
	FILE *fptr ;
	struct Cell_head wind ;

	if (-1 == G_get_set_window(&wind))
		return(-1) ;

	R_open_driver();
	Dchoose(COO.name) ;
	Derase(D_translate_color("black")) ;
	R_close_driver();

	if (NULL != (fptr = popen("Dtext", "w")))
	{
		fprintf(fptr, ".S 15\n.C white\n") ;
		fprintf(fptr, "WINDOW\n") ;
		fprintf(fptr, "     S:%10.1lf W:%10.1lf\n", wind.south, wind.west) ;
		fprintf(fptr, "     N:%10.1lf E:%10.1lf\n", wind.north, wind.east) ;
		fprintf(fptr, "\nCELL-SIZE\n") ;
		fprintf(fptr, "     n-s:%6.1lf   e-w:%6.1lf\n", wind.ns_res, wind.ew_res) ;
		pclose(fptr) ;
	}
}
