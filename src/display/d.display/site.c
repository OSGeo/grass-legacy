#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "D.h"
#include "gis.h"
#include "variables.h"

int site()
{
/*
	fprintf (stdout,"Sorry, not available yet\n") ;
	do_pause() ;
*/
	R_open_driver ();
	Dchoose (MAP.name);
	R_close_driver ();
	gorun ("d.sites", "");

	return 0;
}
