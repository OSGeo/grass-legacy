#include "windows.h"
#include "gis.h"
#include "variables.h"

site()
{
/*
	printf("Sorry, not available yet\n") ;
	do_pause() ;
*/
	R_open_driver ();
	Dchoose (MAP.name);
	R_close_driver ();
	gorun ("d.sites", "");
}
