#include "variables.h"
#include "lproto.h"
#include <stdio.h>

int color()
{
	char buff[128] ;
	if (*mapname == '\0')
	{
		fprintf (stdout,"You must draw a raster map before using this option\n") ;
		do_pause() ;
		return -1;
	}
	sprintf(buff, "'%s@%s'", mapname, mapset) ;
	gorun("d.colors", buff) ;

	return 0;
}
