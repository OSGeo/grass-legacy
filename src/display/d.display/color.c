#include "variables.h"
#include <stdio.h>

color()
{
	char buff[128] ;
	if (*mapname == NULL)
	{
		printf("You must draw a raster map before using this option\n") ;
		do_pause() ;
		return ;
	}
	sprintf(buff, "'%s@%s'", mapname, mapset) ;
	gorun("d.colors", buff) ;
}
