#include "variables.h"
#include <stdio.h>

color()
{
	char buff[128] ;
	if (*mapname == NULL)
	{
		printf("You must draw a cell map before using this option\n") ;
		do_pause() ;
		return ;
	}
	sprintf(buff, "'%s in %s'", mapname, mapset) ;
	gorun("d.colors", buff) ;
}
