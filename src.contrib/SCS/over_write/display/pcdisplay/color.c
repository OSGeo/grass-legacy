#include "variables.h"
#include "windows.h"
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
	R_open_driver();
	Dchoose(LEG.name);
	R_close_driver();
	gorun("d.colors", buff) ;
	show_legend();
}
