#include <stdio.h>
#include "lproto.h"
#include "D.h"
#include "display.h"
#include "raster.h"
#include "gis.h"
#include "windows.h"

int vect_map()
{
	char *vect_mapset ;
	char vect_name[128] ;
	char command[256] ;
	char buff[128] ;
	char *color ;
	int option ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	Dchoose(MAP.name) ;
	R_close_driver();

	vect_mapset = G_ask_old("", vect_name, "dig", "vect")  ;
	if (vect_mapset == NULL)
		return -1;
	
	for(;;)
	{
		fprintf (stdout,"\nEnter number of color desired:\n") ;
		fprintf (stdout," 1 - red        7 - brown\n") ;
		fprintf (stdout," 2 - orange     8 - gray\n") ;
		fprintf (stdout," 3 - yellow     9 - white\n") ;
		fprintf (stdout," 4 - green     10 - black\n") ;
		fprintf (stdout," 5 - blue\n") ;
		fprintf (stdout," 6 - violet\n") ;
		fprintf (stdout," > ") ;
		if(fgets(buff,128,stdin) == NULL) return -1;
		if(sscanf(buff, "%d", &option) != 1)
			continue ;
		fprintf (stdout,"%d\n", option) ;
		if(option < 1 || option > 10)
			continue ;
		switch(option)
		{
		case 1: color = "red";    break ;
		case 2: color = "orange"; break ;
		case 3: color = "yellow"; break ;
		case 4: color = "green";  break ;
		case 5: color = "blue";   break ;
		case 6: color = "violet"; break ;
		case 7: color = "brown";  break ;
		case 8: color = "gray";   break ;
		case 9: color = "white";  break ;
		case 10: color = "black"; break ;
		default: color = "black"; break ;
		}
		break ;
	}
	sprintf(command,"'%s@%s' color=%s", vect_name, vect_mapset, color) ;
	gorun("d.vect", command) ;

	return 0;
}
