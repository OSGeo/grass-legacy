#include "gis.h"
#include "windows.h"
#include "variables.h"

vect_map()
{
	char *vect_mapset ;
	char vect_name[128] ;
	char command[256] ;
	char buff[128] ;
	char *color ;
	int option ;

	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	vect_mapset = G_ask_old("", vect_name, "dig", "vect")  ;
	if (vect_mapset == NULL)
		return ;
	
	for(;;)
	{
		fprintf(stderr,"\nEnter number of color desired:\n") ;
		fprintf(stderr," 1 - red        7 - brown\n") ;
		fprintf(stderr," 2 - orange     8 - gray\n") ;
		fprintf(stderr," 3 - yellow     9 - white\n") ;
		fprintf(stderr," 4 - green     10 - black\n") ;
		fprintf(stderr," 5 - blue\n") ;
		fprintf(stderr," 6 - violet\n") ;
		fprintf(stderr," > ") ;
		if(gets(buff) == NULL) return ;
		if(sscanf(buff, "%d", &option) != 1)
			continue ;
		fprintf(stderr,"%d\n", option) ;
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
	sprintf(command,"'%s in %s' %s", vect_name, vect_mapset, color) ;
	maptyp = 2 ;
	gorun("Dvect", command) ;
}
