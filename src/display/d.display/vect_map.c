#include "gis.h"
#include "windows.h"

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
		printf("\nEnter number of color desired:\n") ;
		printf(" 1 - red        7 - brown\n") ;
		printf(" 2 - orange     8 - gray\n") ;
		printf(" 3 - yellow     9 - white\n") ;
		printf(" 4 - green     10 - black\n") ;
		printf(" 5 - blue\n") ;
		printf(" 6 - violet\n") ;
		printf(" > ") ;
		if(gets(buff) == NULL) return ;
		if(sscanf(buff, "%d", &option) != 1)
			continue ;
		printf("%d\n", option) ;
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
}
