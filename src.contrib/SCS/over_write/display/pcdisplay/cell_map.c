#include "variables.h"
#include "gis.h"
#define MAXCOLR 216

cell_map()
{
	FILE *fptr ;
	FILE *popen() ;
	char buf[256] ;
	int i ;

	struct Colors colors;
	mapset = G_ask_cell_old("", mapname)  ;
	if (mapset == NULL)
		return ;
	if (G_read_colors (mapname, mapset, &colors) < 0)
	{
		G_make_colors (mapname, mapset, &colors);
		G_write_colors (mapname, mapset, &colors);
	}

/*-------------------------------- 4/13/89, RLG -------------
/** This is added to avoid the display lock_up caused when you
*** display more than 216 colors with color mode set to float.
*** if number of colors > 216 give the message
***								 **/

	G_read_colors(mapname,mapset,&colors);
	if ((colors.cmax-colors.cmin+1) > MAXCOLR) {
		G_warning("Cell file has more than 216 colors\n If you need to display this map\n you will need to run Grescale.\n");
	        sleep (2) ;
                return ;
		}

	/* Check to see if there are negative values in this map */
	printf("Checking for negative categories in %s\n\n", mapname) ;
	sprintf(buf, "Gdescribe \"%s in %s\" 2> /dev/null", mapname, mapset) ;
	if (NULL == (fptr = popen(buf, "r")))
	{
		printf("Sorry, couldn't run Gdescribe to check for negative\n") ;
		printf("   values in map %s\n.   Assuming all falues are positive.\n", mapname) ;
		sleep(2) ;
	}
	else
	{
		fscanf(fptr, "%d", &i) ;
		pclose(fptr) ;
		if (i < 0)
		{
			printf("Map %s cannot be displayed because it contains\n", mapname) ;
			printf("   negative category values. ... Sorry.\n") ;
			printf("   Please hit the RETURN key to continue: ") ;
			gets(buf) ;
			return ;
		}
	}
		
	G_free_colors (&colors);
	show_cell() ;
	show_legend() ;
}
