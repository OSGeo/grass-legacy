/* only works under 3.0 */
#include "variables.h"
#include "gis.h"

cell_map()
{
	int neg ;
	struct Colors colors;
	struct Range range;

	mapset = G_ask_cell_old("", mapname)  ;
	if (mapset == NULL)
		return ;

	neg = 0;
	if ((G_read_range (mapname, mapset, &range) > 0) && range.nmin)
	{
		negative_msg(mapname);
		return;
	}
	if (G_read_colors (mapname, mapset, &colors) < 0)
	{
		G_make_colors (mapname, mapset, &colors);
		G_write_colors (mapname, mapset, &colors);
	}
	neg = colors.min < 0;
	G_free_colors (&colors);

	/* Check to see if there are negative values in this map */
	if (neg)
	{
		negative_msg(mapname);
		return;
	}

	show_cell() ;
	show_legend() ;
}

static
negative_msg(mapname)
	char *mapname;
{
	char buf[256];
	printf("Map %s cannot be displayed because it contains\n", mapname) ;
	printf("   negative category values. ... Sorry.\n") ;
	printf("   Please hit the RETURN key to continue: \7") ;
	G_gets(buf) ;
}
