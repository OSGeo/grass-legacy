#include "variables.h"
#include "gis.h"

cell_map()
{
	struct Colors colors;

	mapset = G_ask_cell_old("", mapname)  ;
	if (mapset == NULL)
		return ;

	if (G_read_colors (mapname, mapset, &colors) < 0)
	{
		G_make_colors (mapname, mapset, &colors);
		G_write_colors (mapname, mapset, &colors);
	}
	G_free_colors (&colors);

	show_cell() ;
	show_legend() ;
}
