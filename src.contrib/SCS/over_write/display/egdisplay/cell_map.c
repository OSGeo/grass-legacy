#include "variables.h"
#include "windows.h"
#include "gis.h"

cell_map()
{
	struct Range range;
	char buff[256];

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
 	maptyp = 1 ;
	show_cell() ;
	R_open_driver();
	Dchoose(LEG.name);
	Derase(D_translate_color("black"));
	R_close_driver();
	if (-1 != G_read_range(mapname, mapset, &range))
	{	int cats_num;

		cats_num = range.pmax - range.nmin + 1;
		if (cats_num < 12 * 25) sprintf(buff,
			"'%s in %s' lines=25 cols=12", mapname, mapset);
		else sprintf(buff, "'%s in %s'", mapname, mapset); 
		gorun("Dcolortable", buff);
	}
}
