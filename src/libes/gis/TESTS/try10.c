#include "gis.h"
#include "edit.h"

main()
{
    char name[30], *mapset;
    struct Cell_head cellhd;

    G_gisinit("try");
    while (mapset = G_ask_cell_in_mapset ("", name))
    {
	if (G_get_cellhd (name, mapset, &cellhd) < 0)
	{
	    fprintf (stdout,"OOPS\n");
	    continue;
	}
	if(E_edit_cellhd (&cellhd, 1) >= 0)
	{
	    fprintf (stdout,"ok\n");
	}
    }
}
