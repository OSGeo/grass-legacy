#include "gis.h"
#include "edit.h"

main()
{
    char name[30], *mapset;
    char *G_get_cat();
    struct Categories cats;

    G_gisinit("try");
    while (mapset = G_ask_cell_in_mapset ("", name))
    {
	if (G_read_cats (name, mapset, &cats) < 0)
	{
	    fprintf (stdout,"OOPS\n");
	    continue;
	}
	if(E_edit_cats (&cats) > 0)
	{
	    fprintf (stdout,"updating cats for [%s]\n", name);
	    G_write_cats (name, &cats);
	}
    }
}
