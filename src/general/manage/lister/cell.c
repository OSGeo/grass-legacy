#include "gis.h"

main(argc,argv) char *argv[];
{
    int lister();
    G_gisinit (argv[0]);

    G_list_element ("cell", "raster", argc > 1 ? argv[1] : "", lister);
    exit(0);
}

lister (name, mapset, title)
    char *name, *mapset, *title;
{
    *title = 0;
    if (*name)
	strcpy (title, G_get_cell_title (name, mapset));
}
