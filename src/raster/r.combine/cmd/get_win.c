#include "gis.h"
#include "local_proto.h"

int get_win (char *name)
{
    char *mapset ;
    struct Cell_head win ;

    mapset = G_find_cell(name, "") ;
    if (!mapset)
    {
        fprintf (stdout,"Raster file [%s] not found\n", name) ;
        return -1 ;
    }

    /* read the window file into the win data structure */
    G_get_cellhd(name, mapset, &win) ;

    /* write the win data structure to standard output */
    fprintf (stdout,"Header for raster file [%s in %s]\n", name, mapset) ;
    write_window(&win) ;

    /* everything went okay if it got this far */
    return 1 ;
}
