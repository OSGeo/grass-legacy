#include "gis.h"

get_win(name) char *name ;
{
    char *mapset ;
    struct Cell_head win ;

    mapset = G_find_file("cell", name, "") ;
    if (!mapset)
    {
        printf ("Cell file [%s] not found\n", name) ;
        return -1 ;
    }

    /* read the window file into the win data structure */
    G_get_cellhd(name, mapset, &win) ;

    /* write the win data structure to standard output */
    printf("Window for cell file [%s in %s]\n", name, mapset) ;
    write_window(&win) ;

    /* everything went okay if it got this far */
    return 1 ;
}
