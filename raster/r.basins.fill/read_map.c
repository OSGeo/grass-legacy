#include <grass/gis.h>
#include "local_proto.h"


CELL *read_map (char *name, char *mapset, int nomask, int nrows, int ncols)
{
    int fd;
    CELL *map;
    int row;
    int (*get_row)();

    /* allocate entire map */
    map = (CELL *)G_malloc (nrows*ncols*sizeof(CELL));

    /* open the map */
    if ((fd = G_open_cell_old (name, mapset)) < 0)
	G_fatal_error("unable to open [%s] in [%s]", name, mapset);

    /* read the map */
    G_message("READING [%s] in [%s] ... ", name, mapset);

    if (nomask)
	get_row = G_get_map_row_nomask ;
    else
	get_row = G_get_map_row ;

    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 10);
	if ((*get_row)(fd, map+row*ncols, row) < 0)
	    G_fatal_error("error reading [%s] in [%s]", name, mapset);
    }
    G_percent (nrows, nrows, 10);

    G_close_cell (fd);

    return map;
}
