
#include "tape.h"

int 
put_row (int fd, unsigned char *buf, int row)
{
    CELL *c;
    int ncols;

    ncols = G_window_cols();
    c = cellbuf;
    while(ncols-- > 0)
	*c++ = *buf++;
/* function call changed */
/*
    G_put_map_row (fd, cellbuf, row);
*/
    G_put_c_raster_row (fd, cellbuf);

    return 0;
}
