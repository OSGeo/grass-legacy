#include "tape.h"

int put_row (int fd, unsigned char *buf, int row)
{
    CELL *c;
    int ncols;

    ncols = G_window_cols();
    c = tape.cellbuf;
    while(ncols-- > 0)
	*c++ = *buf++;
/* function call changed */
    G_put_raster_row (fd, tape.cellbuf, CELL_TYPE);

    return 0;
}
