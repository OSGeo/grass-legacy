#include "tape.h"

int put_row (int fd, unsigned char *buf)
{
    CELL *c;
    int ncols;

    ncols = G_window_cols();
    c = tape.cellbuf;
    while(ncols-- > 0)
	*c++ = *buf++;
    G_put_c_raster_row (fd, tape.cellbuf);

    return 0;
}
