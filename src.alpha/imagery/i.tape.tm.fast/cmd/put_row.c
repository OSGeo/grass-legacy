#include "tape.h"

put_row (fd, buf)
    unsigned char *buf;
{
    CELL *c;
    int ncols;

    ncols = G_window_cols();
    c = tape.cellbuf;
    while(ncols-- > 0)
	*c++ = *buf++;
    G_put_map_row (fd, tape.cellbuf);
}
