#include "tape.h"

int put_row (int fd, unsigned char *buf)
{
    CELL *c;
    int nncols;

    nncols = ncols;
    c = cellbuf;
    while(nncols-- > 0)
	*c++ = *buf++;
    G_put_c_raster_row (fd, cellbuf);

    return 0;
}
