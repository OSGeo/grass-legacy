#include "tape.h"

put_row (fd, buf)
    unsigned char *buf;
{
    CELL *c;
    int nncols;

    nncols = ncols;
    c = cellbuf;
    while(nncols-- > 0)
	*c++ = *buf++;
    G_put_map_row (fd, cellbuf);
}
