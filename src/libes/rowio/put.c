#include <stdio.h>
#include "rowio.h"

int rowio_put ( ROWIO *R, char *buf,int row)
{
    int i;
    int col;
    char *b;

    if (row < 0)
	return 0;

    for (i = 0; i < R->nrows; i++)
	if (row == R->rcb[i].row)
	{
	    b = R->rcb[i].buf;
	    for (col = 0; col < R->len; col++)
		*b++ = *buf++;
	    R->rcb[i].dirty = 1;
	    return 1;
	}
    return ((*R->putrow) (R->fd, buf, row, R->len));
}
