#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "segment.h"

int segment_get_row (SEGMENT *SEG, CELL *buf,int row)
{
    int size;
    int ncols;
    int scols;
    int n, index, col;

    ncols = SEG->ncols - SEG->spill ;
    scols = SEG->scols ;
    size = scols * SEG->len;

    for (col = 0; col < ncols; col += scols)
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0)
	    return -1;
	if(read (SEG->fd, buf, size) != size)
	{
	    G_warning ("segment_get_row: %s\n",strerror(errno));
	    return -1;
	}
	buf += size;
    }
    if ((size = SEG->spill * SEG->len))
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0)
	    return -1;
	if(read (SEG->fd, buf, size) != size)
	{
	    G_warning ("segment_get_row: %s\n",strerror(errno));
	    return -1;
	}
    }

    return 1;
}
