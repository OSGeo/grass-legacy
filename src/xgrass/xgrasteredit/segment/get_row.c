/* %W% %G% */

#include <stdio.h>
#include "segment.h"

segment_get_row (SEG, buf, row)
    SEGMENT *SEG;
    char *buf;
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
	    fprintf (stderr, "can't read segment file\n");
	    return -1;
	}
	buf += size;
    }
    if (size = SEG->spill * SEG->len)
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0)
	    return -1;
	if(read (SEG->fd, buf, size) != size)
	{
	    fprintf (stderr, "can't read segment file\n");
	    return -1;
	}
    }

    return 1;
}
