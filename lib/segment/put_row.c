#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include "segment.h"
#include "gis.h"

/*	buf is CELL *   WRAT code	*/
int segment_put_row (SEGMENT *SEG, CELL *buf,int row)
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
	if(segment_seek (SEG, n, index) < 0) {
	    G_warning (
	        "Failed seek in segment file for index = %d n = %d at col:row %d:%d\n",
	        index,n,col,row);
	    return -1;
	}
	if(write (SEG->fd, buf, size) != size)
	{
	    G_warning ("segment_put_row write error %s\n",strerror(errno));
	    return -1;
	}
	buf += size;
    }
    if ((size = SEG->spill * SEG->len))
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0) {
	    G_warning (
	        "Failed seek in segment file for index = %d n = %d at col:row %d:%d\n",
	        index,n,col,row);
	    return -1;
	}
	if(write (SEG->fd, buf, size) != size)
	{
	    G_warning ("segment_put_row final write error: %s\n",strerror(errno));
	    return -1;
	}
    }
    return 1;
}
