/* %W% %G% */

#include <stdio.h>
#include "segment.h"

#define FREESLOT_SEGNUM (-1)

segment_put_row (SEG, buf, row)
    SEGMENT *SEG;
    char *buf;
{
    int segsize,spillsize;
    int ncols;
    int scols;
    int n, index, col;
    char *byteptr;

    ncols = SEG->ncols - SEG->spill ;
    scols = SEG->scols ;
    segsize = scols * SEG->len;

    for (col = 0; col < ncols; col += scols)
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0)
	    return -1;
	if(write (SEG->fd, buf, segsize) != segsize)
	{

/* JNK 07/17/91 -- changes to make errors more informative and to always output
                   full-length rows to the segment file (it only matters on the
                   very end of the segment file, but this helps make sure not to
                   leave unwritten spaces in a file to be random-accessed on the
                   off-chance that some systems will get confused by holes.  (To
                   completely ensure no holes, enough rows must be put to the
                   file to completely fill out the last "row" of segments.     */
	    fprintf (stderr, "put_row -- can't write segment file(1)\n");
	    return -1;
	}
	buf += segsize;
    }
    if (spillsize = SEG->spill * SEG->len)
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0)
	    return -1;
	if(write (SEG->fd, buf, spillsize) != spillsize)
	{
/* JNK 07/17/91 -- another minor change */
	    fprintf (stderr, "put_row -- can't write segment file(2)\n");
	    return -1;
	}

/* JNK 07/17/91 -- this fills out the row using a free segment slot buffer */
        buf=byteptr=SEG->scb[index=segment_inmem(SEG,FREESLOT_SEGNUM)].buf;
        for(col=spillsize;col<segsize;++col) *(byteptr++)='\0';
        if(write (SEG->fd, SEG->scb[index].buf, n=byteptr-buf) != n)
        {
/* JNK 07/17/91 -- another minor change */
            fprintf (stderr, "put_row -- can't write segment file(3)\n");
/* JNK 07/17/91 -- end of changes */

            return -1;
        }
    }
    return 1;
}
