/* %W% %G% */

#include <stdio.h>
#include "segment.h"

segment_pageout (SEG,i)
    SEGMENT *SEG;
{
    segment_seek (SEG, SEG->scb[i].n, 0);
    if (write (SEG->fd, SEG->scb[i].buf, SEG->size) != SEG->size)
    {
	fprintf (stderr, "can't write segment file\n");
	return -1;
    }
    SEG->scb[i].dirty = 0;
    return 1;
}
