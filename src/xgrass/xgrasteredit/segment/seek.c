/* %W% %G% */

#include <stdio.h>
#include "segment.h"

segment_seek (SEG, n, index)
    SEGMENT *SEG;
{
    register SEEK_OFFSET offset;

    offset = n * SEG->size + index + SEG->offset ;
    if (lseek (SEG->fd, offset, 0) < 0)
    {
	fprintf (stderr, "can't seek segment file\n");
	return -1;
    }
    return 1;
}
