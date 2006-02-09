#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <grass/segment.h>

int segment_pageout( SEGMENT *SEG,int i)
{
    segment_seek (SEG, SEG->scb[i].n, 0);
    if (write (SEG->fd, SEG->scb[i].buf, SEG->size) != SEG->size)
    {
	G_warning ("segment_pageout: %s\n",strerror(errno));
	return -1;
    }
    SEG->scb[i].dirty = 0;
    return 1;
}
