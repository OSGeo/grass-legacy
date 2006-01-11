#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "config.h"
#include "gis.h"
#include "segment.h"

int segment_seek (SEGMENT *SEG,int n,int index)
{
    register SEEK_OFFSET offset;

    offset = n * SEG->size + index + SEG->offset ;
    if (lseek (SEG->fd, offset, 0) == (off_t)-1)
    {
	G_warning ("segment_seek: %s\n",strerror(errno));
	return -1;
    }
    return 0;
}
