#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "segment.h"

static int read_int(int,int *);

/* fd must be open for read and write */
int segment_init (SEGMENT *SEG,int fd,int nseg)
{
    SEG->open = 0;
    SEG->fd   = fd;
    SEG->nseg = nseg;

    if (lseek (fd, 0L, 0) < 0)
    {
	G_warning ("segment_init: %s\n",strerror(errno));
	return -1;
    }

/* read the header */
    if (!read_int (fd, &SEG->nrows)
    ||  !read_int (fd, &SEG->ncols)
    ||  !read_int (fd, &SEG->srows)
    ||  !read_int (fd, &SEG->scols)
    ||  !read_int (fd, &SEG->len))
	return -1;

    return segment_setup (SEG);
}

static int read_int (int fd, int *n)
{
    int bytes_read;

    if((bytes_read = read (fd, n, sizeof(int))) == -1)
        G_warning("read_int: %s\n",strerror(errno));
    bytes_read = (bytes_read == sizeof(int));

    return bytes_read;
}
