/* %W% %G% */

#include <stdio.h>
#include "segment.h"

/* fd must be open for read and write */
segment_init (SEG, fd, nseg)
    SEGMENT *SEG;
{
    SEG->open = 0;
    SEG->fd   = fd;
    SEG->nseg = nseg;

    if (lseek (fd, 0L, 0) < 0)
    {
	fprintf (stderr, "seek error on segment file\n");
	return -1;
    }

/* read the header */
    if (!read_int (fd, &SEG->nrows)
    ||  !read_int (fd, &SEG->ncols)
    ||  !read_int (fd, &SEG->srows)
    ||  !read_int (fd, &SEG->scols)
    ||  !read_int (fd, &SEG->len))
    {
	fprintf (stderr, "error reading segment file\n");
	return -1;
    }

    return segment_setup (SEG);
}

static
read_int (fd, n)
    int *n;
{
    return read (fd, n, sizeof(int)) == sizeof(int);
}
