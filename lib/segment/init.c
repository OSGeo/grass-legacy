#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "segment.h"

static int read_int(int,int *);

/* fd must be open for read and write */

/*!
 * \brief initialize segment
 *       structure
 *
 * Initializes the <b>seg</b> structure. The file on <b>fd</b> is
 * a segment file created by <i>segment_format</i> and must be open for
 * reading and writing. The segment file configuration parameters <i>nrows,
 * ncols, srows, scols</i>, and <i>len</i>, as written to the file by
 * <i>segment_format</i>, are read from the file and stored in the
 * <b>seg</b> structure. <b>Nsegs</b> specifies the number of segments that
 * will be retained in memory. The minimum value allowed is 1.
 * <b>Note.</b> The size of a segment is <i>scols*srows*len</i> plus a few
 * bytes for managing each segment.
 * Return codes are:  1 if ok; else -1 could not seek or read segment file,  or -2 out of memory.
 *
 *  \param seg
 *  \param fd
 *  \param nsegs
 *  \return int
 */

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
