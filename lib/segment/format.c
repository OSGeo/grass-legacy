#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include "segment.h"
#include "config.h"

static int _segment_format (int,int,int,int,int,int,int);
static int write_int(int,int);
static int zero_fill(int, long);

/* fd must be open for write */

/*!
 * \brief format a segment file
 *
 * The segmentation routines require a disk file
 * to be used for paging segments in and out of memory. This routine formats the
 * file open for write on file descriptor <b>fd</b> for use as a segment file.
 * A segment file must be formatted before it can be processed by other segment
 * routines. The configuration parameters <b>nrows, ncols, srows, scols</b>,
 * and <b>len</b> are written to the beginning of the segment file which is
 * then filled with zeros.
 * The corresponding nonsegmented data matrix, which is to be transferred to the
 * segment file, is <b>nrows</b> by <b>ncols.</b> The segment file is to be
 * formed of segments which are <b>srows</b> by <b>scols.</b> The data items
 * have length <b>len</b> bytes. For example, if the <i>data type is int</i>,
 * \textbf{<i>len</i> }<i>is sizeof(int).</i>
 * Return codes are: 1 ok; else -1 could not seek or write <i>fd</i>, or -3
 * illegal configuration parameter(s).
 *
 *  \param fd
 *  \param nrows
 *  \param ncols
 *  \param srows
 *  \param scols
 *  \param len
 *  \return int
 */

int segment_format (int fd,int nrows,int ncols,int srows,int scols,int len)
{
    return _segment_format (fd, nrows, ncols, srows, scols, len, 1);
}

int segment_format_nofill (
	int fd,int nrows,int ncols,int srows,int scols,int len)
{
    return _segment_format (fd, nrows, ncols, srows, scols, len, 0);
}

static int _segment_format(
	int fd,
	int nrows,int ncols,
	int srows,int scols,
	int len,int fill)
{
    long nbytes ;
    int spr, size;

    if (nrows <= 0 || ncols <= 0 || len <= 0 || srows <= 0 || scols <= 0)
    {
	G_warning (
	    "segment_format(fd,%d,%d,%d,%d,%d): illegal value(s)",
	    nrows, ncols, srows, scols, len);
	return -3;
    }

    if (lseek (fd, 0L, 0) == (off_t)-1)
    {
	G_warning ("Segment_format: %s",strerror(errno));
	return -1;
    }

    if (!write_int (fd, nrows) ||  !write_int (fd, ncols)
    ||  !write_int (fd, srows) ||  !write_int (fd, scols)
    ||  !write_int (fd, len)) return -1;

    if (!fill) return 1;

    spr = ncols / scols ;
    if(ncols % scols)
	spr++ ;

    size = srows * scols * len;

/* calculate total number of segments */
    nbytes = spr * ((nrows + srows - 1)/ srows);
    nbytes *= size ;

/* fill segment file with zeros */
/* NOTE: this could be done faster using lseek() by seeking
 * ahead nbytes and then writing a single byte of 0,
 * provided lseek() on all version of UNIX will create a file
 * with holes that read as zeros.
 */
    if(zero_fill (fd, nbytes) < 0)
	return -1;

    return 1;
}

static int write_int (int fd,int n)
{
    int x;
    int bytes_wrote;

    x = n;

    if((bytes_wrote = write (fd, &x, sizeof(int)) == sizeof(int) ) < 0)
        G_warning("%s",strerror(errno));
    return bytes_wrote;
}

static int zero_fill(int fd, long nbytes)
{
#ifndef HAVE_LSEEK
    char buf[10240];
    register char *b;
    register int n;

/* zero buf */
    n = nbytes > sizeof(buf) ? sizeof(buf) : nbytes ;
    b = buf;
    while (n-- > 0)
	*b++ = 0;

    while (nbytes > 0)
    {
	n = nbytes > sizeof(buf) ? sizeof(buf) : nbytes ;
	if(write (fd, buf, n) != n) {
            G_warning("%s",strerror(errno));
	    return -1;
        }
	nbytes -= n;
    }
    return 1;
#else
  /* Using lseek (faster upon initialization).
     NOTE: This version doesn't allocate disk storage for the file; storage will
     be allocated dynamically as blocks are actually written. This could 
     result in zero_fill() succeeding but a subsequent call to write() failing
     with ENOSPC ("No space left on device").
   */

    char buf[10];

    buf[0]=0x0;
    G_debug(3,"Using new segmentation code...");
    if ( lseek(fd,nbytes-1,SEEK_SET) < 0 ) { 
            G_warning("%s",strerror(errno));
	    return -1;
    }
    if(write (fd,buf, 1) != 1) {
            G_warning("%s",strerror(errno));
	    return -1;
    }
    return 1;
#endif
}
