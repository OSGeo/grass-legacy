#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include "segment.h"

static int _segment_format (int,int,int,int,int,int,int);
static int write_int(int,int);
static int zero_fill(int, long);

/* fd must be open for write */
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
	    "segement_format(fd,%d,%d,%d,%d,%d): illegal value(s)\n",
	    nrows, ncols, srows, scols, len);
	return -3;
    }

    if (lseek (fd, 0L, 0) == (off_t)-1)
    {
	G_warning ("Segment_format: %s\n",strerror(errno));
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
        G_warning("%s\n",strerror(errno));
    return bytes_wrote;
}

static int zero_fill(int fd, long nbytes)
{
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
            G_warning("%s\n",strerror(errno));
	    return -1;
        }
	nbytes -= n;
    }
    return 1;
}
