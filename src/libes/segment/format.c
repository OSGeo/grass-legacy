#include <stdio.h>

/* fd must be open for write */
segment_format (fd, nrows, ncols, srows, scols, len)
{
    return _segment_format (fd, nrows, ncols, srows, scols, len, 1);
}
segment_format_nofill (fd, nrows, ncols, srows, scols, len)
{
    return _segment_format (fd, nrows, ncols, srows, scols, len, 0);
}

static
_segment_format (fd, nrows, ncols, srows, scols, len, fill)
{
    long nbytes ;
    int spr, size;

    if (nrows <= 0 || ncols <= 0 || len <= 0 || srows <= 0 || scols <= 0)
    {
	fprintf (stderr,
	    "segement_format(fd,%d,%d,%d,%d,%d): illegal value(s)\n",
	    nrows, ncols, srows, scols, len);
	return -3;
    }

    if (lseek (fd, 0L, 0) < 0)
    {
	fprintf (stderr, "seek error on segment file\n");
	return -1;
    }

    if (!write_int (fd, nrows)
    ||  !write_int (fd, ncols)
    ||  !write_int (fd, srows)
    ||  !write_int (fd, scols)
    ||  !write_int (fd, len))
    {
	fprintf (stderr, "error writing segment file\n");
	return -1;
    }

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
    {
	fprintf (stderr, "error writing segment file\n");
	return -1;
    }

    return 1;
}

static
write_int (fd, n)
{
    int x;

    x = n;

    return write (fd, &x, sizeof(int)) == sizeof(int) ;
}

static
zero_fill(fd,nbytes)
    long nbytes;
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
	if(write (fd, buf, n) != n)
	    return -1;
	nbytes -= n;
    }
    return 1;
}
