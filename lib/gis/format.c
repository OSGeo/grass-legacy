#include "gis.h"
#include "glocale.h"
#include <unistd.h>
#include <stdlib.h>

#include "config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "G.h"

#define FCB G__.fileinfo[fd]
#define NROWS  FCB.cellhd.rows

/*!

 <h3>GRASS Raster Format</h3>
 
 Small example to illustrate the raster format:

 A file may contain the following 3x3 floating point matrix:
 \verbatim
   10.000 20.000 30.000
   20.000 40.000 50.000
   30.000 50.000 60.000
 \endverbatim
 
 The header is a single byte, equal to sizeof(long) (typically 4 on a
 32-bit platform, 8 on a 64-bit platform). Then, NROWS+1 offsets are
 written as longs (i.e. 4 or 8 bytes, depending upon platform) in
 big-endian (Motorola) byte order.
 <P>
 Thus, above example is actually interpreted as:
 \verbatim
   4               sizeof(long)
   0 0 0 17        offset of row 0
   0 0 0 36        offset of row 1
   0 0 0 55        offset of row 2
   0 0 0 74        offset of end of data
 \endverbatim

 See G__write_row_ptrs() below for the code which writes this data. 
 However, note that the row offsets are initially zero; 
 they get overwritten later (if you are writing compressed data,
 you don't know how much space it will require until you've compressed
 it).

 As for the format of the actual row data, see put_fp_data() in
 src/libes/gis/put_row.c and RFC 1014 (the XDR specification):
 http://www.faqs.org/rfcs/rfc1014.html
 
*/

/**********************************************************************
 *
 *   G__check_format(fd)
 *      int fd
 *
 *   Check to see if map with file descriptor "fd" is in compressed
 *   format.   If it is, the offset table at the beginning of the 
 *   file (which gives seek addresses into the file where code for
 *   each row is found) is read into the File Control Buffer (FCB).
 *   The compressed flag in the FCB is appropriately set.
 *
 *   returns:    Nothing
 **********************************************************************/

int G__check_format ( int fd )
{
    unsigned char compress[10] ;

/*
 * Check to see if the file is in compress mode
 * 3 possibilites
 *   compressed flag in cellhd is negative (meaning pre 3.0 cell file)
 *       compression flag is first 3 bytes of cell file
 *   compression flag is 0 - not compressed
 *   compression flag is 1 - compressed
 */

    if (FCB.cellhd.compressed < 0)
    {
	if (read(fd,compress,3) != 3
	|| compress[0] != 251
	|| compress[1] != 255
	|| compress[2] != 251)
	    FCB.cellhd.compressed = 0 ;
    }
    if (!FCB.cellhd.compressed)
	return fd;

/* allocate space to hold the row address array */
    FCB.row_ptr = (long *) G_calloc(NROWS + 1, sizeof(long)) ;

/* read the row address array */
    return G__read_row_ptrs (fd);
}

int G__read_row_ptrs (int fd)
{
    int n ;
    int row;
    unsigned char *buf;
    unsigned char *b;
    unsigned char nbytes;
    long v;

/*
 * pre3.0 row addresses were written directly from the array of longs
 * (this makes them machine dependent)
 */

    if (FCB.cellhd.compressed < 0)
    {
	n = (NROWS + 1) * sizeof(long) ;
	if (read(fd, (char *)FCB.row_ptr, n ) != n)
	    goto badread;
	return 1;
    }

/*
 * 3.0 row address array is in a machine independent format
 * (warning - the format will work even if the sizeof(long) is
 *  not the same from machine to machine, as long as the
 *  actual values do not exceed the capability of the long)
 */
    if (read (fd, &nbytes, 1) != 1)
	goto badread;
    if (nbytes == 0)
	goto badread;
    n = (NROWS + 1) * nbytes ;
    buf = (unsigned char *) G_malloc (n);
    if (read (fd, b=buf, n) != n)
	goto badread;
    for (row = 0; row <= NROWS; row++)
    {
	v = 0;
	for (n = 0; n < (int) nbytes; n++)
	    v = (v << 8) + *b++;   /* v = (v * 256) + *b++; */
	FCB.row_ptr[row] = v;
    }
    free(buf);
    return 1;

badread:
    G_warning ( _("Fail of initial read of compressed file [%s in %s]"),
	FCB.name, FCB.mapset) ;
    return -1;
}

int G__write_row_ptrs (int fd)
{
    int i,len;
    int row;
    long x, y;
    unsigned char *buf, *b;

    lseek (fd, 0L, 0);
    len = (NROWS+1) * sizeof(long) + 1;
    b = buf = (unsigned char *) G_malloc (len);
    *b++ = sizeof(long);

    for (row = 0; row <= NROWS; row++)
    {
	x = FCB.row_ptr[row];
	i = sizeof(long);
	while (i-- > 0)
	{
	    y = x >> 8;           /* y = x / 256; */
	    b[i] = x - (y << 8);  /* b[i] = x - (y * 256); */
	    x = y;
	}
	b += sizeof(long);
    }

    i = (write (fd, buf, len) == len);
    free(buf);

    return i;
}
