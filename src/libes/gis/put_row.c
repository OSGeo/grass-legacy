/**********************************************************************
 *
 *   G_put_map_row (fd, buf)
 *      int fd           file descriptor of the opened map
 *      CELL *buf        buffer holding row info to be written
 *
 *   Writes the next row for the cell file opened on 'fd' from 'buf'
 *   All writes go into NEW files that exactly match the current window.
 *   The file must have been opened with G_open_cell_new()
 *   and be written sequentially, ie no skipping rows
 *
 *   *** NOTE *** 
 *   A map cannot be copied using G_get_map_row() and G_put_map_row().
 *   The former resamples the data of the original map into a row buffer
 *   that matches the current window.  The later writes out rows associated
 *   with the window.
 *
 *   returns:    1  if successful
 *              -1  on fail
 *
 *  Keeps track of the largest category number for use in updating
 *  the cats (categories) file upon close of the cell file.
 ********************************************************************** 
 *
 *  G_put_map_row_random (fd, buf, row, col, ncells)
 *      int fd             File descriptor where data is to be written
 *      CELL *buf          Buffer holding data
 *      int row            Map row where data is to be written
 *      int col            Column where data begins
 *      int ncells         Number of columns of data to be written
 *
 *   Writes parts of rows into open cell file.
 *
 *   Cell file must have been opened with G_open_cell_new_random()
 *
 *   returns:    0  if successful
 *              -1  on fail
 *
 *  Keeps track of the largest category number for use in updating
 *  the cats (categories) file upon close of the cell file.
 **********************************************************************/

#include "G.h"

#define FCB G__.fileinfo[fd]

static char *me;

G_put_map_row (fd, buf)
    CELL *buf;
{
    me = "G_put_map_row";
    if (!check_open (fd,0))
        return -1;

    switch(put_data (fd, buf, FCB.cur_row, 0, FCB.cellhd.cols))
    {
        case -1: return -1;
        case  0: return  1;
    }

    if (FCB.want_histogram)
	G_update_cell_stats (buf, FCB.cellhd.cols, &FCB.statf);

    G_row_update_range (buf, FCB.cellhd.cols, &FCB.range);

    FCB.cur_row++;
    return 1;
}

G_put_map_row_random (fd, buf, row, col, n)
    CELL *buf;
{
    me = "G_put_map_row_random";
    if (!check_open (fd,1))
        return -1;

    buf += adjust (fd, &col, &n);
    switch(put_data (fd, buf, row, col, n))
    {
        case -1: return -1;
        case  0: return  1;
    }
    if (FCB.want_histogram)
	G_update_cell_stats (buf, n, &FCB.statf);

    G_row_update_range (buf, n, &FCB.range);
    return 1;
}

/***************************************************************
* put_data (fd, cell, row, col, n)
*
* writes data to cell file for either full or partial rows
*
***************************************************************/

static
put_data (fd, cell, row, col, n)
    CELL *cell;
{
    int random;
    int compressed;
    long offset;
    int rle;
    int len;
    int count, nbytes, nwrite;
    int i,k;
    int neg;
    CELL r,v;
    unsigned char *wk, *wk2, *cp;
    unsigned char *cur;

#ifdef DEBUG
if (row == 0)
printf ("%s(mode=%d): row=%d, col=%d, n=%d (FCB.nbytes=%d)\n\r",
	me, FCB.open_mode, row,col,n,FCB.nbytes);
#endif

    random     = (FCB.open_mode == OPEN_NEW_RANDOM);
    compressed = (FCB.open_mode == OPEN_NEW_COMPRESSED);


/* row out of window? */
    if (row < 0 || row >= FCB.cellhd.rows)
        return 0;

    if (n <= 0)
        return 0;

/* for random writes, seek to the position to begin the write */
    if (random)
    {
	offset = (long) (FCB.cellhd.cols * row + col) * FCB.nbytes;
	if (lseek (fd, offset, 0) < 0)
	{
	    write_error (fd, row);
	    return -1;
	}
    }
    else if (compressed)
    {
	long *row_ptr;
	long lseek();

	row_ptr = FCB.row_ptr;
	row_ptr[row] = lseek (fd, 0L, 1);
    }

/*
 * transform CELL data into non-machine dependent multi-byte format
 */
    wk = G__.work_buf;
    if (compressed) wk++;
    nbytes = 1;
    len = compressed ? sizeof(CELL) : FCB.nbytes;

    for (i = 0; i < n; i++)
    {
	v = *cell++;

    /* negatives */
	if (v < 0)
	{
	    neg = 1;
	    v = -v;
	}
	else neg = 0;

    /* copy byte by byte */
	k = len;
	count = 0;
	while (k-- > 0)
	{
	    r = v >> 8;      /* r = v / 256; */
	    if (v)
		count++;     /* only needed for compressed */

	    wk[k] = v - (r << 8) ;   /* wk[k] = v - (r * 256) ; */

	    v = r;
	}
    /* set negative bit in first byte */
	if (neg)
	{
	    *wk |= 0200 ;
	    count = sizeof(CELL);
	}

    /*
     * determine number of bytes really needed by this cell
     * only needed for compressed
     */
	if (count > nbytes)
	    nbytes = count;

    /* move the buffer pointer */
	wk += len;
    }
#ifdef DEBUG
if (!row && compressed)
printf ("row %d - after cell size reduction: nbytes %d\n", row, nbytes);
#endif
    if (compressed && (nbytes > FCB.nbytes))
	FCB.nbytes = nbytes;

/*
 * for compressed writes, do row compression
 * first trim away zero high bytes
 * then run-length encode the data
 */
    if (compressed)
    {
/* trim */
	len = FCB.cellhd.cols * nbytes;
	if (nbytes < sizeof(CELL))
	{
	    wk = wk2 = G__.work_buf+1;
	    for (i = 0; i < n; i++)
	    {
		for (k = sizeof(CELL); k > nbytes; k--)
		    wk++;
		while (k-- > 0)
		    *wk2++ = *wk++;
	    }
	}

/* rle */
	cp = G__.compressed_buf;
	wk = G__.work_buf;
	*wk++ = *cp++ = nbytes;        /* record the byte count */

	cur = wk ;
	count = 0;
	nwrite = 0;
	rle = 1;

	for (i = 0; i < n; i++)
	{
	    if (count == 255 || !same (cur, wk, nbytes))
	    {
    /* if rle would be bigger, don't do rle */
		nwrite += nbytes + 1;
		if (nwrite >= len)
		{
		    rle = 0;
		    break;
		}

		*cp++ = count;
		G_copy (cp, cur, nbytes);
		cp += nbytes;

		count = 0;
		cur = wk;
	    }
	    count++ ;
	    wk += nbytes ;
	}
	if (rle && count)
	{
	    nwrite += nbytes + 1;
	    if (nwrite >= len)
		rle = 0;
	    else
	    {
		*cp++ = count ;
		G_copy (cp, cur, nbytes);
	    }
	}

	if (rle)
	{
	    nwrite++;
	    if (write (fd, G__.compressed_buf, nwrite) != nwrite)
	    {
		write_error (fd, row);
		return -1;
	    }
	}
	else
	{
	    nwrite = nbytes * n + 1;
	    if (write (fd, G__.work_buf, nwrite) != nwrite)
	    {
		write_error (fd, row);
		return -1;
	    }
	}
    }
    else
    {
	nwrite = FCB.nbytes * n;
#ifdef DEBUG
if (!row)
printf ("writing %d cells at %d bytes per cell\n", n, FCB.nbytes);
#endif
	if (write (fd, G__.work_buf, nwrite) != nwrite)
	{
	    write_error (fd, row);
	    return -1;
	}
    }
    return 1;
}


static
check_open (fd, random)
{
    char msg[100];
    switch (FCB.open_mode)
    {
    case OPEN_OLD:
	sprintf (msg,
	    "%s: layer [%s] not open for write - request ignored",
	    me, FCB.name);
	break;
    case OPEN_NEW_COMPRESSED:
    case OPEN_NEW_UNCOMPRESSED:
	if (!random) return 1;
	sprintf (msg,
	    "%s: layer [%s] not open for random write - request ignored",
	    me, FCB.name);
	break;
    case OPEN_NEW_RANDOM:
	if (random) return 1;
	sprintf (msg,
	    "%s: layer [%s] not open for sequential write - request ignored",
	    me, FCB.name);
	break;
    default:
	sprintf (msg, 
	    "%s: unopened file descriptor - request ignored",
	    me);
	break;
    }
    G_warning (msg);
    return 0;
}

/*******************************************************
*  adjust the column,n so that it is within the window
*  returns the adjustment to buffer that must be made
*  if col,n is adjusted
*
*  if n comes back <= zero, do not write
*******************************************************/
static
adjust (fd, col, n)
    int *col;
    int *n;
{
    int adj;
    int last;

    adj = 0;
    last = *col + *n;
    if (*col < 0)
    {
        adj = -(*col);
        *col = 0;
    }
    if (last > FCB.cellhd.cols)
        last = FCB.cellhd.cols;
    *n = last - *col;

    return adj;
}

static 
write_error (fd, row)
{
    char msg[100];

    if (FCB.io_error) return;

    sprintf (msg,"layer [%s] - unable to write row %d", FCB.name, row);
    G_warning(msg);
    FCB.io_error = 1;
}

static
same (x, y, n)
    register unsigned char *x, *y;
    register n;
{
    while (n-- > 0)
	if (*x++ != *y++)
	    return 0;
    return 1;
}
