/**********************************************************************
 *
 *   G_get_map_row (fd, buf, row)
 *      int fd                     file descriptor for the opened map
 *      CELL *buf                  buffer for the row to be placed into
 *      int row                    data row desired
 *
 *   Reads appropriate information into the buffer "buf" associated 
 *   with the requested row "row".  "buf" is associated with the
 *   current window.
 *
 *   Step 1:  Read appropriate raw map data into a buffer.
 *   Step 2:  Resample this buffer into "buf"  (for current window)
 *   Step 3:  If a mask is being used, mask "buf" with mask buffer.
 *
 *   returns:    1  if successful
 *               0  row requested not within window
 *              -1  on fail
 *
 *   diagnostics: first read request (for each open cell file)
 *                that fails will generate a warning message.
 *                subsequent failures will be silent.
 **********************************************************************/

#include "G.h"

#define FCB         G__.fileinfo[fd]
#define ROW         FCB.cur_row 
#define DATA_BUF    FCB.data

#define CMAP        FCB.col_map

#define WINDOW          G__.window
#define COMP_BUF        G__.compressed_buf
#define DATA_NCOLS      FCB.cellhd.cols
#define DATA_NROWS      FCB.cellhd.rows
#define DATA_FORMAT     FCB.cellhd.format
#define COMPRESSED      FCB.cellhd.compressed

#define AUTO_MASKING G__.auto_mask
#define MASK_FD      G__.mask_fd
#define MASK_BUF     G__.mask_buf


/********************************************************************/
G_get_map_row (fd, buf, row)
    register CELL *buf;
{
    int stat;

    stat = G_get_map_row_nomask (fd, buf, row);
    if(stat <= 0)
        return stat;

    if (AUTO_MASKING > 0)
    {
        register CELL *mask;
        register int n;

        if (G_get_map_row_nomask (MASK_FD, mask = MASK_BUF, row) < 0)
            return stat;

        n = WINDOW.cols;
        while (n-- > 0)
        {
            if (*mask++ == 0)
                *buf = 0;
            buf++;
        }
    }

    return stat;
}


/*********************************************************************/
G_get_map_row_nomask (fd, cell, row)
    CELL *cell;
{
    int r;
    double f;
    register int i;
    CELL *reclass_table ;

#ifdef DEBUG
printf("get_row(%s in %s) %d: %d bytes\n\r",FCB.name,FCB.mapset,row,FCB.nbytes);
#endif

/* check for row in window */
    if (row < 0 || row >= WINDOW.rows)
    {
        char msg[128];

        ROW = -1;
        i = WINDOW.cols;
	while(i-- > 0)
	    *cell++ = 0;

	sprintf (msg,"[%s in %s] - read request for row %d is outside window",
		FCB.name, FCB.mapset, row);
	G_warning (msg);

        return -1;
    }

/* convert window row to cell file row */
    f = row * FCB.C1 + FCB.C2;
    r = f;
    if (f < r) /* adjust for rounding up of negatives */
	r--;
    if (r < 0 || r >= DATA_NROWS)
    {
        ROW = -1;
        i = WINDOW.cols;
	while(i-- > 0)
	    *cell++ = 0;

       return 0;
    }

/* read cell file row if not in memory */
    if (r != ROW )
    {
#ifdef DEBUG
printf ("read row %d\n", r);
#endif
	if(read_data (fd, ROW=r, DATA_BUF, &FCB.cur_nbytes) < 0)
	{
	    i = WINDOW.cols;
	    while(i-- > 0)
		*cell++ = 0;

	    if (!FCB.io_error)
	    {
		char msg[128];

		sprintf (msg,
		    "error reading %sdata layer [%s] in mapset [%s], row %d",
		    COMPRESSED ? "compressed " : "", FCB.name, FCB.mapset, r);
		G_warning (msg);
		FCB.io_error = 1;
	    }
	    return -1;
	}
    }

/* copy cell file data to user buffer translated by window column mapping */
    cell_values (DATA_BUF, CMAP, FCB.cur_nbytes, cell, WINDOW.cols);

/* do reclass now */
    if (FCB.reclass_flag)                    /* reclass required */
    {
	CELL v;
	CELL max;
	CELL min;
#ifdef DEBUG
printf ("   reclass\n\r");
#endif
        i = WINDOW.cols;
        reclass_table = FCB.reclass.table;
	min = FCB.reclass.min;
	max = FCB.reclass.max;

	while (i-- > 0)
	{
	    v = *cell;
	    if (v < min || v > max)
		*cell++ = 0;
	    else
		*cell++ = reclass_table[v-min];
	}
    }

    return 1;
}

/* Actually read a row of data in */
static
read_data (fd, row, data_buf, nbytes)
    int fd ;
    int row ;
    unsigned char *data_buf;
    int *nbytes;
{
    int readamount ;
    int n;


/* If map is in compressed form */
    if (COMPRESSED)
    {
	long t1,t2;
	unsigned char *cmp;

        if (lseek(fd,FCB.row_ptr[row],0) < 0)
            return -1;

        t1 = FCB.row_ptr[row+1] ;
        t2 = FCB.row_ptr[row] ;
        readamount = t1 - t2 ;

        if (read(fd,cmp = COMP_BUF,readamount) != readamount)
            return -1;

    /* Now decompress the row */
	if (COMPRESSED > 0)
	{
	    n = *nbytes = *cmp++;
	    readamount--;	/* one byte is nbyte count */
	}
	else                      /* pre 3.0 compression */
	    n = *nbytes = FCB.nbytes;
#ifdef DEBUG
printf("   read_data %srow %d (nbytes %d)\n\r",COMPRESSED?"compressed ":"", row, n);
#endif
	if (COMPRESSED < 0 || readamount < n * DATA_NCOLS)
        {
            register unsigned char repeat ;
            register unsigned char *col ;
            int pair ;
            int k;

#ifdef DEBUG
printf ("  read_data: decompressing the row\n");
#endif
            col = data_buf ;

	    pair = readamount / (n+1) ;
	    while (pair-- > 0)
	    {
		repeat = *cmp++ ;
		while(repeat--)
		    for (k = 0; k < n; k++)
			*col++ = cmp[k];
		cmp += n;
	    }
#ifdef DEBUG
printf ("  read_data: decompressed!\n");
#endif
        }
	else              /* this row not compressed */
	{
	    while (readamount-- > 0)
		*data_buf++ = *cmp++;
	}
        return 0 ;
    }

/* If map is in uncompressed form */
    else
    {
/* seek to proper place in file */
	n = *nbytes = FCB.nbytes;
        if (lseek(fd, (long) row * DATA_NCOLS * n, 0) == -1)
	    return -1 ;

/* read one row in */
        readamount = DATA_NCOLS * n;
        if (read(fd, data_buf, readamount) != readamount)
            return -1 ;

        return 0 ;
    }
}

static
cell_values (data, cmap, nbytes, cell, n)
    register unsigned char *data;
    register COLUMN_MAPPING *cmap;
    register int nbytes;
    register CELL *cell;
    register int n;
{
    register CELL v;
    register unsigned char *d;
    register int offset;
    register int nb;
    register int neg;
    int big;

    big = nbytes >= sizeof(CELL);
    while (--n >= 0)
    {
	if(!(offset = *cmap++))
	    *cell++ = 0;
	else
	{
	    d = data + (offset-1) * nbytes;
	    if (big && (*d & 0200))
	    {
		neg = 1;
		v = *d++ & 0177;
	    }
	    else
	    {
		neg = 0;
		v = *d++;
	    }
	    for (nb=nbytes; --nb > 0;)
		v = (v << 8) + *d++;    /* v = (v * 256) + *data++; */
	    if (neg)
		v = -v;
 
	    *cell++ = v;
	}
    }
}
