/*
 * $Id$
 */
/**********************************************************************
 *
 *   G_zeros_r_nulls (zeros_r_nulls)
 *      int zeros_r_nulls	the last argument of put_data()
 *
 *   zeros_r_nulls > 0		zero values of buf to be written into files
 *   				are null values by default.
 *
 *   zeros_r_nulls == 0		zero values are just zero itself.
 *
 *   zeros_r_nulls < 0		do not set. return current setting.
 *   				1: set
 *   				0: not set
 *
 *   Return setting values in all cases.
 *
 *   *** NOTE *** 
 *   Use only to change a default behavior for zero of G_put_map_row[_random].
 *
 ********************************************************************** 
 *
 *   G_put_[f/d_]raster_row (fd, buf)
 *      int fd           file descriptor of the opened map
 *      [F/D]CELL *buf   buffer holding row info to be written
 *
 *   Writes the next row for the cell file opened on 'fd' from 'buf'
 *   All writes go into NEW files that exactly match the current window.
 *   The file must have been opened with G_open_cell_new()
 *   and be written sequentially, ie no skipping rows
 *
 *   when the null values are embeded into the data, corresponding cells are 
 *   changed to 0's and the corresponding null value row is written into null 
 *   file.
 *
 *   *** NOTE *** 
 *   A map cannot be copied using G_get_raster_row() and G_put_raster_row().
 *   The former resamples the data of the original map into a row buffer
 *   that matches the current window.  The later writes out rows associated
 *   with the window.
 *
 *   returns:    1  if successful
 *              -1  on fail
 *
 *  Keeps track of the minimum and maximum cell value  for use in updating
 *  the range file upon close of the cell file.
 *  HOWEVER when nulls are not embeded, the cells are considered 0's as far
 *  as updating range is concerned, even if the corresponding cell is null
 *  in the resulting null file, so programmer should be carefult to set all 
 *  the null values using G_set_null_value() or G_insert_[d/f_]null_values()
 *
 ********************************************************************** 
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
 *   NULLS are written into null bitmap file for all cells which are zero,
 *   and cells which have null value (these cells are converted to 0's before
 *   writing) 
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
 *  Keeps track of the minimum and maximum cell value  for use in updating
 *  the range file upon close of the cell file.
 *
 ********************************************************************** 
 *
 *  G_put_map_row_random (fd, buf, row, col, ncells)
 *      int fd                  File descriptor where data is to be written
 *      [F/D]CELL *buf          Buffer holding data
 *      int row                 Map row where data is to be written
 *      int col                 Column where data begins
 *      int ncells              Number of columns of data to be written
 *
 *   Writes parts of rows into open cell file.
 *
 *   Cell file must have been opened with G_open_cell_new_random()
 *   except it can't write null file.
 *
 *   returns:    0  if successful
 *              -1  on fail
 *
 *   behaves the same as G_put_map_row()
 *
 **********************************************************************
 *
 *  Note: there is no G_put_[d/f_]raster_row_randow() because even though
 *  it is possible to randomply write floating and integer rows, it is not
 *  possible to rand. write null data, so the null file can't
 *  be updated correctly.
 *
 ***********************************************************************
 *
 *  G__put_null_value_row (fd, buf, row, col, ncells)
 *      int fd                  File descriptor where data is to be written
 *      char *buf               Buffer holding null data
 *      int row                 Map row where data is to be written
 *      int col                 Column where data begins
 *      int ncells              Number of columns of data to be written
 *
 *   converts a buffer of zero's and ones to bitstream and stores this 
 *   bitstream in memory. (the null rows from memory are written into null
 *   file after the limit is reached, and the place for new null rows
 *   to be kept in memory is freed. Should not be used by application
 *   programs.
 *
 *   returns:    0  if successful
 *              -1  on fail
 **********************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>

#include "config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "G.h"

#define FCB          G__.fileinfo[fd]
#define MIN_NULL_ROW FCB.min_null_row
#define NULL_BUF     G__.null_buf
#define WORK_BUF     FCB.data
#define WINDOW       G__.window

/* Needed until zlib stuff gets worked out, comment out if you
 * want to try G_zlib_*() funcs, also do the same in get_row.c
 */
/* #define USE_LZW_COMPRESSION */
 
/* convert type "RASTER_MAP_TYPE" into index */
#define F2I(map_type) \
        (map_type == CELL_TYPE ? 0 : (map_type == FCELL_TYPE ? 1 : 2))

static int ERROR;
static char *me;
static RASTER_MAP_TYPE write_type;
static int _zeros_r_nulls = 1;

static int put_raster_data (int,void *,int,int,int,int,RASTER_MAP_TYPE);
static int put_data (int,CELL *,int, int,int,int);
static int check_open (int,int);
static int adjust (int, int *, int *);
static int write_error (int,int);
static int same( unsigned char *, unsigned char *, int);
static int seek_random (int, int, int);
static void set_file_pointer (int,int);
static void update_compressed_bits (int,int);
static int put_fp_data (int,void *,int,int,int,RASTER_MAP_TYPE);
static int put_null_data (int,char *, int);
static void convert_and_write_if (int, CELL *);
static void convert_and_write_id (int, CELL *);
static void convert_and_write_fi (int, FCELL *);
static void convert_and_write_di (int, DCELL *);
static void convert_and_write_df (int, DCELL *);
static void convert_and_write_fd (int, FCELL *);
static void dummy (int, void *);

static void (*convert_and_write_FtypeOtype [3][3])() =
  {{dummy,                convert_and_write_if, convert_and_write_id},
   {convert_and_write_fi, dummy               , convert_and_write_fd},
   {convert_and_write_di, convert_and_write_df, dummy               }};

#define CONVERT_AND_WRITE \
     (convert_and_write_FtypeOtype [F2I (write_type)] [F2I (FCB.map_type)])

int G_zeros_r_nulls (int zeros_r_nulls)
{
    if (zeros_r_nulls >= 0)
	_zeros_r_nulls = zeros_r_nulls > 0;

    return _zeros_r_nulls;
}

int G_put_map_row (int fd, CELL *buf)
{
    me = "G_put_map_row";
    if (!check_open (fd,0))
        return -1;

    write_type = CELL_TYPE;
    if(FCB.map_type != write_type)
    {
	char buf[300];
	sprintf(buf,"%s: %s is not integer! Use G_put_[f/d_]raster_row()!",
		    me, FCB.name);
	G_fatal_error(buf);
        return -1;
    }

    G_zero (NULL_BUF, FCB.cellhd.cols * sizeof(char));

    switch(put_data (fd, buf, FCB.cur_row, 0, FCB.cellhd.cols, _zeros_r_nulls))
    {
        case -1: return -1;
        case  0: return  1;
    }

    /* only for integer maps */
    if (FCB.want_histogram)
	G_update_cell_stats(buf, FCB.cellhd.cols, &FCB.statf);

    G__row_update_range (buf, FCB.cellhd.cols, &FCB.range, 1);

    FCB.cur_row++;

    /* write the null row for the data row */
    return G__put_null_value_row(fd, NULL_BUF);
}

int G_put_map_row_random (int fd, CELL *buf, int row, int col, int n)
{
    me = "G_put_map_row_random";
    if (!check_open (fd,1))
        return -1;

    buf += adjust (fd, &col, &n);
    switch(put_data (fd, buf, row, col, n, _zeros_r_nulls))
    {
        case -1: return -1;
        case  0: return  1;
    }

    /* only for integer maps */
    if (FCB.want_histogram)
	G_update_cell_stats (buf, n, &FCB.statf);

    G_row_update_range (buf, n, &FCB.range);

    return 1;
}

int G__put_null_value_row (int fd, char *buf)
{
    me = "G__put_null_value_row";
    switch(put_null_data (fd, buf, FCB.null_cur_row))
    {
        case -1: return -1;
        case  0: return  1;
    }

    FCB.null_cur_row++;
    return 1;
}

int G_put_raster_row (int fd, void *buf, RASTER_MAP_TYPE data_type)
{
    me = "G_put_raster_row";
    if (!check_open (fd,0))
        return -1;

    write_type = data_type;
    if(FCB.map_type != write_type)
    {
        ERROR = 0;
        CONVERT_AND_WRITE(fd, buf);
        if(ERROR) return -1;
        else return 1;
    }

    G_zero (NULL_BUF, FCB.cellhd.cols * sizeof(char));

    switch(put_raster_data (fd, buf, FCB.cur_row, 0, FCB.cellhd.cols, 0, data_type))
    {
        case -1: return -1;
        case  0: return  1;
    }

    /* only for integer maps */
    if (data_type == CELL_TYPE)
    {
	if (FCB.want_histogram)
 	    G_update_cell_stats(buf, FCB.cellhd.cols, &FCB.statf);
        G_row_update_range ((CELL *) buf, FCB.cellhd.cols, &FCB.range);
    }
    else
        G_row_update_fp_range (buf, FCB.cellhd.cols, &FCB.fp_range, data_type);

    FCB.cur_row++;

    /* write the null row for the data row */
    return G__put_null_value_row(fd, NULL_BUF);
}

int G_put_c_raster_row (int fd, CELL *buf)
{
    return G_put_raster_row(fd, (void *) buf, CELL_TYPE);
}

int G_put_f_raster_row (int fd, FCELL *buf)
{
    return G_put_raster_row(fd, (void *) buf, FCELL_TYPE);
}

int G_put_d_raster_row (int fd, DCELL *buf)
{
    return G_put_raster_row(fd, (void *) buf, DCELL_TYPE);
}

static int put_raster_data (int fd, void *rast, int row, int col, int n, int zeros_r_nulls, RASTER_MAP_TYPE map_type)
{
    switch(map_type)
    {
       case CELL_TYPE: return put_data (fd, (CELL *) rast, row, col, n, zeros_r_nulls);
       default: return put_fp_data (fd, rast, row, col, n, map_type);
    }
}

/***************************************************************
* put_data (fd, cell, row, col, n, zeros_r_nulls)
*
* writes data to cell file for either full or partial rows
*
***************************************************************/

static int put_data (int fd, CELL *cell,
    int row, int col, int n, int zeros_r_nulls)
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
fprintf (stderr, "%s(mode=%d): row=%d, col=%d, n=%d (FCB.nbytes=%d)\n\r",
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
        /* substitute embeded null vals by 0's */
        if (G_is_c_null_value(&v))
        {
            v = 0;
            if(!random) NULL_BUF[col + i] = 1;
        }
	else if(!random && zeros_r_nulls && !v)
	    NULL_BUF[col + i] = 1;

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
fprintf (stderr, "row %d - after cell size reduction: nbytes %d\n", row, nbytes);
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
		G_copy ((char *) cp, (char *) cur, nbytes);
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
		G_copy ((char *) cp, (char *) cur, nbytes);
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
fprintf (stderr, "writing %d cells at %d bytes per cell\n", n, FCB.nbytes);
#endif
	if (write (fd, G__.work_buf, nwrite) != nwrite)
	{
	    write_error (fd, row);
	    return -1;
	}
    }
    return 1;
}

static int check_open (int fd,int random)
{
    char msg[100];
    switch (FCB.open_mode)
    {
    case OPEN_OLD:
	sprintf (msg,
	    "%s: map [%s] not open for write - request ignored",
	    me, FCB.name);
	break;
    case OPEN_NEW_COMPRESSED:
    case OPEN_NEW_UNCOMPRESSED:
	if (!random) return 1;
	sprintf (msg,
	    "%s: map [%s] not open for random write - request ignored",
	    me, FCB.name);
	break;
    case OPEN_NEW_RANDOM:
	if (random) return 1;
	sprintf (msg,
	    "%s: map [%s] not open for sequential write - request ignored",
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
static int adjust (int fd, int *col, int *n)
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

static int write_error (int fd,int row)
{
    if (FCB.io_error) return 0;

    G_warning("map [%s] - unable to write row %d", FCB.name, row);
    FCB.io_error = 1;

    return 0;
}

static int same(
     register unsigned char *x,
     register unsigned char *y,
     register int n)
{
    while (n-- > 0)
	if (*x++ != *y++)
	    return 0;
    return 1;
}

/*--------------------------------------------------------------------------*/

int G__write_data (int fd, int row, int n)

{
  int nwrite;

  nwrite = FCB.nbytes * n;

  if (write (fd, G__.work_buf, nwrite) != nwrite) {
    write_error (fd, row);
    return -1;
  }

  return 0;
}

/*--------------------------------------------------------------------------*/

/* i use a naive heuristic to set the max number of bits used for the lzw
   compression. this heuristsic adjusts the max number of bits according to
   the length of the row. 
*/

int G__write_data_compressed (int fd, int row, int n)

{
  int nwrite;
#ifdef USE_LZW_COMPRESSION
  int l;

  nwrite = FCB.nbytes * n;

  l = log((double) nwrite) / log((double) 2);  
  
  if ((1 << l) > (nwrite * 3.0 / 4.0)) l--;     /* just a guess */

  if (l > 16) l = 16;      
  if (l < 9) l = 9;

  G_lzw_set_bits (l);

  if ((nwrite = G_lzw_write (fd, G__.work_buf, nwrite)) < 0) {
    write_error (fd, row);
    return -1;
  }
#else
  nwrite = FCB.nbytes * n;

  if ((nwrite = G_zlib_write (fd, G__.work_buf, nwrite)) < 0) {
    write_error (fd, row);
    return -1;
  }
#endif
  
  return 0;
}

/*--------------------------------------------------------------------------*/

static int seek_random (int fd, int row, int col)
{
  long offset;

  offset = (long) (FCB.cellhd.cols * row + col) * FCB.nbytes;

  if (lseek (fd, offset, 0) < 0) {
    write_error (fd, row);
    return -1;
  }

  return 0;
}

/*--------------------------------------------------------------------------*/

static void set_file_pointer (int fd, int row)

{
  FCB.row_ptr[row] = lseek (fd, 0L, 1);
}

/*--------------------------------------------------------------------------*/

static void update_compressed_bits (int fd, int row)

{
#ifdef USE_LZW_COMPRESSION
  if ((row == 0) || (FCB.compression_bits < G_lzw_max_used_bits ()))
    FCB.compression_bits = G_lzw_max_used_bits ();
#else
  /* Not relevant to zlib */
  FCB.compression_bits = -1;
#endif
}

/*--------------------------------------------------------------------------*/

/* writes data to fcell file for either full or partial rows */

static int put_fp_data (int fd, void *rast, int row, int col, int n, RASTER_MAP_TYPE data_type)

{
  int i, random, compressed;
  register XDR* xdrs;
  DCELL d;
  FCELL f;
  
  random     = (FCB.open_mode == OPEN_NEW_RANDOM);
  compressed = (FCB.open_mode == OPEN_NEW_COMPRESSED);

  /* row out of window? */
  if (row < 0 || row >= FCB.cellhd.rows) return 0;
  
  if (n <= 0) return 0;
  
  if (random) {
    if (seek_random (fd, row, col) == -1) return -1;
  } else 
    if (compressed) set_file_pointer (fd, row);
  
  xdrmem_create (&FCB.xdrstream, G__.work_buf,
                 (u_int) (FCB.nbytes * FCB.cellhd.cols), XDR_ENCODE);
  xdrs = &FCB.xdrstream; /* xdr stream is initialized to write into */
  xdr_setpos (xdrs, 0);  /* G__.work_buf in 'opencell.c' */
  
  for (i = 0; i < n; i++) 
  {
      if(data_type == FCELL_TYPE)
      {
         /* substitute embeded null vals by 0's */
	 if(G_is_f_null_value((FCELL *) rast))
	 {
	     f = 0.;
	     if(!random) NULL_BUF[col + i] = 1;
         }
	 else 
	     f = *((FCELL *) rast);
         if (! xdr_float (xdrs, &f)) 
         {
          G_warning("xdr_float failed for index %d of row %d.\n", 
                                              i, row);
          return -1;
        }
      }
      else
      {
         /* substitute embeded null vals by 0's */
	 if(G_is_d_null_value((DCELL *) rast))
	 {
	     d = 0.;
	     if(!random) NULL_BUF[col + i] = 1;
         }
	 d = *((DCELL *) rast);
         if (! xdr_double (xdrs, &d)) 
         {
          G_warning("xdr_double failed for index %d of row %d.\n", 
                                              i, row);
          return -1;
        }
      }
      rast = G_incr_void_ptr(rast, G_raster_size(data_type));
  } /* for-loop */

  xdr_destroy (&FCB.xdrstream);
  
  if (compressed) {
    if (G__write_data_compressed (fd, row, n) == -1) return -1;
    update_compressed_bits (fd, row);
  } else
    if (G__write_data (fd, row, n) == -1) return -1;
  
  return 1;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int put_null_data (int fd, char *flags, int row)
{
   int null_fd, i;

   if (MIN_NULL_ROW + NULL_ROWS_INMEM <= row)

   /* the row is out of the range of rows stored in memory */
   /* write out all the rows kept in memory, and initialize memory
      for keeping new NULL_ROWS_INMEM rows */
   {
      if(MIN_NULL_ROW >= 0)
      {
         null_fd = G__open_null_write(fd);
	 if(null_fd <= 0) return -1;

         for (i= 0; i < NULL_ROWS_INMEM ; i++)
         {
         /* FCB.cellhd.rows doesn't have to be a miultiple of NULL_ROWS_INMEM */
            if(i+MIN_NULL_ROW >= FCB.cellhd.rows)
                    break;

            if( G__write_null_bits(null_fd, FCB.NULL_ROWS[i], 
                        i+MIN_NULL_ROW, FCB.cellhd.cols, fd) < 0)
                     return -1;

         } /* done writing out memory rows */
         if(null_fd > 0) close (null_fd);
      }

      /* now initialize memory to store new NULL_ROWS_INMEM rows */
      MIN_NULL_ROW = MIN_NULL_ROW + NULL_ROWS_INMEM;
      /* init memory to store next NULL_ROWS_INMEM rows */

    } /* row out of the range */

    /* rememer the null row for i for the future writing */
    G__convert_01_flags(flags, FCB.NULL_ROWS[row - MIN_NULL_ROW], FCB.cellhd.cols);
    return 1;
}

int G__open_null_write (int fd)
{
   int null_fd;

   if (access(FCB.null_temp_name,0) != 0) 
   {
       G_warning("unable to find a temporary null file %s", 
             FCB.null_temp_name);
       return -1;
   }
   null_fd = open(FCB.null_temp_name, 2);
   if(null_fd <= 0)
       return -1;

   if(null_fd >= MAXFILES)
   {
       G_warning("G__open_null_new(): too many open files!");
       close (null_fd);
       return -1;
   }

   return null_fd;
}

int G__write_null_bits (int null_fd, unsigned char *flags, int row, int cols, int fd)
{
   long offset;
   int size;

   size = G__null_bitstream_size(cols);
   offset = (long) (size * row * sizeof(unsigned char)) ; 
   if (lseek (null_fd, offset, 0) < 0)
   {
       G_warning("error writing null row %d\n",row);
       return -1;
   }
   if (write (null_fd, flags, size) != size)
   {
       G_warning("error writing null row %d\n",row);
       return -1;
   }
   return 1;
}

static void convert_and_write_if (int fd, CELL *buf)
{
    FCELL * p;
    CELL * q;
    int col;

    p = (FCELL *) WORK_BUF;
    q = buf;
    col = FCB.cellhd.cols;
    while (col-- > 0)
        *p++ = *q++ ;

    G_put_f_raster_row(fd, (FCELL *) WORK_BUF);
}

static void convert_and_write_id (int fd, CELL *buf)
{
    DCELL * p;
    CELL * q;
    int col;

    p = (DCELL *) WORK_BUF;
    q = buf;
    col = FCB.cellhd.cols;
    while (col-- > 0)
        *p++ = *q++ ;

    G_put_d_raster_row(fd, (DCELL *) WORK_BUF);
}

static void convert_and_write_fi (int fd, FCELL *buf)
{
    G_warning("can't put float row into integer map"); 
    ERROR = 1;
}

static void convert_and_write_di (int fd, DCELL *buf)
{
    G_warning("can't put double row into integer map"); 
    ERROR = 1;
}

static void convert_and_write_df (int fd, DCELL *buf)
{
    FCELL * p;
    DCELL * q;
    int col;

    p = (FCELL *) WORK_BUF;
    q = buf;
    col = FCB.cellhd.cols;
    while (col-- > 0)
        *p++ = *q++ ;

    G_put_f_raster_row(fd, (FCELL *) WORK_BUF);
}

static void convert_and_write_fd (int fd, FCELL *buf)
{
    DCELL * p;
    FCELL * q;
    int col;

    p = (DCELL *) WORK_BUF;
    q = buf;
    col = FCB.cellhd.cols;
    while (col-- > 0)
        *p++ = *q++ ;

    G_put_d_raster_row(fd, (DCELL *) WORK_BUF);
}

static void dummy (int fd, void *buf)
{
  return;
}

