/**********************************************************************
 *
 *   get_row_nomask() works for all map types and doesn't consider
 *   null row corresponding to the requested row 
 *
 *   G_get_map_row (fd, buf, row)
 *      int fd                     file descriptor for the opened map
 *      CELL *buf                  buffer for the row to be placed into
 *      int row                    data row desired
 *
 *   G_get_raster_row (fd, buf, row, data_type)
 *      int fd                     file descriptor for the opened map
 *      void *buf                  buffer for the row to be placed into
 *      int row                    data row desired
 *      RASTER_MAP_TYPE data_type  FCELL_TYPE, DCELL_TYPE, or CELL_TYPE.
 *
 *   G_get_c_raster_row (fd, buf, row)
 *      int fd                     file descriptor for the opened map
 *      CELL *buf                  buffer for the row to be placed into
 *      int row                    data row desired
 *
 *   G_get_f_raster_row (fd, buf, row)
 *      int fd                     file descriptor for the opened map
 *      FCELL *buf                 buffer for the row to be placed into
 *      int row                    data row desired
 *
 *   G_get_d_raster_row (fd, buf, row)
 *      int fd                     file descriptor for the opened map
 *      DCELL *buf                 buffer for the row to be placed into
 *      int row                    data row desired
 *
 *   Reads appropriate information into the buffer "buf" associated 
 *   with the requested row "row".  "buf" is associated with the
 *   current window.
 *
 *   Note, that the type of the data in "buf" (say X) is independent of 
 *   the type of the data in the file described by fd (say Y).
 *
 *   Step 1:  Read appropriate raw map data into a intermediate buffer.
 *   Step 2:  Convert the data into a CPU readable format, and subsequently
 *            resample the data. the data is stored in a second intermediate 
 *            buffer (the type of the data in this buffer is Y).
 *   Step 3:  Convert this type Y data into type X data and store it in
 *            buffer "buf". Conversion is performed in functions 
 *            "transfer_to_cell_XY". (For details of the conversion between
 *            two particular types check the functions).
 *   Step 4:  read or simmulate null value row and zero out cells corresponding 
 *            to null value cells. The masked out cells are set to null when the
 *            mask exists. (the MASK is taken care of by null values
 *            (if the null file doesn't exist for this map, then the null row
 *            is simulated by assuming that all zero are nulls *** in case
 *            of G_get_raster_row() and assuming that all data is valid 
 *            in case of G_get_f/d_raster_row(). In case of deprecated function
 *            G_get_map_row() all nulls are converted to zeros (so there are
 *	      no embedded nulls at all). Also all masked out cells become zeros.
 *
 *   returns:    1  if successful
 *               0  row requested not within window
 *              -1  on fail
 *
 *   diagnostics: first read request (for each open cell file)
 *                that fails will generate a warning message.
 *                subsequent failures will be silent.
 **********************************************************************
 *   G_get_null_value_row (fd, buf, row)
 *      int fd                     file descriptor for the opened map
 *      char *buf                  buffer for the row to be placed into
 *      int row                    data row desired
 *
 *   read or simmulate null value row and set the cells corresponding 
 *   to null value to 1. The masked out cells are set to null when the
 *   mask exists. (the MASK is taken care of by null values
 *   (if the null file doesn't exist for this map, then the null row
 *   is simulated by assuming that all zeros in raster map are nulls.
 *   Allso all masked out cells become nulls.
 *
 **********************************************************************/

#include "config.h"
#include <rpc/types.h> /* need this for sgi */
#include <rpc/xdr.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/types.h>
#include <unistd.h>

#include "G.h"

#define FCB          G__.fileinfo[fd]
#define ROW          FCB.cur_row 
#define DATA_BUF     FCB.data
#define MIN_NULL_ROW FCB.min_null_row
#define NULL_BUF     G__.null_buf

#define CMAP        FCB.col_map

#define NULL_FILE   "null"
#define WINDOW          G__.window
#define COMP_BUF        G__.compressed_buf
#define DATA_NCOLS      FCB.cellhd.cols
#define DATA_NROWS      FCB.cellhd.rows
#define DATA_FORMAT     FCB.cellhd.format
#define COMPRESSED      FCB.cellhd.compressed

#define AUTO_MASKING     G__.auto_mask
#define MASK_FD          G__.mask_fd
#define MASK_BUF         G__.mask_buf
#define NULL_FILE_EXISTS FCB.null_file_exists

/*--------------------------------------------------------------------------*/
/* Until the zlib stuff gets worked out, comment out if you want to try
 * the G_zlib_*() functions, also do the same in put_row.c
 */
/* #define USE_LZW_COMPRESSION */

/*--------------------------------------------------------------------------*/

#define DATA_BUF2    G__.work_buf 

/*--------------------------------------------------------------------------*/

/* convert type "RASTER_MAP_TYPE" into index */
#define F2I(map_type) \
        (map_type == CELL_TYPE ? 0 : (map_type == FCELL_TYPE ? 1 : 2))

/*--------------------------------------------------------------------------*/

static int 
compute_window_row (int fd, int row, int *cellRow)

{
  double f;
  int r;

  /* check for row in window */
  if (row < 0 || row >= WINDOW.rows) {
    G_warning ("[%s in %s] - read request for row %d is outside region",
	     FCB.name, FCB.mapset, row);
    
    return -1;
  }

  /* convert window row to cell file row */
  f = row * FCB.C1 + FCB.C2;
  r = f;
  if (f < r) /* adjust for rounding up of negatives */
    r--;

  if (r < 0 || r >= DATA_NROWS) return 0;

  *cellRow = r;
  return 1;
}

/*--------------------------------------------------------------------------*/

static void 
do_reclass_int (int fd, void *cell, int null_is_zero)

{
  CELL v;
  CELL max;
  CELL min;
  CELL *reclass_table;
  register int i;
  register CELL *c;
  
#ifdef DEBUG
fprintf (stderr, "   reclass\n\r");
#endif

  c = (CELL *) cell;

  i = WINDOW.cols;
  reclass_table = FCB.reclass.table;
  min = FCB.reclass.min;
  max = FCB.reclass.max;
  
  while (i-- > 0) 
  {
    if(G_is_c_null_value(c)) 
    {
       c++;
       continue;
    }
    v = *(c);
    if (v < min || v > max)
    {
       if(null_is_zero)
	   *(c)++ = 0;
       else
           G_set_c_null_value(c++,1);
    }
    else
      *(c)++ = reclass_table [v - min];
  }
}

/*--------------------------------------------------------------------------*/

static void 
do_reclass_float (int fd, void *cell, int null_is_zero)
     
{
  register FCELL *c;

  c = (FCELL *) cell;
}

/*--------------------------------------------------------------------------*/

static void 
do_reclass_double (int fd, void *cell, int null_is_zero)

{
  register DCELL *c;

  c = (DCELL *) cell;
}

/*--------------------------------------------------------------------------*/

static int 
read_data_fp_compressed (int fd, int row, unsigned char *data_buf, int *nbytes)

{
  if (lseek(fd, FCB.row_ptr[row],0) < 0) return -1;

  *nbytes = FCB.nbytes;
#ifdef USE_LZW_COMPRESSION
  if (G_lzw_read (fd, data_buf, DATA_NCOLS * FCB.nbytes) != 
      DATA_NCOLS * FCB.nbytes) 
#else
  /* The subtraction of offsets lets G_zlib_read know exactly
   * how many bytes to read (which in some case may be more than
   * the output bytes)
   */
  if (G_zlib_read (fd, 
			  FCB.row_ptr[row+1] - FCB.row_ptr[row],
	               	  data_buf, DATA_NCOLS * FCB.nbytes) !=
		  DATA_NCOLS * FCB.nbytes)
#endif
    return -1;

  return 0;
}

/*--------------------------------------------------------------------------*/

static int 
read_data_compressed (int fd, int row, unsigned char *data_buf, int *nbytes)

{
  int readamount;
  int n;
  long t1,t2;
  unsigned char *cmp;

  if (lseek(fd,FCB.row_ptr[row],0) < 0)
    return -1;
  
  t1 = FCB.row_ptr[row+1];
  t2 = FCB.row_ptr[row];
  readamount = t1 - t2;
  
  if (read(fd,cmp = COMP_BUF,readamount) != readamount)
    return -1;
  
  /* Now decompress the row */
  if (COMPRESSED > 0) {
    n = *nbytes = *cmp++;
    readamount--;	/* one byte is nbyte count */
  }
  else                      /* pre 3.0 compression */
    n = *nbytes = FCB.nbytes;
  
#ifdef DEBUG
fprintf(stderr, "   read_data %srow %d (nbytes %d)\n\r",COMPRESSED?"compressed ":"", row, n);
#endif
  
  if (COMPRESSED < 0 || readamount < n * DATA_NCOLS) {
    register unsigned char repeat;
    register unsigned char *col;
    int pair;
    int k;
    
#ifdef DEBUG
fprintf (stderr, "  read_data: decompressing the row\n");
#endif
    col = data_buf;
    
    pair = readamount / (n+1);
    while (pair-- > 0) {
      repeat = *cmp++;
      while(repeat--)
	for (k = 0; k < n; k++)
	  *col++ = cmp[k];
      cmp += n;
    }
    
#ifdef DEBUG
fprintf (stderr, "  read_data: decompressed!\n");
#endif
    
  } else {             /* this row not compressed */
    while (readamount-- > 0)
      *data_buf++ = *cmp++;
  }
  return 0;
}


/*--------------------------------------------------------------------------*/

static int 
read_data_uncompressed (int fd, int row, unsigned char *data_buf, int *nbytes)
{
  int readamount;

  *nbytes = FCB.nbytes;

  if (lseek(fd, (long) row * DATA_NCOLS * FCB.nbytes, 0) == -1) return -1;
  
  readamount = DATA_NCOLS * FCB.nbytes;
  if (read(fd, data_buf, readamount) != readamount) return -1;

  return 0;
}

/*--------------------------------------------------------------------------*/

/* Actually read a row of data in */

  static int (*read_data_type [3])() = {read_data_compressed,
					read_data_fp_compressed,
					read_data_fp_compressed};
#define READ_DATA_COMPRESSED (read_data_type [F2I (FCB.map_type)])

static int 
read_data (int fd, int row, unsigned char *data_buf, int *nbytes)
{

  if (COMPRESSED) /* map is in compressed form */
    return READ_DATA_COMPRESSED (fd, row, data_buf, nbytes);
  else 
    return read_data_uncompressed (fd, row, data_buf, nbytes);
}

/*--------------------------------------------------------------------------*/

/* copy cell file data to user buffer translated by window column mapping */

static void 
cell_values_int (int fd, register unsigned char *data, register COLUMN_MAPPING *cmap, register int nbytes, void *cell, register int n)

{
  register CELL v;
  register unsigned char *d;
  register int offset;
  register int nb;
  register int neg;
  int big;
  register CELL *c;

  c = (CELL *) cell;

    big = nbytes >= sizeof(CELL);
    while (--n >= 0)
    {
	if(!(offset = *cmap++))
	    *(c)++ = 0;
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
 
	    *(c)++ = v;
	}
    }

    return;
}

/*--------------------------------------------------------------------------*/

static void 
cell_values_float (int fd, register unsigned char *data, register COLUMN_MAPPING *cmap, register int nbytes, void *cell, register int n)

{
  register XDR* xdrs;
  register FCELL *c;
  register COLUMN_MAPPING cmapold;
  char rsbbuf[40];

  c = (FCELL *) cell;

  xdrs = &FCB.xdrstream; /* xdr stream is initialized to read from */
  xdr_setpos (xdrs, 0);  /* DATA_BUF in 'opencell.c' */

  cmapold = 0;
  while (--n >= 0) {

    if (*cmap) {

      if (*cmap != cmapold) {

	while (cmapold++ != *cmap) /* skip */
	  if (! xdr_float (xdrs, c)) {
	    /* fprintf (stderr, 
	      "ERROR: cell_values_f: xdr_float failed for index %d.\n", n);
	    exit (1); */
	    /* Roger Bivand 17 June 2000 */
	    sprintf(rsbbuf, "cell_values_f: xdr_float failed for index %d.", n);
	    G_fatal_error(rsbbuf);
	    return;
	  } 
	
	cmapold--;

      } else 

	*c = *(c - 1);

      c++;

    } else 
      *c++ = 0.;

    cmap++;
  }
}

/*--------------------------------------------------------------------------*/

static void 
cell_values_double (int fd, register unsigned char *data, register COLUMN_MAPPING *cmap, register int nbytes, void *cell, register int n)

{
  register XDR* xdrs;
  register DCELL *c;
  register COLUMN_MAPPING cmapold;
  char rsbbuf[40];

  c = (DCELL *) cell;

  xdrs = &FCB.xdrstream; /* xdr stream is initialized to read from */
  xdr_setpos (xdrs, 0);  /* DATA_BUF in 'opencell.c' */

  cmapold = 0;
  while (--n >= 0) {

    if (*cmap) {

      if (*cmap != cmapold) {

	while (cmapold++ != *cmap) /* skip */
	  if (! xdr_double (xdrs, c)) {
	    /* fprintf (stderr, 
	      "ERROR: cell_values_d: xdr_double failed for index %d.\n", n);
	    exit (1); */
	    /* Roger Bivand 17 June 2000 */
	    sprintf(rsbbuf, "cell_values_d: xdr_double failed for index %d.", n);
	    G_fatal_error(rsbbuf);
	    return;
	  } 
	
	cmapold--;

      } else 

	*c = *(c - 1);

      c++;

    } else 
      *c++ = 0;

    cmap++;
  }
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/* transfer_to_cell_XY takes bytes from DATA_BUF, converts these bytes with
   the appropriate procedure (e.g. XDR or byte reordering) into type X 
   values which are put into array DATA_BUF2.  
   finally the values in DATA_BUF2 are converted into 
   type Y and put into 'cell'.
   if type X == type Y the intermediate step of storing the values in 
   DATA_BUF2 might be ommited. check the appropriate function for XY to
   determine the procedure of conversion. 
*/

/*--------------------------------------------------------------------------*/

/* transfer: int --> int
             float --> float
	     double --> float
*/

static void 
transfer_to_cell_XX (int fd, void *cell)

{
  static void (*cell_values_type [3])() = {cell_values_int,
					   cell_values_float,
					   cell_values_double};
#define CELL_VALUES (cell_values_type [F2I (FCB.map_type)])

  CELL_VALUES (fd, DATA_BUF, CMAP, FCB.cur_nbytes, cell, WINDOW.cols);
}

/*--------------------------------------------------------------------------*/

/* transfer: int --> float */

static void 
transfer_to_cell_if (int fd, void *cell)

{
  register int n;
  register FCELL *c;
  register CELL *b;

  c = (FCELL *) cell;
  b = (CELL *) DATA_BUF2;

  transfer_to_cell_XX (fd, (void *) DATA_BUF2);
  n = WINDOW.cols;
  while (n-- > 0)
	*c++ = *b++ ;
}

/*--------------------------------------------------------------------------*/

/* transfer: int --> double */

static void 
transfer_to_cell_id (int fd, void *cell)

{
  register int n;
  register DCELL *c;
  register CELL *b;

  c = (DCELL *) cell;
  b = (CELL *) DATA_BUF2;

  transfer_to_cell_XX (fd, (void *) DATA_BUF2);
  n = WINDOW.cols;
  while (n-- > 0)
	*c++ = *b++ ;
}

/*--------------------------------------------------------------------------*/

/* transfer: float --> double */

static void 
transfer_to_cell_fd (int fd, void *cell)

{
  register int n;
  register DCELL *c;
  register FCELL *b;

  c = (DCELL *) cell;
  b = (FCELL *) DATA_BUF2;

  transfer_to_cell_XX (fd, (void *) DATA_BUF2);

  n = WINDOW.cols;
  while (n-- > 0)
	*c++ = *b++ ;
}

/*--------------------------------------------------------------------------*/

/* transfer: float --> int */

static void 
transfer_to_cell_fi (int fd, void *cell)

{
  register int n;
  register CELL *c;
  register CELL *cmap;
  register FCELL *b;

  c = (CELL *) cell;
  b = (FCELL *) DATA_BUF2;
  cmap = CMAP;

  transfer_to_cell_XX (fd, (void *) DATA_BUF2);

  /* translate FCELL row c into CELL row b using quant rules */
  n = WINDOW.cols;
  while (n-- > 0)
  {
     if(*cmap++ == 0)
 	*c++ = *b++ ; /* 0 */
     else
        *c++ = G_quant_get_cell_value(&(FCB.quant), (DCELL) *b++);
  }

}

/*--------------------------------------------------------------------------*/

/* transfer: double --> float */

static void 
transfer_to_cell_df (int fd, void *cell)

{
  register int n;
  register FCELL *c;
  register DCELL *b;

  c = (FCELL *) cell;
  b = (DCELL *) DATA_BUF2;

  transfer_to_cell_XX (fd, (void *) DATA_BUF2);

  n = WINDOW.cols;
  while (n-- > 0)
	*c++ = *b++ ;
}

/*--------------------------------------------------------------------------*/

/* transfer: double --> int */

static void 
transfer_to_cell_di (int fd, void *cell)

{
  register int n;
  register CELL *c;
  register DCELL *b;
  register CELL *cmap;

  c = (CELL *) cell;
  b = (DCELL *) DATA_BUF2;
  cmap = CMAP;

  transfer_to_cell_XX (fd, (void *) DATA_BUF2);

  /* translate DCELL row c into CELL row b using quant rules */
  n = WINDOW.cols;
  while (n-- > 0)
  {
     if(!*cmap++)
 	*c++ = *b++ ; /* 0 */
     else
        *c++ = G_quant_get_cell_value(&(FCB.quant), *b++);
  }
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int 
get_map_row_nomask (int fd, void *cell, int row, RASTER_MAP_TYPE cell_type)

{
  static void (*transfer_to_cell_FtypeOtype [3][3])() =
    {{transfer_to_cell_XX, transfer_to_cell_if, transfer_to_cell_id},
     {transfer_to_cell_fi, transfer_to_cell_XX, transfer_to_cell_fd},
     {transfer_to_cell_di, transfer_to_cell_df, transfer_to_cell_XX}};
#define TRANSFER_TO_CELL \
     (transfer_to_cell_FtypeOtype [F2I (FCB.map_type)] [F2I (cell_type)])

  int r;
  int rowStatus;

#ifdef DEBUG
fprintf(stderr, "get_row(%s in %s) %d: %d bytes\n\r",FCB.name,FCB.mapset,row,
       FCB.nbytes);
#endif

  if ((rowStatus = compute_window_row (fd, row, &r)) <= 0) {
    ROW = -1;
    G_zero_raster_buf (cell, cell_type);
    return rowStatus;
  }

  /* read cell file row if not in memory */
  if (r != ROW ) {

#ifdef DEBUG
fprintf (stderr, "read row %d\n", r);
#endif
    
    if (read_data (fd, ROW=r, DATA_BUF, &FCB.cur_nbytes) < 0) {
      G_zero_raster_buf (cell, cell_type);

      if (!FCB.io_error) {
	G_warning ( "error reading %smap [%s] in mapset [%s], row %d",
		 COMPRESSED ? "compressed " : "", FCB.name, FCB.mapset, r);
	FCB.io_error = 1;
      }
      return -1;
    }
  }

  TRANSFER_TO_CELL (fd, cell);

  return 1;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/* exported functions G_get_X_map_row_nomask and G_get_X_raster_row */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static void (*do_reclass_type [3])() = {do_reclass_int,
                                          do_reclass_float,
                                          do_reclass_double};
#define DO_RECLASS (do_reclass_type [F2I (data_type)])


int 
G_get_map_row_nomask (int fd, CELL *cell, int row)

{
  int stat;

  stat = get_map_row_nomask (fd, (void *) cell, row, CELL_TYPE);
  if(stat >= 0)
  {
      stat = embed_nulls_nomask(fd, (void *) cell, row, CELL_TYPE, 1);
      if (FCB.reclass_flag) do_reclass_int(fd, cell, 1);
  }
  return stat;
}

/*--------------------------------------------------------------------------*/

int 
G_get_raster_row_nomask (int fd, void *rast, int row, RASTER_MAP_TYPE data_type)
{
  int stat, i;

  if(data_type == CELL_TYPE)
     return G_get_c_raster_row_nomask (fd, (CELL *) rast, row);

  /* if the map is reclass table, use G_get_c_raster_row() to get and
  reclass CELL row and copy results to needed type  */

  if(FCB.reclass_flag) 
  {
     stat = G_get_c_raster_row_nomask(fd, G__.mask_buf, row);
     if(stat<0) return stat;
     for(i=0; i<WINDOW.cols; i++)
	G_set_raster_value_c(rast, G__.mask_buf[i], data_type);
     /* nulls are already embedded */
     return stat;
  }
  stat = get_map_row_nomask (fd, rast, row, data_type);
  if(stat >= 0)
     return embed_nulls_nomask(fd, rast, row, data_type, 0);

  /* here if later fp reclass is implemented, if map is a reclass of
     a fp map (FCB.fpreclass_flag) DO_RECLASS(fd, rast, 1) */

  else return stat;
}

/*--------------------------------------------------------------------------*/

int 
G_get_c_raster_row_nomask (int fd, CELL *cell, int row)
{
  int stat;

  stat = get_map_row_nomask (fd, (void *) cell, row, CELL_TYPE);
  if(stat >= 0)
  {
      stat = embed_nulls_nomask(fd, (void *) cell, row, CELL_TYPE, 0);
      if (FCB.reclass_flag) do_reclass_int(fd, cell, 0);
  }
  return stat;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
int 
G_get_f_raster_row_nomask (int fd, FCELL *fcell, int row)

{
     return G_get_raster_row_nomask(fd, (void *) fcell, row, FCELL_TYPE);
}

/*--------------------------------------------------------------------------*/

int 
G_get_d_raster_row_nomask (int fd, DCELL *dcell, int row)

{
     return G_get_raster_row_nomask(fd, (void *) dcell, row, DCELL_TYPE);
}

/*--------------------------------------------------------------------------*/

int 
G_get_map_row (int fd, CELL *cell, int row)

{
  int stat;

  stat = get_map_row_nomask (fd, (void *) cell, row, CELL_TYPE);
  if(stat >= 0)
  {
      stat = embed_nulls_nomask(fd, (void *) cell, row, CELL_TYPE, 1);
      if (FCB.reclass_flag) do_reclass_int(fd, cell, 1);
  }
  return stat;
}

/*--------------------------------------------------------------------------*/

int 
G_get_raster_row (int fd, void *rast, int row, RASTER_MAP_TYPE data_type)

{
  int stat, i;

  if(data_type == CELL_TYPE)
     return G_get_c_raster_row (fd, (CELL *) rast, row);

  /* if the map is reclass table, use G_get_c_raster_row() to get and
  reclass CELL row and copy results to needed type  */

  if(FCB.reclass_flag) 
  {
     stat = G_get_c_raster_row(fd, G__.mask_buf, row);
     if(stat<0) return stat;
     for(i=0; i<WINDOW.cols; i++)
	G_set_raster_value_c(rast, G__.mask_buf[i], data_type);
     /* nulls are already embedded */
     return stat;
  }
  stat = get_map_row_nomask (fd, rast, row, data_type);
  if(stat >= 0)
     return embed_nulls(fd, rast, row, data_type, 0);

  /* here if later fp reclass is implemented, if map is a reclass of
     a fp map (FCB.fpreclass_flag) DO_RECLASS(fd, rast, 1) */

  else return stat;
}

/*--------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------*/

int 
G_get_c_raster_row (int fd, CELL *cell, int row)

{
  int stat;

  stat = get_map_row_nomask (fd, (void *) cell, row, CELL_TYPE);
  if(stat >= 0)
  {
      stat = embed_nulls(fd, (void *) cell, row, CELL_TYPE, 0);
      if (FCB.reclass_flag) do_reclass_int(fd, cell, 0);
  }
  return stat;
}
/*--------------------------------------------------------------------------*/

int G_get_f_raster_row (int fd, FCELL *buf, int row)
{
  return G_get_raster_row(fd, (void *) buf, row, FCELL_TYPE);
}
/*--------------------------------------------------------------------------*/

int G_get_d_raster_row (int fd, DCELL *buf, int row)
{
  return G_get_raster_row(fd, (void *) buf, row, DCELL_TYPE);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

int 
G_get_null_value_row (int fd, char *flags, int row)
{
  register CELL *mask;
  register int n, stat;

  stat = G_get_null_value_row_nomask (fd, flags, row);

  if (stat < 0) return (stat);

  if (AUTO_MASKING > 0) 
  {
     if (get_map_row_nomask (MASK_FD, (void *) (mask = MASK_BUF), row, CELL_TYPE) >= 0) 
     {
	   if (G__.fileinfo[MASK_FD].reclass_flag) 
			    do_reclass_int(MASK_FD, mask, 1);
           n = WINDOW.cols;
           while (n-- > 0) 
           {
                if (*mask++ == 0)
                    *flags = 1;
                flags++;
           }
     }
  }
  return 1;
}
    
/*--------------------------------------------------------------------------*/


int 
G_get_null_value_row_nomask (int fd, char *flags, int row)

{ 
   int i, j, null_fd;

   if(row > WINDOW.rows || row < 0)   
   {
       G_warning ("[%s in %s] - read request for row %d is outside region",
	     FCB.name, FCB.mapset, row);
   }
          
   if ((MIN_NULL_ROW > row) || (MIN_NULL_ROW + NULL_ROWS_INMEM -1 < row))
   /* the null row row is not in memory */ 
   {
      /* read in NULL_ROWS_INMEM rows from null file 
         so that the requested row is between MIN_NULL_ROW
         and MIN_NULL_ROW + NULL_ROWS_INMEM */

      MIN_NULL_ROW = (row / NULL_ROWS_INMEM) * NULL_ROWS_INMEM;

      null_fd = G__open_null_read(fd);

      for (i= 0; i < NULL_ROWS_INMEM ; i++)
      {
         /* WINDOW.rows doesn't have to be a multiple of NULL_ROWS_INMEM */
         if(i+MIN_NULL_ROW >= WINDOW.rows)
               break;


         if( G__read_null_bits(null_fd, FCB.null_work_buf, 
                        i+MIN_NULL_ROW, FCB.cellhd.cols, fd) < 0)
         {
	    if(FCB.map_type == CELL_TYPE)
	    {
	       /*
               If can't read null row, assume  that all map 0's are nulls 
	       use allocated G__.mask_buf to read map row */
	       get_map_row_nomask (fd, (void *) G__.mask_buf, i+MIN_NULL_ROW, 
					    CELL_TYPE);
               for(j=0;j<WINDOW.cols;j++)
               {
                  if(G__.mask_buf[j] == 0)
                      flags[j] = 1;
                  else
                      flags[j] = 0;
               }
	    }
	    else /* fp map */
	    {
               /* if can't read null row, assume  that all data is valid */ 
	       G_zero(flags, sizeof(char) * WINDOW.cols);
               /* the flags row is ready now */
	    }
         } /*if no null file */
         else
         {
         /* copy null row to flags row translated by window column mapping */
         /* the FCB.NULL_ROWS[row-MIN_NULL_ROW] has WINDOW.cols bits, */
         /* the FCB.null_work_buf has size FCB.cellhd.cols */
            for(j=0;j<WINDOW.cols;j++)
            {
                 if(!CMAP[j])
                    flags[j] = 1;
                 else
                    flags[j] = G__check_null_bit( FCB.null_work_buf,
                                         CMAP[j]-1, FCB.cellhd.cols);
            }
         }
         /* remember the null row for i for the future reference */
	 
	 /*bf-We should take of the size - or we get 
	 zeros running on their own after flags convertions -A.Sh.*/
	 FCB.NULL_ROWS[i] = (unsigned char *) realloc(FCB.NULL_ROWS[i],
	 	sizeof(unsigned char)*G__null_bitstream_size(WINDOW.cols)+1);
		if (FCB.NULL_ROWS[i] == NULL) G_fatal_error ("Could not realloc buffer");
		
         G__convert_01_flags(flags, FCB.NULL_ROWS[i], WINDOW.cols);

     }  /* for loop */

     if(null_fd > 0)
            close(null_fd);
  } /* row is not in memory */

/* copy null file data translated by column mapping to user null row */
/* the user requested flags row is of size WINDOW.cols */
   G__convert_flags_01(flags, FCB.NULL_ROWS[row - MIN_NULL_ROW], WINDOW.cols);

   return 1;
}

int 
G__open_null_read (int fd)
{
   int null_fd;
   char dir_name[200];
   static char *name=NULL, *mapset=NULL;

   if(NULL_FILE_EXISTS == 0) return -1;

   if (FCB.reclass_flag)
   {
      name = FCB.reclass.name ;
      mapset = FCB.reclass.mapset ;
   }
   else
   {
      name = FCB.name;
      mapset = FCB.mapset;
   }
			 
   sprintf(dir_name, "cell_misc/%s", name);
    
   if (G_find_file(dir_name, NULL_FILE, mapset)==NULL)
   {
/*
      G_warning ("unable to find [%s]",path);
*/
      NULL_FILE_EXISTS = 0;
      return -1;
   }

   null_fd = G_open_old (dir_name, NULL_FILE, mapset);
   if (null_fd < 0)
       return -1;

   if (null_fd >= MAXFILES)
   {
       close (null_fd);
       G_warning("Too many open raster files");
       return -1;
   }
   NULL_FILE_EXISTS = 1;
   return null_fd;
}

int 
G__read_null_bits (int null_fd, unsigned char *flags, int row, int cols, int fd)
{
   long offset;
   int size, R;
   char msg[200];

   if (compute_window_row (fd, row, &R) <= 0) 
   {
      /* write a row of nulls */
      G__init_null_bits(flags, cols);
      return 1;
   }

   if(null_fd < 0) return -1;
   /* the null file doesn't exist */

   size = G__null_bitstream_size(cols);
   offset = (long) (size * R * sizeof(unsigned char)) ;
   if (lseek (null_fd, offset, 0) < 0)
   {
       G_warning("error reading null row %d\n",R);
       return -1;
   }
   if (read (null_fd, flags, size) != size)
   {
       G_warning("error reading null row %d\n",R);
       return -1;
   }
   return 1;
}

int embed_nulls_nomask (int fd, void *buf, int row,
    RASTER_MAP_TYPE map_type, int null_is_zero)
{
    int i;

    /* this is because without null file the nulls can be only due to 0's
       in data row or mask */
    if(null_is_zero && !NULL_FILE_EXISTS) return 1;
    if(G_get_null_value_row_nomask(fd, NULL_BUF, row) < 0) 
          return -1;
    for(i = 0; i < WINDOW.cols; i++)
    {
      /* also check for nulls which might be already embedded by quant
      rules in case of fp map. */
      if(NULL_BUF[i] || G_is_null_value(buf, map_type))
      {
	 /* G__set_[f/d_]null_value() sets it to 0 is the embedded mode
	    is not set and calls G_set_[f/d_]null_value() otherwise */
	    G__set_null_value(buf, 1, null_is_zero, map_type);
      }
      buf = G_incr_void_ptr(buf, G_raster_size(map_type));
    }
    return 1;
}

int 
embed_nulls (int fd, void *buf, int row, RASTER_MAP_TYPE map_type, int null_is_zero)
{
    int i;

    /* this is because without null file the nulls can be only due to 0's
       in data row or mask */
    if(AUTO_MASKING<=0 && null_is_zero && !NULL_FILE_EXISTS) return 1;
    if(G_get_null_value_row(fd, NULL_BUF, row) < 0) 
          return -1;
    for(i = 0; i < WINDOW.cols; i++)
    {
      /* also check for nulls which might be already embedded by quant
      rules in case of fp map. */
      if(NULL_BUF[i] || G_is_null_value(buf, map_type))
      {
	 /* G__set_[f/d_]null_value() sets it to 0 is the embedded mode
	    is not set and calls G_set_[f/d_]null_value() otherwise */
         G__set_null_value(buf, 1, null_is_zero, map_type);
      }
      buf = G_incr_void_ptr(buf, G_raster_size(map_type));
    }
    return 1;
}
