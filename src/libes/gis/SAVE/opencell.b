/**********************************************************
 *
 *  G_open_cell_old (name, mapset)
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 *  opens the existing cell file 'name' in the 'mapset'
 *  for reading by G_get_map_row()
 *  with mapping into the current window
 *
 *  returns: open file descriptor ( >= 0) if successful
 *           negative integer if error
 *
 *  diagnostics: warning message printed if open fails
 ***********************************************************
 *
 *  G_open_cell_new (name)
 *      char *name            map file name
 *
 *  opens a new cell file 'name' in the current mapset
 *  for writing by G_put_map_row()
 *
 *  the file is created and filled with no data
 *  it is assumed that the new cell file is to conform to
 *  the current window.
 *
 *  The file must be written sequentially.
 *   (Use G_open_cell_new_random() for non sequential writes)
 *
 *  note: the open actually creates a temporary file
 *        G_close_cell() will move the temporary file
 *        to the cell file and write out the necessary 
 *        support files (cellhd, cats, hist, etc.)
 *
 *  returns: open file descriptor ( >= 0) if successful
 *           negative integer if error
 *
 *  diagnostics: warning message printed if open fails
 *
 *  warning: calls to G_set_window() made after opening a new
 *           cell file may create confusion and should be avoided
 *           the new cell file will be created to conform
 *           to the window at the time of the open.
 ***********************************************************
 * G_set_cell_format(n)
 *
 * sets the format for subsequent opens on new cell files
 * (uncompressed and hrandom only)
 * warning - subsequent put_row calls will only write n+1 bytes
 *           per cell. If the data requires more, the cell file
 *           will be written incorrectly (but with n+1 bytes per cell)
 ***********************************************************
 * G_want_histogram(flag)
 *
 * If newly created cell files should have histograms, set flag=1
 * otherwise set flag=0. Applies to subsequent opens.
 *
 ***********************************************************/

#include "G.h"
#define FCB G__.fileinfo[fd]
#define WINDOW G__.window
#define DATA_NROWS  FCB.cellhd.rows
#define DATA_NCOLS  FCB.cellhd.cols

G_open_cell_old (name, mapset)
    char *name;
    char *mapset;
{
    int fd;

    if ((fd = G__open_cell_old (name, mapset)) < 0)
    {
        char msg[128];

        sprintf (msg, "unable to open data layer [%s] in mapset [%s]",
            name, mapset);
        G_warning (msg);
        return fd;
    }

/* turn on auto masking, if not already on */
    G__check_for_auto_masking();

    return fd;
}


/********************************************************************
 * G__open_cell_old (name, mapset)
 *
 * function:
 *   This is the work horse. It is used to open cell files, supercell
 *   files, and the MASK file. 
 *
 * parms:
 *   name, mapset    name and mapset of cell file to be opened.
 *
 * actions:
 *   opens the named cell file, following reclass reference if
 *     named layer is a reclass layer.
 *   creates the required mapping between the data and the window
 *     for use by the get_map_row family of routines.
 *
 * returns:
 *   open file descriptor or -1 if error.
 *
 * diagnostics:
 *   errors other than actual open failure will cause a diagnostic to be
 *   delivered thru G_warning() open failure messages are left to the
 *   calling routine since the masking logic will want to issue a different
 *   warning.
 *
 * note:
 *  this routine does NOT open the MASK layer. If it did we would get
 *  infinite recursion.  This routine is called to open the mask by
 *  G__check_for_auto_masking() which is called by G_open_cell().
 ***********************************************************************/
G__open_cell_old (name, mapset)
    char *name;
    char *mapset;
{
    int fd;
    char *r_name ;
    char *r_mapset ;
    struct Cell_head cellhd ;
    int nbytes;                   /* bytes per cell */
    int reclass_flag;
    struct Reclass reclass;


/* make sure window is set    */
    G__init_window ();

/* Check for reclassification */
    reclass_flag = G_get_reclass (name, mapset, &reclass) ;

    switch (reclass_flag)
    {
        case 0:
            r_name = name ;
            r_mapset = mapset ;
            break ;
        case 1:
            r_name = reclass.name ;
            r_mapset = reclass.mapset ;
	    if (G_find_cell (r_name, r_mapset) == NULL)
	    {
		char msg[200];
		sprintf (msg,
		    "unable to open [%s] in [%s] since it is a reclass of [%s] in [%s] which does not exist",
		    name,mapset,r_name,r_mapset);
		G_warning (msg);
		return -1;
	    }
            break ;
        default:           /* Error reading cellhd/reclass file */
            return -1 ;
    }

/* read the cell header */
    if(G_get_cellhd (r_name, r_mapset, &cellhd) < 0)
        return -1;
    nbytes = cellhd.format + 1;
    if (nbytes < 1)
    {
        char msg[100];
        sprintf (msg,
            "[%s] in mapset [%s] - format field in header file invalid",
            r_name, r_mapset);
        G_warning (msg);
        return -1;
    }

    if (cellhd.proj != G__.window.proj)
    {
        char msg[100];
        sprintf (msg,
            "[%s] in mapset [%s] - in different projection than current window",
            name, mapset);
        G_warning (msg);
        return -1;
    }
    if (cellhd.zone != G__.window.zone)
    {
        char msg[100];
        sprintf (msg,
            "[%s] in mapset [%s] - in different zone than current window",
            name, mapset);
        G_warning (msg);
        return -1;
    }

/* warn if too large cell size */
    if (nbytes > sizeof(CELL))
    {
        char msg[100];
        sprintf (msg,
            "[%s] in [%s] - bytes per cell (%d) too large",
            name, mapset, nbytes);
        G_warning (msg);
	return -1;
    }

    fd = G_open_old ("cell", r_name, r_mapset);
    if (fd < 0)
        return -1;

    if (fd >= MAXFILES)
    {
        close (fd);
        G_warning("Too many open cell files");
        return -1;
    }

/* mark closed */
    FCB.open_mode = -1;

/* save name and mapset */
    {
    char xname[256],xmapset[256];
    if (G__name_in_mapset(name, xname, xmapset))
	FCB.name   = G_store (xname);
    else
	FCB.name   = G_store (name);
    }
    FCB.mapset = G_store (mapset);

/* record number of bytes per cell */
    FCB.nbytes = nbytes;

/* mark no data row in memory  */
    FCB.cur_row = -1;

/* if reclass, copy reclass structure */
    if (FCB.reclass_flag = reclass_flag)
	G_copy (&FCB.reclass, &reclass, sizeof(reclass));

/* Save cell header */
    G_copy (&FCB.cellhd, &cellhd, sizeof(cellhd));

/* check for compressed data format, making initial reads if necessary */
    if(G__check_format (fd) < 0)
    {
        close (fd); /* warning issued by check_format() */
        return -1;
    }

/* create the mapping from cell file to window */
    G__create_window_mapping (fd);

/*
 * allocate the data buffer
 * number of bytes per cell is cellhd.format+1
 */
    FCB.data = (unsigned char *) G_calloc (FCB.cellhd.cols, FCB.nbytes);

/*
 * allocate/enlarge the compressed data buffer needed by get_map_row()
 */
    allocate_compress_buf (fd);

/* now mark open for read: this must follow create_window_mapping() */
    FCB.open_mode = OPEN_OLD;
    FCB.io_error = 0;

    return fd;
}

/*****************************************************************/

static int NBYTES = sizeof(CELL);


G_open_cell_new (name)
    char *name;
{
    return G__open_cell_new (name, OPEN_NEW_COMPRESSED);
}

G_open_cell_new_random (name)
    char *name;
{
    return G__open_cell_new (name, OPEN_NEW_RANDOM);
}

G_open_cell_new_uncompressed (name)
    char *name;
{
    return G__open_cell_new (name, OPEN_NEW_UNCOMPRESSED);
}

G_want_histogram(flag)
{
    G__.want_histogram = flag;
}

G_set_cell_format (n)
{
    NBYTES = n+1;
    if (NBYTES <= 0)
	NBYTES = 1;
    if (NBYTES > sizeof(CELL))
	NBYTES = sizeof(CELL);
}

G_cellvalue_format (v)
    CELL v;
{
    int i;
    if (v >= 0)
	for (i = 0; i < sizeof(CELL); i++)
	    if (!(v /= 256))
		return i;
    return sizeof(CELL)-1;
}

G__open_cell_new (name, open_mode)
    char *name;
{
    int fd;
    char *tempname;

/* check for legal grass name */
    if (G_legal_filename (name) < 0)
    {
	char msg[100];

	sprintf (msg, "opencell: %s - illegal cell file name", name);
	G_warning (msg);
	return -1;
    }

/* make sure window is set */
    G__init_window();

/* open a tempfile name */
    tempname = G_tempfile ();
    fd = creat (tempname, 0666);
    if (fd < 0)
    {   
        G_warning ("opencell: no temp files available");
        free (tempname);
        return -1;
    }

    if (fd >= MAXFILES)
    {
        free (tempname);
        close (fd);
        G_warning("opencell: too many open cell files");
        return -1;
    }

/*
 * since we are bypassing the normal open logic
 * must create the cell element 
 */
    G__make_mapset_element ("cell");

/* mark closed */
    FCB.open_mode = -1;
    FCB.data = 0;


/*
 * copy current window into cell header
 * set format to cell/supercell
 * for compressed writing
 *   allocate space to hold the row address array
 *   allocate/enlarge both the compress_buf and the work_buf
 */
    G_copy (&FCB.cellhd, &WINDOW, sizeof (FCB.cellhd));
    if (open_mode == OPEN_NEW_COMPRESSED)
    {
	FCB.row_ptr = (long *) G_calloc(DATA_NROWS + 1, sizeof(long)) ;
	G_zero(FCB.row_ptr,(DATA_NROWS + 1) * sizeof(long)) ;
	G__write_row_ptrs (fd);
	FCB.cellhd.compressed = 1;

	allocate_compress_buf(fd);
	FCB.nbytes = 1;		/* to the minimum */
    }
    else
    {
	FCB.nbytes = NBYTES ;
	FCB.cellhd.compressed = 0;
	if (open_mode == OPEN_NEW_RANDOM)
	    G_write_zeros (fd, (long) NBYTES * DATA_NCOLS * DATA_NROWS);
    }
    allocate_work_buf(fd);

/* save name and mapset, and tempfile name */
    FCB.name      = G_store (name);
    FCB.mapset    = G_store (G_mapset());
    FCB.temp_name = tempname;

/* next row to be written (in order) is zero */
    FCB.cur_row = 0;

/* init cell stats */
    if (FCB.want_histogram = G__.want_histogram)
	G_init_cell_stats (&FCB.statf);

    G_init_range (&FCB.range);

/* mark file as open for write */
    FCB.open_mode = open_mode;
    FCB.io_error = 0;

    return fd;
}
/*
 * allocate/enlarge the compressed data buffer needed by get_map_row()
 * and put_map_row()
 * note: compressed format is repeat, value:
 *  repeat takes 1 byte, value takes up to sizeof(CELL)
 *  plus 1 byte header for nbytes needed to store row
 */
static
allocate_compress_buf(fd)
{
    int n;
    n = FCB.cellhd.cols * (sizeof(CELL) + 1) + 1;
    if (FCB.cellhd.compressed && (n > G__.compressed_buf_size))
    {
        if (G__.compressed_buf_size <= 0)
            G__.compressed_buf = (unsigned char *) G_malloc (n);
        else
            G__.compressed_buf = (unsigned char *) G_realloc(G__.compressed_buf,n);
        G__.compressed_buf_size  = n;
    }
}
/*
 * allocate/enlarge the work data buffer needed by put_map_row()
 */
static
allocate_work_buf(fd)
{
    int n;
    n = FCB.cellhd.cols * (sizeof(CELL) + 1) + 1;
    if (n > G__.work_buf_size)
    {
        if (G__.work_buf_size <= 0)
            G__.work_buf = (unsigned char *) G_malloc (n);
        else
            G__.work_buf = (unsigned char *) G_realloc(G__.work_buf,n);
        G__.work_buf_size  = n;
    }
}
