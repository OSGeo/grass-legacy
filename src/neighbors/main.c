/* %W% %G% */
#include "gis.h"
#define MAIN
#include "ncb.h"
#include "method.h"

main(argc,argv)	char *argv[];
{
    int method;
    int old;
    int new;
    CELL *rp;
    CELL *result;
    int row, col;
    int readrow;
    int nrows, ncols;
    int n;
    int i;
    cfunc newvalue;
    ifunc cat_names;
    struct Colors colr;
    struct Cell_head cellhd;
    struct Cell_head window;

    CELL *values;	/* list of neighborhood values */

/* get user for mail message and pgm name */
    G_gisinit ("neighbors");

/* explain program to user */
    explain();

/* ask for the old cell file name */

    ncb.oldcell.mapset = G_ask_cell_old("", ncb.oldcell.name);
    if (!ncb.oldcell.mapset)
	    exit(0);

/* warn user if window doesn't agree with cell header */
    if (G_get_cellhd (ncb.oldcell.name, ncb.oldcell.mapset, &cellhd) >= 0)
    {
	G_get_window (&window);
	if (window.ns_res != cellhd.ns_res || window.ew_res != cellhd.ew_res)
	{
	    printf ("The current window has a different resolution than [%s]\n",
		ncb.oldcell.name);
	    if (!G_yes ("Proceed anyway? ", -1))
		exit(0);
	}
    }
    nrows = G_window_rows();
    ncols = G_window_cols();

/* open cell file */
    if ((old = G_open_cell_old (ncb.oldcell.name, ncb.oldcell.mapset)) < 0)
    {
	char msg[200];
	sprintf(msg,"can't open cell file <%s> in mapset %s\n",
		ncb.oldcell.name, ncb.oldcell.mapset);
	G_fatal_error (msg);
	exit(-1);
    }

/* ask for new cell file name */

    ncb.newcell.mapset = G_ask_cell_new ("", ncb.newcell.name);
    if (!ncb.newcell.mapset)
	    exit(0);

/* get the method and the neighborhood size */
    while((method = ask_method()) < 0)
	    ;
    ncb.nsize = ask_nsize();
    ncb.dist = ncb.nsize/2;

/* allocate the cell buffers */
    allocate_bufs ();
    values = (CELL *) G_malloc (ncb.nsize * ncb.nsize * sizeof (CELL));
    result = G_allocate_cell_buf ();


/* get title, initialize the category and stat info */

    title (menu[method].title);

/* initialize the cell bufs with 'dist' rows of the old cellfile */

    readrow = 0;
    for (row = 0; row < ncb.dist; row++)
	readcell (old, readrow++, nrows, ncols);

/* establish the newvalue routine */
    newvalue = menu[method].method;

/* go into background */
    printf("you will be notified by mail when %s is complete\n", G_program_name());

    if (G_fork()) exit(0);
    freopen ("/dev/null", "w", stderr);

/*
 * open the new cellfile now
 * must occur after the fork, since pid has changed and this affects
 * tempfiles used for creating new cell files
 */

    new = G_open_cell_new (ncb.newcell.name);
    if (new < 0)
	exit(1);

    ncb.changed = 0;
    for (row = 0; row < nrows; row++)
    {
	readcell (old, readrow++, nrows, ncols);
	ncb.center = ncb.buf[ncb.dist] + ncb.dist;
	rp = result;
	for (col = 0; col < ncols; col++)
	{
	    n = gather (values, col);
	    *rp++ = newvalue (values, n);
	    ncb.center++;
	}
	G_put_map_row (new, result);
    }
    G_close_cell (new);
    G_close_cell (old);

/* put out category info */
    null_cats () ;
    if (cat_names = menu[method].cat_names)
	    cat_names();
    G_set_cat (0, "no data", &ncb.cats);
    G_write_cats (ncb.newcell.name, &ncb.cats);

/* also copy color table */
    G_suppress_warnings (1);
    if(menu[method].copycolr &&
      (G_read_colors (ncb.oldcell.name, ncb.oldcell.mapset, &colr) > 0))
	G_write_colors (ncb.newcell.name, ncb.newcell.mapset, &colr);
    G_suppress_warnings (0);

    complete ();
}
