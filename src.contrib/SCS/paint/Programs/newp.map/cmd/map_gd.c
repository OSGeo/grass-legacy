#include "gis.h"
#include "dlg.h"
#include "graphics.h"
#include "parms.h"
#include "vector.h"
#include "misc.h"
#include "fullwindow.h"
#include "colormode.h"
#include "vectrailer.h"

#define MARGIN 2

map(window)
    struct Cell_head *window;   /* current window */
{
    struct Cell_stats statf;
    int next_raster_row;
    int rasrow;
	int	done_legends	= 0;
    unsigned char *rasptr;

    char *date;

    int extra_lines;       /* number of raster lines needed
			    * to paint vector info at end of map
			    */
    CELL *cell;	/* buffer for reading cell file	*/
    CELL *cptr;
    CELL *olcell1;	/* buffers for reading outline cell file	*/
    CELL *olcell2;
    CELL *maskcell;	/* buffer for reading MASK file	*/

    int prows, pcols;
    int max;		/* max number of printer pixels	*/
    int pixels;
    int npix;		/* # of pixels remaining in panel */
    int row;
    int col;
    int scol;
    int count;

    unsigned char black;    /* black catagory number      */
    unsigned char white;    /* white catagory number      */
    int npanels;  /* number of panels in map    */
    int panel;    /* current panel being printed */
    int i;
    unsigned char *red, *blu, *grn, *set;
    short *red_carry_below;	/* color carryovers */
    short *grn_carry_below;
    short *blu_carry_below;
    short red_carry_right;
    short grn_carry_right;
    short blu_carry_right;

    int move_abs(), cont_abs();
	char *catname = "Test";


/************************ begin *****************************/

/* get date */

    date = G_date();
/* build color tables needed for spreading the color around */
    build_color_tables();
    if (colormode == COLORMODE_DITHER)
	build_dither_tables();

/* Determine max number of pixels
 * allow for a border at left and right edges of panels
 */

    Pnpixels (&prows, &pcols);
    init_graphics (pcols);

printf (" pcols  is %d \n", pcols);
printf (" prows is %d \n", prows);

	pcols	= pcols - 100;
	prows = prows - 100;

    max = pcols;
    max -= MARGIN;
	done_legends	= 0;

    if (parms.cellfd >= 0)
    {
	red = (unsigned char *)G_malloc (pcols);
	grn = (unsigned char *)G_malloc (pcols);
	blu = (unsigned char *)G_malloc (pcols);
	set = (unsigned char *)G_malloc (pcols);
	if (colormode == COLORMODE_DIFFUSION)
	{
	    red_carry_below = (short *)G_malloc (pcols * sizeof (short));
	    grn_carry_below = (short *)G_malloc (pcols * sizeof (short));
	    blu_carry_below = (short *)G_malloc (pcols * sizeof (short));
	}
    }


    white = WHITE;
    black = BLACK;

    cell = NULL ;
    olcell1 = NULL ;
    olcell2 = NULL ;
    maskcell = NULL ;


/*
 * adjust the window based on the scale
 */

/*
	window->north	= 5465400.00;
	window->west	= 700000.70; 
	window->east	= 715000.27;
	window->south	= 5451000.00;
	*/

    if (prows)
	unscaled (window, prows-MARGIN, max);
    else
	scale (window, max+MARGIN, MARGIN, parms.scaletext);

printf (" window->rows is %d\n", window->rows);
printf (" window->cols is %d\n", window->cols);


    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;

/* determine how many panels this map will require */

    npanels = (fullwindow.cols + max - 1) / max;

/*
 * determine the number of raster rows for the vector lines in the legend
 * the lines will drawn at the same width as in the map (plus lines of white
 * before and after)
 */
    extra_lines = 0;
    if (prows == 0)
    {
	for (i=0; i < vector.count; i++)
	    extra_lines += vector_info_lines(i);
	extra_lines += 2;
    }

/* initialize the stats */
    G_init_cell_stats (&statf);
    if (parms.cellfd >= 0)
    {
	if (verbose) fprintf (stderr, "Gathering map stats ...");
	cell = G_allocate_cell_buf();
	for (row = 0; row < fullwindow.rows; row++)
	{
	    if (verbose) G_percent (row, fullwindow.rows, 2);
	    G_get_map_row (parms.cellfd, cell, row);
	    G_update_cell_stats (cell, fullwindow.cols, &statf);
		getcats();
	}
	if (verbose) G_percent (row, fullwindow.rows, 2);
    }

/* begin! */

    G_setup_plot ((double) 0.0,
		  (double) fullwindow.rows,
		  (double) 0.0,
		  (double) fullwindow.cols,
		  move_abs, cont_abs);

/* print each panel */

    npix = fullwindow.cols;
    scol = 0;
    for (panel = 1; panel <= npanels; panel++)
    {
	if (parms.endpanel > 0 && panel > parms.endpanel)
	    break;
	if (verbose > 1)
	    printf("PAINT: panel %d (of %d)\n", panel, npanels);

/* compute number of pixels in this panel */

	pixels = npix > max ? max : npix;

/* adjust the east of the window for the number of cols */
	window->cols = pixels;
	window->east = window->west + window->ew_res * pixels;

	if (parms.startpanel > 0 && panel < parms.startpanel)
	    goto nextpanel;
	G_set_window (window);
	if (G_window_cols() != pixels) fprintf (stderr, "OOPS: window cols changed!!\n");

	if (cell)
	    free (cell);
	cell = G_allocate_cell_buf ();
	if (maskcell)
	    free (maskcell);
	maskcell = G_allocate_cell_buf ();

	if (parms.outlinefd >= 0)
	{
	    if (olcell1)
		free (olcell1);
	    if (olcell2)
		free (olcell2);
	    olcell1 = G_allocate_cell_buf ();
	    olcell2 = G_allocate_cell_buf ();
	    G_get_map_row (parms.outlinefd, olcell1, 0);
	}
	if (parms.cellfd >= 0)
	{
	    if (colormode == COLORMODE_DIFFUSION)
	    {
		G_zero (red_carry_below, window->cols * sizeof (short)); 
		G_zero (grn_carry_below, window->cols * sizeof (short)); 
		G_zero (blu_carry_below, window->cols * sizeof (short)); 
	    }
	}

/* put printer in raster mode, define picture */

	Pflush () ;
	Praster ();
	Ppictsize (fullwindow.rows+extra_lines, pixels+MARGIN);
	Pflush () ;

/* one white line to start */
	if (prows == 0)
	{
	    Prle_begin();
	    Prle (white, pixels+MARGIN);
	    Prle_end();
	    Pflush ();
	}

/* some title info */

	if (prows == 0)
	{
	    Palpha ();
	    header (panel, npanels, date);
	    title (parms.celltitle);
	    Praster ();
	    Pflush () ;
	}



/* print top border */

	Prle_begin();
	Prle (black, pixels+MARGIN);
	Prle_end();

/* print each row of the map */

	next_raster_row = -1;
	for (row = 0; row < fullwindow.rows; row++)
	{
	    if (colormode == COLORMODE_DIFFUSION)
	    {
		red_carry_right = 0;
		grn_carry_right = 0;
		blu_carry_right = 0;
	    }

	    if (row >= next_raster_row)	/* must clear raster area */
	    {
		next_raster_row = set_graphics (window, row, scol);

		pre_legend(&parms.pcats, &statf);
		do_newgrid();
	    mask_grid();
		do_vectors (0);
		do_plfile (0);
		do_region(0);
		mask_vectors (maskcell);
		do_vectors (1);
		do_plfile (1);
		do_sites();
		do_legends();
		do_barscales();
		do_region(1);




		do_labels (0) ;
		if (parms.grid_numbers > 0)
		    do_grid_numbers (panel);
		do_labels (1);
        if (parms.cellfd >= 0) {
			do_cats (&parms.pcolr);
			do_ramps(&parms.pcolr,&parms.pcats, &statf);
	
		}



		rasrow = 0 ;
	    }
	    rasptr = graphics.raster[rasrow++] ;

	    if (parms.cellfd >= 0)
	    {
		G_get_map_row (parms.cellfd, cptr = cell, row);
		G_lookup_colors(cell, red, grn, blu, set, pixels, &parms.pcolr);
		if (colormode == COLORMODE_DITHER)
		{
		    red_dither (red, row, /*scol*/ 0, pixels);
		    grn_dither (grn, row, /*scol*/ 0, pixels);
		    blu_dither (blu, row, /*scol*/ 0, pixels);
		}
	    }
	    if (parms.outlinefd >= 0)
	    {
		CELL *temp;

		temp = olcell1;
		olcell1 = olcell2;
		olcell2 = temp;
		G_get_map_row (parms.outlinefd, olcell1, row);
	    }

	    Prle_begin ();
	    Prle (black, 1);	/* left border */

	    col = scol;
	    for (count = 0; count < pixels; count++)
	    {
		unsigned char c;
		unsigned char ras;
		int n,r,g,b;

		c = white;
		if (parms.cellfd >= 0)
		{
		    r = red[count];
		    g = grn[count];
		    b = blu[count];
		    if (colormode == COLORMODE_DIFFUSION)
		    {
			r += red_carry_right + red_carry_below[count];
			g += grn_carry_right + grn_carry_below[count];
			b += blu_carry_right + blu_carry_below[count];
			red_carry_below[count] = red_carry_right = red_carryover(r)/2;
			grn_carry_below[count] = grn_carry_right = grn_carryover(g)/2;
			blu_carry_below[count] = blu_carry_right = blu_carryover(b)/2;
		    }

		    n = lookup_from_pattern(*cptr++,row,col++) ;
		    if (n < 0)
			n = printer_color_number (r,g,b);
		    c = n;
		}
		if (ras = *rasptr++)
		    c = ras - 1;
		else if (parms.outlinefd >= 0)
		{
		    if (olcell1[count] != olcell2[count])
			c = parms.outline_color;
		    else if (count && (olcell1[count] != olcell1[count-1]))
			c = parms.outline_color;
		}
		Prle (c, 1);
	    }

	    Prle (black, 1);	/* right border */

	    Prle_end () ;
	}


/* print bottom border */

	Prle_begin();
	Prle (black, pixels+MARGIN);
	Prle_end();

/* trailer info */

	if (with_trailer || with_vectinfo) {
	if (prows == 0)
	{
	    Pflush () ;
	    Palpha  ();
		if (with_trailer)
	    trailer (window, parms.grid, parms.scaletext);

		if (with_vectinfo)
	    vector_info (pixels+MARGIN);

	    Praster ();
	    Pflush () ;
	}
	}

/* a trailing white line */
	if (prows == 0)
	{
	    Prle_begin();
	    Prle (white, pixels+MARGIN);
	    Prle_end();
	}

nextpanel:
	npix -= pixels;
	scol += pixels;
	window->west = window->east;
	Pflush () ;
    }

/* if unscaled, skip what follows */
    if (prows)
    {
	if (verbose > 1)
	    printf("PAINT: complete\n");
	return;
    }

/* do the color table */

    if (parms.with_colortable)
    {
	if (verbose > 1)
	    printf("PAINT: printing color table\n");
	ctable (&parms.pcats, &parms.pcolr, &statf);
    }


/* comments */
    if (parms.commentfile != NULL)
    {
	FILE *fd;
	char line[1024];

printf (" in commentfile \n");
	if (verbose > 1)
	    printf("PAINT: printing comments\n");

	Palpha ();

	if (fd = fopen (parms.commentfile, "r"))
	{
	    *line = 0;
	    for (i = 0; i < 100; i++)
		strcat (line,"-");
	    Ptext (line);
	    while (G_getl (line, sizeof line, fd))
		Ptext (line);
	    for (i = 0; i < 5; i++)
		Ptext ("");
	    fclose (fd);
	}
	else
	    error ("comment file","","can't open");
    }
    if (verbose > 1)
	printf("PAINT: complete\n");
}
static
dump(window)
    struct Cell_head *window;
{
    struct Cell_head cur;

    printf ("window----------------\n");
    G__write_Cell_head (stdout, window, 0);
    printf ("cur-------------------\n");
    G_get_set_window (&cur);
    G__write_Cell_head (stdout, &cur, 0);
}
