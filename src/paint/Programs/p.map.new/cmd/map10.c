#include <string.h>
#include "gis.h"
#include "dlg.h"
#include "graphics.h"
#include "parms.h"
#include "vector.h"
#include "misc.h"
#include "Paintlib.h"
#include "fullwindow.h"
#include "colormode.h"
#include "vectrailer.h"
#include "sgrid.h" 
#include "vtext.h"
#include "local_proto.h"


#define MARGIN 2
static int dump (struct Cell_head *);

int 
map (
    struct Cell_head *window   /* current window */
)
{
    struct Cell_stats statf;
    int next_raster_row;
    int rasrow;
    unsigned char *rasptr;

    char *date;

    int extra_lines;       /* number of raster lines needed
			    * to paint vector info at end of map
			    */
    void *rast;	/* buffer for reading cell file	*/
    CELL *cell;	/* buffer for reading cell file	*/
    void *cptr;
    void *olcell1;	/* buffers for reading outline cell file	*/
    void *olcell2;
    RASTER_MAP_TYPE map_type, outl_map_type;
    int raster_size, outl_raster_size;
    void *tr,*bl, *br;
    CELL *maskcell;	/* buffer for reading MASK file	*/

    int prows, pcols;
    int max;		/* max number of printer pixels	*/
    int pixels;
	int subpixels;
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


/************************ begin *****************************/

    outl_map_type = map_type = parms.map_type;
    outl_raster_size = raster_size = G_raster_size(map_type);

/* get date */
    date = G_date();

/* build color tables needed for spreading the color around */
    build_color_tables();
    if (colormode == COLORMODE_DITHER)
	build_dither_tables();

/* Determine max number of pixels
 * allow for a border at left and right edges of panels
 */


/*
	i_e = 698685.30;
	i_n = 5464264.00;
	*/

    Pnpixels (&prows, &pcols);
    MAXCOLOR = Pncolors();
    /* WARNING this needs to be changed if the new driver has 255 colors
       the recomended change is to keep array Colors_Used[256], and
       var UNUSED_COLOR which function set_color() should set. */
    init_graphics (pcols);

	pcols_sp	= 0;;
	prows_sp 	= 0;;
	mrows		= 0;
	mcols		= 0;
	drows		= 0;
	dcols		= 0;
	m1panels	= 0;
	m_set_grid	= 0;
	set_grid_on = 0;

	pcols	= pcols ;
	prows = prows  ;

    max = pcols;
    max -= MARGIN;

    if (prows)
	unscaled (window, prows-MARGIN, max);
    else
	scale (window, max+MARGIN, MARGIN, parms.scaletext);


    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;


	grows	= window->rows;

/* determine how many panels this map will require */

    npanels = (fullwindow.cols + max - 1) / max;
	m1panels	= npanels;

	if (set_grid_num) {
	pcols_sp	= 50;
	prows_sp 	=  18;
	}
	if (m1panels > 1 && set_grid_num) {
		set_grid_num 	= 0;
		mcols			= 50;
		mrows			= 20;
		drows		= 20;
		dcols		= 50;
		pcols_sp	= 0;
		prows_sp	= 0;
		set_grid_on = 1;

		}

/*
fprintf (stdout," mrows is %d\n", mrows);
fprintf (stdout," mcols is %d\n", mcols);
fprintf (stdout," pcols_sp is %d \n", pcols_sp);
fprintf (stdout," prows_sp is %d \n", prows_sp);
*/


    if (parms.cellfd >= 0)
    {
	red = (unsigned char *)G_malloc (pcols-pcols_sp);
	grn = (unsigned char *)G_malloc (pcols-pcols_sp);
	blu = (unsigned char *)G_malloc (pcols-pcols_sp);
	set = (unsigned char *)G_malloc (pcols-pcols_sp);
	if (colormode == COLORMODE_DIFFUSION)
	{
	    red_carry_below = (short *)G_malloc ((pcols-pcols_sp) * sizeof (short));
	    grn_carry_below = (short *)G_malloc ((pcols-pcols_sp) * sizeof (short));
	    blu_carry_below = (short *)G_malloc ((pcols-pcols_sp) * sizeof (short));
	}
    }


    white = WHITE;
    black = BLACK;

    cell = NULL ;
    olcell1 = NULL ;
    olcell2 = NULL ;
    maskcell = NULL ;
    rast = NULL;


/*
 * adjust the window based on the scale
 */




    if (prows)
	unscaled (window, prows-prows_sp-MARGIN, max-pcols_sp);
    else
	scale (window, max-prows_sp+MARGIN, MARGIN, parms.scaletext);



    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;

	grows	= grows- window->rows;;


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
    if (parms.cellfd >= 0 && parms.need_stats)
    {
	if (verbose) fprintf (stderr, "Gathering map stats ...");
	getcats();
	G_set_quant_rules(parms.cellfd, &(parms.pcats.q));
	cell = G_allocate_cell_buf();
	for (row =0 ; row < fullwindow.rows; row++)
	{
	    if (verbose) G_percent (row, fullwindow.rows, 2);
	    G_get_c_raster_row (parms.cellfd, cell, row);
	    G_update_cell_stats (cell, fullwindow.cols, &statf);
	}
	if (verbose) G_percent (row, fullwindow.rows, 2);
    }

/* begin! */

    G_setup_plot ((double) 0.0,
		  (double) fullwindow.rows,
		  (double) 0.0,
		  (double) fullwindow.cols,
		  move_abs, cont_abs);

    fprintf (stdout,"rows %d  cols %d\n", fullwindow.rows, fullwindow.cols);
/* print each panel */

    npix = fullwindow.cols;
    scol = 0;


    for (panel = 1; panel <= npanels; panel++)
    {

	if (parms.endpanel > 0 && panel > parms.endpanel)
	    break;
	if (verbose > 1)
	    fprintf (stdout,"PAINT: panel %d (of %d)\n", panel, npanels);

/* compute number of pixels in this panel */

	pixels = npix > (max-pcols_sp) ? (max-pcols_sp) : npix;
	subpixels = pixels;

/* adjust the east of the window for the number of cols */
	window->cols = pixels  ;
	window->east = window->west + window->ew_res * (pixels);

	if (parms.startpanel > 0 && panel < parms.startpanel)
	    goto nextpanel;
	G_set_window (window);
	if (G_window_cols() != pixels) fprintf (stderr, "OOPS: window cols changed!!\n");

	if (cell)
	    G_free (cell);
        if(rast)
	    G_free(rast);
	rast = G_allocate_raster_buf(map_type);
	if (maskcell)
	    G_free (maskcell);
	maskcell = G_allocate_cell_buf ();

	if (parms.outlinefd >= 0)
	{
	    if (olcell1)
		G_free (olcell1);
	    if (olcell2)
		G_free (olcell2);
	    olcell1 = G_allocate_raster_buf (outl_map_type);
	    olcell2 = G_allocate_raster_buf (outl_map_type);
	    G_get_raster_row (parms.outlinefd, olcell1, 0, outl_map_type);
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
    if (prows)
	unscaled (window, prows-MARGIN, max);
    else
	scale (window, max+MARGIN, MARGIN, parms.scaletext);



    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;

	pixels = fullwindow.cols >max ? max : fullwindow.cols;

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

	if (prows == 0 && with_rastertitle)
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
    if (prows)
	unscaled (window, prows-prows_sp-MARGIN, max-pcols_sp);
    else
	scale (window, max-prows_sp+MARGIN, MARGIN, parms.scaletext);



    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;



	next_raster_row = -1;
	/*
	for (row = 0; row < fullwindow.rows+prows_sp+mrows; row++)
	*/
	for (row = 0; row < fullwindow.rows+prows_sp; row++)
	{
	    if (colormode == COLORMODE_DIFFUSION)
	    {
		red_carry_right = 0;
		grn_carry_right = 0;
		blu_carry_right = 0;
	    }

	    if (row >= next_raster_row)	/* must clear raster area */
	    {
    if (prows)
	unscaled (window, prows-MARGIN, max);
    else
	scale (window, max+MARGIN, MARGIN, parms.scaletext);

    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;

		next_raster_row = set_graphics (window, row, scol);
    if (prows)
	unscaled (window, prows-prows_sp-MARGIN, max-pcols_sp);
    else
	scale (window, max-prows_sp+MARGIN, MARGIN, parms.scaletext);

    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;

    window->cols = subpixels  ;
    window->east = window->west + window->ew_res * (subpixels);
    G_set_window (window);




		do_vectors (0);
		do_region(0);
		do_plfile (0);
		mask_vectors (maskcell);
		do_vectors (1);
		do_sites();
	        pre_mask_grid();
		do_newgrid();
	        mask_grid();
		if (parms.grid_numbers > 0)
		    do_grid_numbers (panel);
		/*
		I modified it to draw labels right after
		drawing each object and modified do_labels
		to delete the labels from labels.other 
		after drawing them.
		The reason for this is that otherwise
		if all the lables are drawn together after
		all the objects, and some objects are on top
		of others, then the lable of the bottom object
		is shown on a top of the top object which is wrong,
			 Olga     
		*/
		do_plfile (1);
		do_labels (0) ;
		do_labels (1);
		do_region(1);
		do_labels (1);
		do_barscales();
		do_labels (1);
		pre_legend(&statf);
		do_legends();
		do_labels (1);
		do_labels (2); /* do the text instructions */


        if (parms.cellfd >= 0) {
			do_cats (&parms.pcolr);
			do_ramps(&statf);
	
		}



		rasrow = 0;
	    }

	if (row >= prows_sp+mrows && row < fullwindow.rows+prows_sp) {


		row= row-prows_sp;
    if (prows)
	unscaled (window, prows-prows_sp-MARGIN, max-pcols_sp);
    else
	scale (window, max-prows_sp+MARGIN, MARGIN, parms.scaletext);



    G_set_window (window);
    fullwindow.ns_res = window->ns_res;
    fullwindow.ew_res = window->ew_res;
    fullwindow.rows   = window->rows;
    fullwindow.cols   = window->cols;

	    rasptr = graphics.raster[rasrow++] ;

	    if (parms.cellfd >= 0)
	    {
		/* if(row==0 || row>80)*/
		G_get_raster_row (parms.cellfd, cptr = rast, row, map_type);
		/*if(row==0 || row>80)*/
		G_lookup_raster_colors(rast, red, grn, blu, set, subpixels, &parms.pcolr, map_type);
		if (colormode == COLORMODE_DITHER)
		{
		    red_dither (red, row, /*scol*/ 0, subpixels);
		    grn_dither (grn, row, /*scol*/ 0, subpixels);
		    blu_dither (blu, row, /*scol*/ 0, subpixels);
		}
	    }
	    if (parms.outlinefd >= 0)
	    {
		void *temp;

		temp = olcell1;
		olcell1 = olcell2;
		olcell2 = temp;
		G_get_raster_row (parms.outlinefd, olcell1, row, outl_map_type);
	    }

	    Prle_begin ();
	    Prle (black, 1);	/* left border */

	    col = scol;
	    for (count = 0; count < (pixels); count++)
		{
		unsigned char c;
		unsigned char ras;
		int n,r,g,b;

		c = white;
		if (parms.cellfd >= 0)
		{
		if (count >= pcols_sp) {

			count = count - pcols_sp;
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

		    if(map_type == CELL_TYPE)
		    {
			n = lookup_from_pattern(*((CELL *)cptr),row,col++) ;
		        cptr = G_incr_void_ptr(cptr, raster_size);
                    }
                    else
			n = -1;
		    if (n < 0)
			n = printer_color_number (r,g,b);
		    c = n;
		count = count + pcols_sp ;
			}
		}
		if (count <pcols_sp+mcols) {
		c = white;
		if (count == pcols_sp+mcols-1)
		/*
			Prle(black, 1);
			*/
			c = black;
		if (count < 2) 
			c = white ;
		else{ 
		if (ras = *rasptr++) 
			c = ras - 1;
		}
			Prle (c, 1);

	    }
		/*
		if (count < mcols) {
		c = white;
		if (ras = *rasptr++) 
		c = ras - 1;
		Prle(c, 1);
		}
		*/
		else {
		if (ras = *rasptr++) {
		    c = ras - 1;
			/*
			fprintf (stdout," in graphics raster \n");
			*/
			}
		else if (parms.outlinefd >= 0)
		{
		count = count - pcols_sp ;
		    if(count) 
		    bl = G_incr_void_ptr(olcell2, (count-1) * outl_raster_size);
		    br = G_incr_void_ptr(olcell2, count * outl_raster_size);
		    tr = G_incr_void_ptr(olcell1, count * outl_raster_size);
		    if( G_raster_cmp(tr, br, outl_map_type) != 0)
			c = parms.outline_color;
		    else if(count && G_raster_cmp(bl, br, outl_map_type) != 0)
			c = parms.outline_color;
		count = count + pcols_sp ;
		}
		/*
		fprintf (stdout,"count is %d \n", count);
		fprintf (stdout,"row is %d \n", row);
		fprintf (stdout," c is %d \n", c);
		*/
		Prle (c, 1);
	    }

		}

	    Prle (black, 1);	/* right border */

	    Prle_end () ;
		/*
	row = row + prows_sp + mrows;
	*/
	row = row + prows_sp ;
	}
	else {

		Prle_begin();
	    Prle (black, 1);	/* left border */
	    rasptr = graphics.raster[rasrow++] ;
	    for (count = 0; count < (pixels); count++)
		{
		unsigned char c;
		unsigned char ras;
		if (row == prows_sp+mrows-1 && count>=pcols_sp+mcols) {
			Prle (black, 1);
			}
		else {
		if (ras = *rasptr++)
		{
			c = ras - 1;
		Prle (c, 1);
		}
		else 
		Prle (white, 1);
		}
		}
	    Prle (black, 1);	/* right border */
	    Prle_end () ;
	}

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
		drows	= 20;
	if (panel ==  1) {
		dcols	= 0;
		mcols	= 0;
		}
    }

/* if unscaled, skip what follows */
    if (prows)
    {
	if (verbose > 1)
	    fprintf (stdout,"PAINT: complete\n");
	return 0;
    }

/* do the color table */

    if (parms.with_colortable)
    {
	if (verbose > 1)
	    fprintf (stdout,"PAINT: printing color table\n");
	ctable (&parms.pcats, &parms.pcolr, &statf);
    }


/* comments */
    if (parms.commentfile != NULL)
    {
	FILE *fd;
	char line[1024];

fprintf (stdout," in commentfile \n");
	if (verbose > 1)
	    fprintf (stdout,"PAINT: printing comments\n");

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
	fprintf (stdout,"PAINT: complete\n");

    return 0;
}

static int dump (struct Cell_head *window)
{
    struct Cell_head cur;

    fprintf (stdout,"window----------------\n");
    G__write_Cell_head (stdout, window, 0);
    fprintf (stdout,"cur-------------------\n");
    G_get_set_window (&cur);
    G__write_Cell_head (stdout, &cur, 0);

    return 0;
}
