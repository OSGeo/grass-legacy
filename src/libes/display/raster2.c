/********************************************************************
 * code in this file is designed to send raster data to the graphics
 * driver. It handles cell->color lookup translation, as well as
 * loading appropriate colormaps into the driver and the sending of
 * raster data to the plotter. The loading of colors is desinged to
 * never send more colors than the hardware can support - even though
 * the GRASS drivers will allocate virtual colormaps to pretend there are more
 * This code effectively disables that driver feature/mistake.
 *
 * To simply plot raster data:
 *
 * to get the colors loaded
 *   D_set_colors (colors)
 *      struct Colors *colors;  /color info/
 *
 * to specify if overlay mode is to be used
 *   D_set_overlay_mode(flag)
 *      int flag;              /1=yes,0=no/

 * to plot a row of cell data
 *   D_raster (cell, ncols, nrows, colors)
 *      CELL *cell;            /cell data to be plotted/
 *      int ncols;             /number of columns in cell array/
 *      int nrows;             /number of rows this array represents/
 *      struct Colors *colors; /color info/
 *
 * to select a raster color for line drawing
 *   D_color (cat, colors)
 *      CELL cat
 *      struct Colors *colors; /color info/
 *
 * to translate cell values into colors for calls to R_color()
 *   D_lookup_colors (cell, n, colors)
 *      CELL *cell;            /cell data to be plotted/
 *      int n;                 /number of items in cell array/
 *      struct Colors *colors; /color info/
 *      
 *
 * Note: the same Colors structure must be passed to all routines.
 *
 */
#include "gis.h"

static CELL cmin = 0, cmax = -1;
static int
    RED[256],
    GRN[256],
    BLU[256],
    fixed = 1;

static unsigned char
    *red = NULL,
    *grn = NULL,
    *blu = NULL,
    *set = NULL;

static int nalloc = 0;

int D__overlay_mode = 0; /* external for now, but to be fixed later */

D_set_overlay_mode (n)
{
    D__overlay_mode = (n!=0);
}

D_set_colors (colors)
    struct Colors *colors;
{
    int ncolors;
    int nl, i, r, g, b;
    unsigned char junk, R,G,B;
    CELL cat;
    double span;

/* check if the colors fit into the hardware colormap */
    G_get_color_range (&cmin, &cmax, colors);
    fixed = !D_check_colormap_size (cmin, cmax, &ncolors);

    nalloc = 0; /* force a reallocation */
    if (fixed)
    {
	for(nl=0; nl*nl*nl <= ncolors-1; nl++)
		;
	nl--;
	ncolors = nl*nl*nl+1; /* reset ncolors to what we need for fixed */

	/* create color translation table */
	for(i=0; i<256; i++)
	{
	    RED[i] = (int)((i / 256.0) * nl) * nl * nl ;
	    GRN[i] = (int)((i / 256.0) * nl) * nl ;
	    BLU[i] = (int)((i / 256.0) * nl) ;
	}

	/* create the colortable for the driver */

	if (ncolors > nalloc)
	    allocate_colors (ncolors);
 
	if (nl > 1)
	    span = 255.0 / (nl-1) ;
	else
	    span = 0.0;

	*red = *grn = *blu = 255;
	i = 1 ;
        for(r=0; r<nl; r++)
	{
	    R = (int)(r * span) ;
	    for(g=0; g<nl; g++)
	    {
		G = (int)(g * span) ;
		for(b=0; b<nl; b++)
		{
		    B = (int)(b * span) ;
		    red[i] = R;
		    grn[i] = G;
		    blu[i] = B;
		    i++;
		}
	    }
	}
    }
    else
    {
	if (ncolors > nalloc)
	    allocate_colors (ncolors);

	cat = 0;
	G_lookup_colors (&cat, red, grn, blu, &junk, 1, colors);
	for (i = 1, cat = cmin; cat <= cmax; cat++, i++)
	    G_lookup_colors (&cat, red+i, grn+i, blu+i, &junk, 1, colors);
    }

    /* send the colortable to the driver */
    R_reset_colors (0, ncolors-1, red, grn, blu);
    synch();

/* tell if the color table fits into the hardware */
    return !fixed;
}

/* this routine modifies the hardware colormap
 * provided that we are not using fixed mode colors.
 * For use by programs such as d.colors
 *
 * returns:
 *    0 error - in fixed mode,
 *              or cat not in min:max color range
 *    1 ok
 */
D_reset_color (cat, r, g, b)
    CELL cat;
{
    if (fixed) return 0;
    if (cat)
    {
	if (cat < cmin || cat > cmax) return 0;
	cat -= cmin-1;
    }
    R_reset_color ((unsigned char)r, (unsigned char)g, (unsigned char)b, cat);
    synch();
    return 1;
}

/* select color for line drawing */
D_color (cat, colors)
    CELL cat;
    struct Colors *colors;
{
    CELL x;

    x = cat;
    D_lookup_colors (&x,1,colors);
    R_color ((int)x);
}

D_lookup_colors (cell, ncols, colors)
    CELL *cell;
    struct Colors *colors;
{
    int i;
    CELL cat;
    if (fixed)
    {
	if (ncols > nalloc)
	    allocate_colors (ncols);

	G_lookup_colors (cell, red, grn, blu, set, ncols, colors);
	for (i = 0; i < ncols; i++)
	    if (!D__overlay_mode || cell[i])
		cell[i] = RED[red[i]] + GRN[grn[i]] + BLU[blu[i]] + 1;
    }
    else
    {
	for (i = 0; i < ncols; i++)
	{
	    if(cat = cell[i])
	    {
		if (cat < cmin || cat > cmax)
		    cell[i] = 0;
		else
		    cell[i] -= cmin-1;
	    }
	}
    }
}

D_raster (cell, ncols, nrows, colors)
    CELL *cell;
    struct Colors *colors;
{
    D_lookup_colors (cell, ncols, colors);
    R_raster (ncols, nrows, !D__overlay_mode, cell);
}

/* This routine determines if the number of required colors
 * fits into the hardware colormap or not. If it does, then
 * it is possible to change the individual colors by changing
 * the hardware colormap. Otherwise a fixed lookup scheme is to
 * be used and no color toggling is possible
 *
 * If the colors will fit,
 *   the required number of colors is passed back (in ncolors), computed as
 *     max-min+2   (1 extra for 0, even if 0 is between min and max)
 *   and 1 is returned.
 *
 * Otherwise the number of hardware colors is passed back (in ncolors)
 *   and 0 is returned.
 */

D_check_colormap_size (min, max, ncolors)
    CELL min, max;
    int *ncolors;
{
    int hardware_ncolors;

/* find out how many colors the hardware has */
    R_get_num_colors (&hardware_ncolors);
    *ncolors = max - min + 2; /* extra color for 0 */

/* if we need more colors than there are in the driver, then return hardware_ncolors
 * otherwise return ncolors;
 */
    if (*ncolors <= 1 || *ncolors > hardware_ncolors)
    {
	*ncolors = hardware_ncolors;
	return 0;
    }
    return 1;
}

static
synch()
{
    int junk;
    R_get_num_colors (&junk); /* this will cause a round trip to driver */
}

static
allocate_colors(ncolors)
{
    red = (unsigned char *) G_realloc (red, ncolors);
    grn = (unsigned char *) G_realloc (grn, ncolors);
    blu = (unsigned char *) G_realloc (blu, ncolors);
    set = (unsigned char *) G_realloc (set, ncolors);
    nalloc = ncolors;
}
