/********************************************************************
 * code in this file is designed to send raster data to the graphics
 * driver. It handles raster->color lookup translation, as well as
 * loading appropriate colormaps into the driver and the sending of
 * raster data to the plotter. The loading of colors is designed to
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

 * to plot a row of raster data
 *   D_raster (raster, ncols, nrows, colors)
 *      CELL *raster;            /raster data to be plotted/
 *      int ncols;             /number of columns in raster array/
 *      int nrows;             /number of rows this array represents/
 *      struct Colors *colors; /color info/
 *
 *   D_raster_of_type (raster, ncols, nrows, colors, data_type)
 *      void *raster;
 *      int ncols;             /number of columns in raster array/
 *      int nrows;             /number of rows this array represents/
 *      struct Colors *colors; /color info/
 *      RASTER_MAP_TYPE data_type;
 *
 * to select a raster color for line drawing
 *   D_color (cat, colors)
 *      CELL cat
 *      struct Colors *colors; /color info/
 *
 *   D_color_of_type(raster, colors, data_type);
 *      void *raster;
 *      struct Colors *colors; /color info/
 *      RASTER_MAP_TYPE data_type;
 *
 * to translate raster values into colors for calls to R_color()
 *   D_lookup_colors (raster, n, colors)
 *      CELL *raster;            /raster data to be plotted/
 *      INT N;                 /NUMBER OF ITEMS IN RASTER ARRAY/
 *      struct Colors *colors; /color info/
 *      
 *
 * Note: the same Colors structure must be passed to all routines.
 *
 */
#include <stdlib.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

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

static int color_buf_size = 0;
static int *color_buf = NULL;
static int is_grey_scale(struct Colors *);
static int synch(void);
static int allocate_colors(int);

int D__overlay_mode = 0; /* external for now, but to be fixed later */

int D_set_overlay_mode (int n)
{
    D__overlay_mode = (n!=0);

    return 0;
}

int D_set_colors (struct Colors *colors)
{
    int ncolors;
    int nl, i, r, g, b;
    unsigned char junk, R,G,B;
    CELL cat;
    double span;

/* check if the colors fit into the hardware colormap */
    G_get_color_range (&cmin, &cmax, colors);

    fixed = !D_check_colormap_size (cmin, cmax, &ncolors);
if(getenv("DEBUG"))fprintf (stderr, "# monitor colors = %d (mode: %s)\n", ncolors, fixed?"fixed":"float");

    nalloc = 0; /* force a reallocation */
    if (fixed)
    {
	if (is_grey_scale (colors))
	{
	    /* restrict colortable to a feasible size */
	    if (ncolors > 256) ncolors = 256;

	    for (i = 0; i < 256; i++)
	    {
		RED[i] = (int) (i * (ncolors-1))/256;
		GRN[i] = 0;
		BLU[i] = 0;
	    }
	    if (ncolors > nalloc)
		allocate_colors (ncolors);
	    for (i = 0; i < ncolors; i++)
		red[i] = grn[i] = blu[i] = (i*255)/ncolors;
	}
	else  /* use a color cube */
	{
	    /* compute size of color cube */
	    for(nl=0; nl*nl*nl <= ncolors-1; nl++) {}
	    nl--;

	    /* restrict colortable to a feasible size */
	    if (nl > 32) nl = 32;

	    /* reset ncolors to what we need for color cube */
	    ncolors = nl*nl*nl+1;

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
    }
    else
    {
        int r, b, g;
	if (ncolors > nalloc)
	    allocate_colors (ncolors);

	for (i = 1, cat = cmin; cat <= cmax; cat++, i++)
	    G_lookup_colors (&cat, red+i, grn+i, blu+i, &junk, 1, colors);

        G_get_null_value_color(&r, &g, &b, colors);
        /* G_get_null_value_color expects int pointers */
        red[ncolors-2] = r;
        grn[ncolors-2] = g;
        blu[ncolors-2] = b;
        red[0] = r;
        grn[0] = g;
        blu[0] = b;

        G_get_default_color(&r, &g, &b, colors);
        red[ncolors-1] = r;
        grn[ncolors-1] = g;
        blu[ncolors-1] = b;

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
int D_reset_color (CELL cat,int r,int g,int b)
{
    if (fixed) return 0;
    if(!G_is_c_null_value(&cat))
    {
	if (cat < cmin || cat > cmax) 
            cat = cmax - cmin + 3;
	else
            cat -= cmin - 1;
    }
    else
         cat = cmax - cmin + 2;
/* OLGA: I put null and undef colors to the end of color table */
    R_reset_color ((unsigned char)r, (unsigned char)g, (unsigned char)b, cat);
    synch();
    return 1;
}

int D_color (
    CELL cat,
    struct Colors *colors)
{
    D_c_color (cat, colors);

    return 0;
}

/* select color for line drawing */
int D_c_color ( CELL cat,
    struct Colors *colors)
{
    CELL x;

    x = cat;
    D_lookup_colors (&x,1,colors);
    R_color ((int)x);

    return 0;
}

/* select color for line drawing */
int D_d_color (DCELL val,
    struct Colors *colors)
{
    DCELL tmp = val;
    int color;

    D_lookup_d_raster_colors (&tmp, &color,1,colors);
    R_color (color);

    return 0;
}

/* select color for line drawing */
int D_f_color (
    FCELL val,
    struct Colors *colors)
{
    FCELL tmp = val;
    int color;

    D_lookup_f_raster_colors (&tmp, &color,1,colors);
    R_color (color);

    return 0;
}

int D_color_of_type( void *raster,
    struct Colors *colors,
    RASTER_MAP_TYPE data_type)
{
    switch(data_type)
    {
       case CELL_TYPE: D_color(*((CELL *)raster), colors); break;
       case FCELL_TYPE: D_d_color(*((FCELL *)raster), colors); break;
       case DCELL_TYPE: D_f_color(*((DCELL *)raster), colors); break;
    }

    return 0;
}

int D_lookup_colors ( CELL *raster,int ncols, struct Colors *colors)
{
   D_lookup_raster_colors ((void *) raster, (int *) raster, ncols, colors, CELL_TYPE);

    return 0;
}

int D_lookup_c_raster_colors (
    CELL *raster,int ncols,
    struct Colors *colors)
{
   D_lookup_raster_colors ((void *) raster, (int *) raster, ncols, colors, CELL_TYPE);

    return 0;
}

int D_lookup_f_raster_colors (
    FCELL *fraster,
    int *color_buf,int ncols,
    struct Colors *colors)
{
   D_lookup_raster_colors ((void *) fraster, color_buf, ncols, colors, FCELL_TYPE);

    return 0;
}

int D_lookup_d_raster_colors (
    DCELL *draster,
    int *color_buf,int ncols,
    struct Colors *colors)
{
   D_lookup_raster_colors ((void *) draster, color_buf, ncols, colors, DCELL_TYPE);

    return 0;
}

int D_lookup_raster_colors (
    void *raster,
    int *color_buf,
    int ncols,
    struct Colors *colors,
    RASTER_MAP_TYPE data_type)
{
    int i;
    CELL cat;
    if (fixed)
    {
	if (ncols > nalloc)
	    allocate_colors (ncols);

	G_lookup_raster_colors (raster, red, grn, blu, set, ncols, colors, data_type);

	for (i = 0; i < ncols; i++)
        {
	    if (!D__overlay_mode || !G_is_null_value(raster, data_type))
		color_buf[i] = RED[red[i]] + GRN[grn[i]] + BLU[blu[i]] + 1;
            else color_buf[i] = 0;
	    raster = G_incr_void_ptr(raster, G_raster_size(data_type));
        }
    }
    else /* not fixed can only be CELL_TYPE */
    {
	for (i = 0; i < ncols; i++)
	{
	    color_buf[i] = cat = G_get_raster_value_c(raster, data_type);
	    if(!G_is_null_value(raster, data_type))
	    {
		if (cat < cmin || cat > cmax)
		    color_buf[i] = cmax - cmin + 3;
		else
		    color_buf[i] -= cmin-1;
	    }
            else
            {
                 if(!D__overlay_mode)
                   color_buf[i] = cmax - cmin + 2;
                 else
		   color_buf[i] = 0;
            }
	    raster = G_incr_void_ptr(raster, G_raster_size(data_type));
	}
    }

    return 0;
}

int D_raster_of_type (
    void *raster,int ncols,int nrows,
    struct Colors *colors,
    RASTER_MAP_TYPE data_type)
{
    /* reallocate color_buf if necessary */
    if(data_type != CELL_TYPE && ncols >= color_buf_size)
    {
       color_buf = (int *) G_realloc((char *) color_buf, ncols * sizeof(int));
       color_buf_size = ncols;
    }
    else
       color_buf = (int *) raster;

    D_lookup_raster_colors (raster, color_buf, ncols, colors, data_type);
    R_raster (ncols, nrows, !D__overlay_mode, color_buf);

    return 0;
}

int D_raster ( CELL *raster,int ncols,int nrows, struct Colors *colors)
{
    D_c_raster (raster, ncols, nrows, colors);

    return 0;
}

int D_c_raster (
    CELL *raster,int ncols, int nrows,
    struct Colors *colors)
{
    D_lookup_colors (raster, ncols, colors);
    R_raster (ncols, nrows, !D__overlay_mode, raster);

    return 0;
}

int D_f_raster (FCELL *fraster,int ncols,int nrows, struct Colors *colors)
{
      /* reallocate color_buf if necessary */
      if(ncols >= color_buf_size)
    /* reallocate color_buf if necessary */
    if(ncols >= color_buf_size)
    {
       color_buf = (int *) G_realloc((char *) color_buf, ncols * sizeof(int));
       color_buf_size = ncols;
    }
    D_lookup_f_raster_colors (fraster, color_buf, ncols, colors);
    R_raster (ncols, nrows, !D__overlay_mode, color_buf);

    return 0;
}

int D_d_raster (
    DCELL *draster,int ncols, int nrows,struct Colors *colors)
{
    /* reallocate color_buf if necessary */
    if(ncols >= color_buf_size)
    {
       color_buf = (int *) G_realloc((char *) color_buf, ncols * sizeof(int));
       color_buf_size = ncols;
    }
    D_lookup_d_raster_colors (draster, color_buf, ncols, colors);
    R_raster (ncols, nrows, !D__overlay_mode, color_buf);

    return 0;
}

/* This routine determines if the number of required colors
 * fits into the hardware colormap or not. If it does, then
 * it is possible to change the individual colors by changing
 * the hardware colormap. Otherwise a fixed lookup scheme is to
 * be used and no color toggling is possible
 *
 * If the colors will fit,
 *   the required number of colors is passed back (in ncolors), computed as
 *     max-min+4   (1 extra for shift 2 extra for null value and undef)
 *   All categories are shifter up by 1 so that 0 means no color 
 *   cat=0 should only be sent to R_raster() if not overlay mode 
 *   for R_raster().
 *   and 1 is returned.
 *
 * Otherwise the number of hardware colors is passed back (in ncolors)
 *   and 0 is returned.
 *
 * Note : in case of floating color rules, max - min + 1 is so big, that
 * hardware_colors size is guaranteed to be chosen
 *
 */

int D_check_colormap_size (CELL min,CELL max,int *ncolors)
{
    int hardware_ncolors;

/* find out how many colors the hardware has */
    R_get_num_colors (&hardware_ncolors);
    *ncolors = max - min + 4; /* 2 extra color for null and undef */

/* if we need more colors than there are in the driver,
 * then return hardware_ncolors
 * otherwise return ncolors;
 */
    if (*ncolors <= 1 || *ncolors > hardware_ncolors)
    {
	*ncolors = hardware_ncolors;
	return 0;
    }
    return 1;
}

static int synch()
{
    R_stabilize();

    return 0;
}

static int allocate_colors(int ncolors)
{
    red = (unsigned char *) G_realloc (red, ncolors);
    grn = (unsigned char *) G_realloc (grn, ncolors);
    blu = (unsigned char *) G_realloc (blu, ncolors);
    set = (unsigned char *) G_realloc (set, ncolors);
    nalloc = ncolors;

    return 0;
}

static int is_grey_scale(struct Colors *colors)
{
    int r, g, b;
    CELL min, max;
    struct _Color_Rule_ *rule;
    struct _Color_Info_ *cp;

    G_get_color_range (&min, &max, colors);

    cp = &colors->modular;
    for (rule = cp->rules; rule; rule = rule->next)
    {
        if (rule->low.red != rule->low.grn 
	 || rule->low.red != rule->low.blu 
	 || rule->low.grn != rule->low.blu)
        {
  	   return 0;
        }

        if (rule->high.red != rule->high.grn 
	 || rule->high.red != rule->high.blu 
	 || rule->high.grn != rule->high.blu)
        {
  	   return 0;
        }
    }

    cp = &colors->fixed;
    for (rule = cp->rules; rule; rule = rule->next) 
    {
        if (rule->low.red != rule->low.grn 
	 || rule->low.red != rule->low.blu 
	 || rule->low.grn != rule->low.blu)
        {
  	   return 0;
        }

        if (rule->high.red != rule->high.grn 
	 || rule->high.red != rule->high.blu 
	 || rule->high.grn != rule->high.blu)
        {
  	   return 0;
        }
    }

    G_get_null_value_color(&r, &g, &b, colors);
    if (r != g || r != b || g != b) return 0;
    G_get_default_color(&r, &g, &b, colors);
    if (r != g || r != b || g != b) return 0;
    return 1;
}
