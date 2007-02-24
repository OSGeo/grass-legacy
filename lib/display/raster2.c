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
#include <grass/gis.h>
#include <grass/display.h>
#include <grass/raster.h>

static unsigned char
    *red = NULL,
    *grn = NULL,
    *blu = NULL,
    *set = NULL;

static int nalloc = 0;

static int allocate_colors(int);

int D__overlay_mode = 0; /* external for now, but to be fixed later */


/*!
 * \brief configure raster overlay mode
 *
 * This routine determines if <i>D_draw_cell</i> draws in overlay mode
 * (locations with category 0 are left untouched) or not (colored with the color
 * for category 0). Set <b>flag</b> to 1 (TRUE) for overlay mode; 0 (FALSE)
 * otherwise.
 *
 *  \param flag
 *  \return int
 */

int D_set_overlay_mode (int n)
{
    D__overlay_mode = (n!=0);

    return 0;
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

int D_color (
    CELL cat,
    struct Colors *colors)
{
    return D_c_color (cat, colors);
}

/* select color for line drawing */
int D_c_color ( CELL cat,
    struct Colors *colors)
{
    return D_color_of_type(&cat, colors, CELL_TYPE);
}

/* select color for line drawing */

/*!
 * \brief 
 *
 * Same functionality as <tt>D_color()</tt> except that the <em>value</em> is type 
 * <tt>DCELL</tt>.  This implies that the floating-point interfaces to the <em>colors</em>
 *  are used by this routine.
 *
 *  \param value
 *  \param colors
 *  \return int
 */

int D_d_color (DCELL val,
    struct Colors *colors)
{
    return D_color_of_type(&val, colors, DCELL_TYPE);
}

/* select color for line drawing */

/*!
 * \brief 
 *
 * Same
 * functionality as <tt>D_color()</tt> except that the <em>value</em> is type <tt>FCELL</tt>. 
 * This implies that the floating-point interfaces to the <em>colors</em> are used by this routine.
 *
 *  \param value
 *  \param colors
 *  \return int
 */

int D_f_color (
    FCELL val,
    struct Colors *colors)
{
    return D_color_of_type(&val, colors, FCELL_TYPE);
}


/*!
 * \brief 
 *
 * If the <em>data_type</em> is CELL_TYPE,
 * calls D_color((CELL *value, colors);
 * If the <em>data_type</em> is FCELL_TYPE, calls D_f_color((FCELL *value,
 * colors);
 * If the <em>data_type</em> is DCELL_TYPE, calls D_d_color((DCELL *value,
 * colors);
 *
 *  \param value
 *  \param colors
 *  \param data_type
 *  \return int
 */

int D_color_of_type( void *raster,
    struct Colors *colors,
    RASTER_MAP_TYPE data_type)
{
    int r, g, b;

    G_get_raster_color(raster, &r, &g, &b, colors, data_type);
    R_RGB_color((unsigned char) r, (unsigned char) g, (unsigned char) b);

    return 0;
}


/*!
 * \brief 
 *
 * If <em>map_type</em> is
 * CELL_TYPE, calls D_raster((CELL *) rast, ncols, nrows, colors);
 * If <em>map_type</em> is FCELL_TYPE, calls D_f_raster((FCELL *) rast, ncols,
 * nrows, colors);
 * If <em>map_type</em> is DCELL_TYPE, calls D_d_raster((DCELL *) rast, ncols,
 * nrows, colors);
 *
 *  \param rast
 *  \param ncols
 *  \param nrows
 *  \param colors
 *  \param data_type
 *  \return int
 */

int D_raster_of_type (
    void *raster,int ncols,int nrows,
    struct Colors *colors,
    RASTER_MAP_TYPE data_type)
{
    int size = G_raster_size(data_type);
    void *p = raster;
    int i;

    if (ncols > nalloc)
	allocate_colors(ncols);

    G_lookup_raster_colors (raster, red, grn, blu, set, ncols, colors, data_type);

    if (D__overlay_mode)
	for (i = 0; i < ncols; i++)
	{
	    set[i] = G_is_null_value(p, data_type);
	    p = G_incr_void_ptr(p, size);
	}

    R_RGB_raster (ncols, nrows, red, grn, blu,
		  D__overlay_mode ? set : NULL);

    return 0;
}

/*!
 * \brief low level raster plotting
 *
 * This low-level routine plots raster data.
 * The <b>raster</b> array has <b>n</b> values. The raster is plotted
 * <b>repeat</b> times, one row below the other. 
 * <b>Note.</b> This routine does not perform resampling or placement.
 * <i>D_draw_cell</i> does resampling and placement and then calls this
 * routine to do the actual plotting.
 *
 *  \param raster
 *  \param n
 *  \param repeat
 *  \param colors
 *  \return int
 */

int D_raster ( CELL *raster,int ncols,int nrows, struct Colors *colors)
{
    return D_c_raster (raster, ncols, nrows, colors);
}

int D_c_raster (
    CELL *raster,int ncols, int nrows,
    struct Colors *colors)
{
    return D_raster_of_type(raster, ncols, nrows, colors, CELL_TYPE);
}


/*!
 * \brief 
 *
 * Same functionality as <tt>D_raster()</tt> except that the <em>fcell</em> array 
 * is type <tt>FCELL</tt>. This implies that the floating-point
 * interfaces to the <em>colors</em> are used by this routine.
 *
 *  \param fcell
 *  \param ncols
 *  \param nrows
 *  \param colors
 *  \return int
 */

int D_f_raster (FCELL *raster,int ncols,int nrows, struct Colors *colors)
{
    return D_raster_of_type(raster, ncols, nrows, colors, FCELL_TYPE);
}


/*!
 * \brief 
 *
 *  Same functionality as <tt>D_raster()</tt> except that the <em>dcell</em> array
 *  is type <tt>DCELL</tt>. This implies that the floating-point
 * interfaces to the <em>colors</em> are used by this routine.
 *
 *  \param dcell
 *  \param ncols
 *  \param nrows
 *  \param colors
 *  \return int
 */

int D_d_raster (
    DCELL *raster,int ncols, int nrows,struct Colors *colors)
{
    return D_raster_of_type(raster, ncols, nrows, colors, DCELL_TYPE);
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

