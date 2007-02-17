/* routines used by programs such as Dcell, display, combine, and weight
 * for generating raster images (for 1-byte, i.e. not super-cell, data)
 *
 * D_cell_draw_setup(t, b, l, r)
 *    int t, b, l, r    (pixle extents of display window)
 *                      (obtainable via D_get_screen_window(&t, &b, &l, &r)
 *   Sets up the environment for D_draw_cell
 *
 * D_draw_cell(A_row, xarray, colors)
 *    int A_row ;  
 *    CELL *xarray ;
 *   - determinew which pixle row gets the data
 *   - resamples the data to create a pixle array
 *   - determines best way to draw the array
 *      a - for single cat array, a move and a draw
 *      b - otherwise, a call to D_raster()
 *   - returns  -1 on error or end of picture
 *         or array row number needed for next pixle row.
 *
 *   presumes the map is drawn from north to south
 *
 * ALSO: if overlay mode is desired, then call D_set_overlay_mode(1)
 *       first.
 */

#include <stdio.h>
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/display.h>

extern int D__overlay_mode;

static int src[2][2], dst[2][2];

static int draw_cell(int,void *,struct Colors *,RASTER_MAP_TYPE);

int D_draw_raster(
    int A_row,
    void *array,
    struct Colors *colors,
    RASTER_MAP_TYPE data_type)
{
    return draw_cell(A_row, array, colors, data_type);
}

int D_draw_d_raster(
    int A_row,
    DCELL *darray,
    struct Colors *colors)
{
    return draw_cell(A_row, (void *) darray, colors, DCELL_TYPE);
}

int D_draw_f_raster(
    int A_row,
    FCELL *farray,
    struct Colors *colors)
{
    return draw_cell(A_row, (void *) farray, colors, FCELL_TYPE);
}

int D_draw_c_raster(
    int A_row,
    CELL *carray,
    struct Colors *colors)
{
    return draw_cell(A_row, (void *) carray, colors, CELL_TYPE);
}


/*!
 * \brief render a raster row
 *
 * The <b>row</b> gives the map array row. The <b>raster</b>
 * array provides the categories for each raster value in that row. 
 * This routine is called consecutively with the information necessary to draw a
 * raster image from north to south. No rows can be skipped. All screen pixel
 * rows which represent the current map array row are rendered. The routine
 * returns the map array row which is needed to draw the next screen pixel row.
 *
 *  \param row
 *  \param raster
 *  \param colors
 *  \return int
 */

int D_draw_cell(
    int A_row,
    CELL *carray,
    struct Colors *colors)
{
    return draw_cell(A_row, (void *) carray, colors, CELL_TYPE);
}

static int draw_cell(
    int A_row,
    void *array,
    struct Colors *colors,
    RASTER_MAP_TYPE data_type)
{
	static unsigned char *red, *grn, *blu, *set;
	static int nalloc;

	int ncols = src[0][1] - src[0][0];
	int i;

	if (nalloc < ncols)
	{
		nalloc = ncols;
		red = G_realloc(red, nalloc);
		grn = G_realloc(grn, nalloc);
		blu = G_realloc(blu, nalloc);
		set = G_realloc(set, nalloc);
	}

	G_lookup_raster_colors(array, red, grn, blu, set, ncols, colors, data_type);

	if (D__overlay_mode)
		for (i = 0; i < ncols; i++)
		{
			set[i] = G_is_null_value(array, data_type);
			array = G_incr_void_ptr(array, G_raster_size(data_type));
		}

	A_row = R_scaled_raster(ncols, A_row, red, grn, blu, D__overlay_mode ? set : NULL);

	return (A_row < src[1][1])
		? A_row
		: -1;
}

/*!
 * \brief prepare for raster graphic
 *
 * The raster display subsystem establishes
 * conversion parameters based on the screen extent defined by <b>top,
 * bottom, left</b>, and <b>right</b>, all of which are obtainable from
 * <i>D_get_screen_window for the current frame.</i>
 *
 *  \param top
 *  \param bottom
 *  \param left
 *  \param right
 *  \return int
 */

int D_cell_draw_setup(int t,int b,int l,int r)
{
    struct Cell_head window;

    if (G_get_set_window(&window) == -1) 
        G_fatal_error("Current window not available");
    if (D_do_conversions(&window, t, b, l, r))
        G_fatal_error("Error in calculating conversions");

    /* Set up the screen for drawing map */
    D_get_a(src);
    D_get_d(dst);

    R_begin_scaled_raster(src, dst);

    return 0;
}

