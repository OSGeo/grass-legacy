/* routines used for generating RGB raster images
 *
 * D_cell_draw_setup_RGB(t, b, l, r)
 *    int t, b, l, r    (pixle extents of display window)
 *                      (obtainable via D_get_screen_window(&t, &b, &l, &r)
 *   Sets up the environment for D_draw_cell_RGB et al
 *
 * D_draw_cell_RGB(A_row, r_array, g_array, b_array, minval, maxval)
 *    int A_row ;  
 *    CELL *r_array, *b_array, *g_array ;
 *    CELL minval, maxval ;
 *   - determinew which pixel row gets the data
 *   - scales the data such that minval => 0 and maxval => 255
 *   - resamples the data to create a pixel array
 *   - returns  -1 on error or end of picture
 *         or array row number needed for next pixel row.
 *
 *   presumes the map is drawn from north to south
 *
 * NOTE: D_cell_draw_RGB() must be preceded by a call to D_set_colors_RGB()
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

static int draw_cell_RGB(
    int A_row,
    void *r_array, void *g_array, void *b_array,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors,
    RASTER_MAP_TYPE r_type, RASTER_MAP_TYPE g_type, RASTER_MAP_TYPE b_type);

int D_draw_raster_RGB(
    int A_row,
    void *r_array, void *g_array, void *b_array,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors,
    RASTER_MAP_TYPE r_type, RASTER_MAP_TYPE g_type, RASTER_MAP_TYPE b_type)
{
    return draw_cell_RGB(A_row,
			 r_array,  g_array,  b_array,
			 r_colors, g_colors, b_colors,
			 r_type,   g_type,   b_type);
}

int D_draw_d_raster_RGB(
    int A_row,
    DCELL *r_array, DCELL *g_array, DCELL *b_array,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors)
{
    return draw_cell_RGB(A_row,
			 r_array,  g_array,  b_array,
			 r_colors, g_colors, b_colors,
			 DCELL_TYPE, DCELL_TYPE, DCELL_TYPE);
}

int D_draw_f_raster_RGB(
    int A_row,
    FCELL *r_array, FCELL *g_array, FCELL *b_array,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors)
{
    return draw_cell_RGB(A_row,
			 r_array,  g_array,  b_array,
			 r_colors, g_colors, b_colors,
			 FCELL_TYPE, FCELL_TYPE, FCELL_TYPE);
}

int D_draw_c_raster_RGB(
    int A_row,
    CELL *r_array, CELL *g_array, CELL *b_array,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors)
{
    return draw_cell_RGB(A_row,
			 r_array,  g_array,  b_array,
			 r_colors, g_colors, b_colors,
			 CELL_TYPE, CELL_TYPE, CELL_TYPE);
}

int D_draw_cell_RGB(
    int A_row,
    CELL *r_array, CELL *g_array, CELL *b_array,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors)
{
    return draw_cell_RGB(A_row,
			 r_array,  g_array,  b_array,
			 r_colors, g_colors, b_colors,
			 CELL_TYPE, CELL_TYPE, CELL_TYPE);
}

static int draw_cell_RGB(
    int A_row,
    void *r_raster, void *g_raster, void *b_raster,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors,
    RASTER_MAP_TYPE r_type, RASTER_MAP_TYPE g_type, RASTER_MAP_TYPE b_type)
{
    static unsigned char *r_buf, *g_buf, *b_buf, *n_buf;
    static int nalloc;

    int r_size = G_raster_size(r_type);
    int g_size = G_raster_size(g_type);
    int b_size = G_raster_size(b_type);
    int ncols = src[0][1] - src[0][0];
    int i;

    /* reallocate color_buf if necessary */
    if (nalloc < ncols)
    {
	nalloc = ncols;
	r_buf = G_realloc(r_buf, nalloc);
	g_buf = G_realloc(g_buf, nalloc);
	b_buf = G_realloc(b_buf, nalloc);
	n_buf = G_realloc(n_buf, nalloc);
    }

    /* convert cell values to bytes */
    G_lookup_raster_colors(r_raster, r_buf, n_buf, n_buf, n_buf, ncols, r_colors, r_type);
    G_lookup_raster_colors(g_raster, n_buf, g_buf, n_buf, n_buf, ncols, g_colors, g_type);
    G_lookup_raster_colors(b_raster, n_buf, n_buf, b_buf, n_buf, ncols, b_colors, b_type);

    if (D__overlay_mode)
	for (i = 0; i < ncols; i++)
	{
	    n_buf[i] = (G_is_null_value(r_raster, r_type) ||
			G_is_null_value(g_raster, g_type) ||
			G_is_null_value(b_raster, b_type));

	    r_raster = G_incr_void_ptr(r_raster, r_size);
	    g_raster = G_incr_void_ptr(g_raster, g_size);
	    b_raster = G_incr_void_ptr(b_raster, b_size);
	}

    A_row = R_scaled_raster(ncols, A_row, r_buf, g_buf, b_buf, D__overlay_mode ? n_buf : NULL);

    return (A_row < src[1][1])
	? A_row
	: -1;
}

int D_cell_draw_setup_RGB(int t,int b,int l,int r)
{
    struct Cell_head window;

    if (G_get_set_window(&window) == -1) 
        G_fatal_error("Current window not available") ;
    if (D_do_conversions(&window, t, b, l, r))
        G_fatal_error("Error in calculating conversions") ;

    D_get_a(src);
    D_get_d(dst);

    R_begin_scaled_raster(src, dst);

    return(0) ;
}

int D_raster_of_type_RGB (
    void *r_raster, void *g_raster, void *b_raster,
    int ncols,int nrows,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors,
    RASTER_MAP_TYPE r_type, RASTER_MAP_TYPE g_type, RASTER_MAP_TYPE b_type)
{
    static unsigned char *r_buf, *g_buf, *b_buf, *n_buf;
    static int nalloc;

    int r_size = G_raster_size(r_type);
    int g_size = G_raster_size(g_type);
    int b_size = G_raster_size(b_type);
    int i;

    /* reallocate color_buf if necessary */
    if(nalloc < ncols)
    {
	nalloc = ncols;
	r_buf = G_realloc(r_buf, ncols);
	g_buf = G_realloc(g_buf, ncols);
	b_buf = G_realloc(b_buf, ncols);
	n_buf = G_realloc(n_buf, ncols);
    }

    /* convert cell values to bytes */
    G_lookup_raster_colors(r_raster, r_buf, n_buf, n_buf, n_buf, ncols, r_colors, r_type);
    G_lookup_raster_colors(g_raster, n_buf, g_buf, n_buf, n_buf, ncols, g_colors, g_type);
    G_lookup_raster_colors(b_raster, n_buf, n_buf, b_buf, n_buf, ncols, b_colors, b_type);

    for (i = 0; i < ncols; i++)
    {
	int r, g, b, x;

	n_buf[i] = (G_is_null_value(r_raster, r_type) ||
		    G_is_null_value(g_raster, g_type) ||
		    G_is_null_value(b_raster, b_type));

	r_raster = G_incr_void_ptr(r_raster, r_size);
	g_raster = G_incr_void_ptr(g_raster, g_size);
	b_raster = G_incr_void_ptr(b_raster, b_size);
    }

    R_RGB_raster(ncols, nrows, r_buf, g_buf, b_buf, D__overlay_mode ? n_buf : NULL);

    return 0;
}

void D_set_colors_RGB(void)
{
    unsigned char array[256];
    int i;

    for (i = 0; i < 256; i++)
	array[i] = i;

    R_set_RGB_color(array, array, array);
}
