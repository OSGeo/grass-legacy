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
#include "gis.h"
#include "raster.h"
#include "display.h"

extern int D__overlay_mode;

static int *D_to_A_tab;
static int D_x_beg, D_y_beg, D_x_end, D_y_end ;
static int cur_D_row ;

static int color_buf_size;
static unsigned char *r_buf, *g_buf, *b_buf;
static void *r_raster, *g_raster, *b_raster;

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
    void *r_array, void *g_array, void *b_array,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors,
    RASTER_MAP_TYPE r_type, RASTER_MAP_TYPE g_type, RASTER_MAP_TYPE b_type)
{
    int D_row ;
    int repeat ;
    int cur_A_row ;
    int r_size = G_raster_size(r_type);
    int g_size = G_raster_size(g_type);
    int b_size = G_raster_size(b_type);
    int r_bytes = (D_x_end - D_x_beg) * r_size;
    int g_bytes = (D_x_end - D_x_beg) * g_size;
    int b_bytes = (D_x_end - D_x_beg) * b_size;

/* Allocate memory for raster */
    if(!r_raster)
	r_raster = G_malloc(r_bytes) ;
    if(!g_raster)
	g_raster = G_malloc(g_bytes) ;
    if(!b_raster)
	b_raster = G_malloc(b_bytes) ;

/* If picture is done, return -1 */
    if (cur_D_row >= D_y_end)
        return(-1) ;

/* Get window (array) row currently required */
    D_row = cur_D_row ;
    cur_A_row = (int)D_d_to_a_row(cur_D_row + 0.5) ;

/* If we need a row further down the array, return that row number */
    if (cur_A_row > A_row)
        return (cur_A_row) ;

/* Find out how many screen lines the current A_row gets repeated */
    repeat = 1 ;
    for (cur_D_row++ ; cur_D_row < D_y_end; cur_D_row++)
    {
        if (A_row == (cur_A_row = (int)D_d_to_a_row(cur_D_row + 0.5)))
            repeat++ ;
        else
            break ;
    }

    /* Make the screen raster */
    {
        register int D_col ;
	void *r_ptr, *r_arr_ptr;
	void *g_ptr, *g_arr_ptr;
	void *b_ptr, *b_arr_ptr;

	r_ptr = r_raster;
	g_ptr = g_raster;
	b_ptr = b_raster;

        for (D_col = D_x_beg; D_col < D_x_end; D_col++)
	{
	    /* copy array[[D_to_A_tab[D_col]] to *raster, advance raster by 1 */

	    r_arr_ptr = G_incr_void_ptr(r_array, D_to_A_tab[D_col] * r_size);
	    G_raster_cpy(r_ptr, r_arr_ptr, 1, r_type);
	    r_ptr = G_incr_void_ptr(r_ptr, r_size);

	    g_arr_ptr = G_incr_void_ptr(g_array, D_to_A_tab[D_col] * g_size);
	    G_raster_cpy(g_ptr, g_arr_ptr, 1, g_type);
	    g_ptr = G_incr_void_ptr(g_ptr, g_size);

	    b_arr_ptr = G_incr_void_ptr(b_array, D_to_A_tab[D_col] * b_size);
	    G_raster_cpy(b_ptr, b_arr_ptr, 1, b_type);
	    b_ptr = G_incr_void_ptr(b_ptr, b_size);
        }
    }

    R_move_abs(D_x_beg, D_row) ;
    D_raster_of_type_RGB(r_raster, g_raster, b_raster,
			 D_x_end - D_x_beg, repeat,
			 r_colors, g_colors, b_colors,
			 r_type,   g_type,   b_type);

/* If picture is done, return -1 */
    if (cur_D_row >= D_y_end)
        return(-1) ;

/* Return the array row of the next row needed */
    return (cur_A_row) ;
}

int D_cell_draw_setup_RGB(int t,int b,int l,int r)
{
    int D_col ;
    struct Cell_head window ;

    if (G_get_set_window(&window) == -1) 
        G_fatal_error("Current window not available") ;
    if (D_do_conversions(&window, t, b, l, r))
        G_fatal_error("Error in calculating conversions") ;
/* Set up the screen for drawing map */
    D_x_beg = (int)D_get_d_west() ;
    D_x_end = (int)D_get_d_east() ;
    D_y_beg = (int)D_get_d_north() ;
    D_y_end = (int)D_get_d_south() ;
    cur_D_row = D_y_beg ;

    if (D_to_A_tab)
        free (D_to_A_tab) ;

    D_to_A_tab = (int *)G_calloc(D_x_end, sizeof(int)) ;

/* construct D_to_A_tab for converting x screen Dots to x data Array values */
    for (D_col = D_x_beg; D_col < D_x_end; D_col++)
        D_to_A_tab[D_col] = (int)(D_d_to_a_col(D_col + 0.5)) ;

    if (r_raster)
    {
        free(r_raster) ;
        r_raster = NULL ;
    }

    if (g_raster)
    {
        free(g_raster) ;
        g_raster = NULL ;
    }

    if (b_raster)
    {
        free(b_raster) ;
        b_raster = NULL ;
    }
    return(0) ;
}

int D_raster_of_type_RGB (
    void *r_raster, void *g_raster, void *b_raster,
    int ncols,int nrows,
    struct Colors *r_colors, struct Colors *g_colors, struct Colors *b_colors,
    RASTER_MAP_TYPE r_type, RASTER_MAP_TYPE g_type, RASTER_MAP_TYPE b_type)
{
    int r_size = G_raster_size(r_type);
    int g_size = G_raster_size(g_type);
    int b_size = G_raster_size(b_type);
    unsigned char *r_ptr, *b_ptr, *g_ptr;
    int i;

    /* reallocate color_buf if necessary */
    if(ncols > color_buf_size)
    {
	r_buf = (unsigned char *) G_realloc(r_buf, ncols);
	g_buf = (unsigned char *) G_realloc(g_buf, ncols);
	b_buf = (unsigned char *) G_realloc(b_buf, ncols);
	color_buf_size = ncols;
    }

    r_ptr = r_buf;
    g_ptr = g_buf;
    b_ptr = b_buf;

    /* convert cell values to bytes */
    for (i = 0; i < ncols; i++)
    {
	if (G_is_null_value(r_raster, r_type) ||
	    G_is_null_value(g_raster, g_type) ||
	    G_is_null_value(b_raster, b_type))
	    *r_ptr = *g_ptr = *b_ptr = 0;
	else
	{
	    int r, g, b, x;
	    G_get_raster_color (r_raster, &r, &x, &x, r_colors, r_type);
	    G_get_raster_color (g_raster, &x, &g, &x, g_colors, g_type);
	    G_get_raster_color (b_raster, &x, &x, &b, b_colors, b_type);
	    *r_ptr = r;
	    *g_ptr = g;
	    *b_ptr = b;
	}

	r_raster = G_incr_void_ptr(r_raster, r_size);
	g_raster = G_incr_void_ptr(g_raster, g_size);
	b_raster = G_incr_void_ptr(b_raster, b_size);

	r_ptr++;
	g_ptr++;
	b_ptr++;
    }

    R_RGB_raster (ncols, nrows, r_buf, g_buf, b_buf, !D__overlay_mode);

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
