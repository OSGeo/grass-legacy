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
 * NOTE: D_cell_draw() must be preceded by a call to D_set_colors()
 *       with the same Colors structure
 *
 * ALSO: if overlay mode is desired, then call D_set_overlay_mode(1)
 *       first.
 */

#include <stdio.h>
#include "gis.h"

static int *D_to_A_tab = NULL;
static int D_x_beg, D_y_beg, D_x_end, D_y_end ;
static int cur_D_row ;
static CELL *raster = NULL ;
static DCELL *draster = NULL ;


D_draw_d_cell(A_row, darray, colors)
    int A_row ;
    DCELL *darray ;
    struct Colors *colors;
{
    return draw_cell(A_row, (CELL *)NULL, darray, colors);
}

D_draw_cell(A_row, carray, colors)
    int A_row ;
    CELL *carray ;
    struct Colors *colors;
{
    return draw_cell(A_row, carray, (DCELL *)NULL, colors);
}

static int 
draw_cell(A_row, carray, darray, colors)
    int A_row ;
    CELL *carray ;
    DCELL *darray ;
    struct Colors *colors;
{
    int D_row ;
    int repeat ;
    char send_raster ;
    int cur_A_row ;
    double D_d_to_a_row() ;

/* Allocate memory for raster */
    if (carray && !raster)
        raster = (CELL *)G_malloc((D_x_end-D_x_beg+1) * sizeof(CELL)) ;
    if (darray && !draster)
        draster = (DCELL *)G_malloc((D_x_end-D_x_beg+1) * sizeof(DCELL)) ;

/* If picture is done, return -1 */
    if (cur_D_row >= D_y_end)
        return(-1) ;

/* Get window (array) row currently required */
    D_row = cur_D_row ;
    cur_A_row = (int)D_d_to_a_row((double)cur_D_row) ;

/* If we need a row further down the array, return that row number */
    if (cur_A_row > A_row)
        return (cur_A_row) ;

/* Find out how many screen lines the current A_row gets repeated */
    repeat = 1 ;
    for (cur_D_row++ ; cur_D_row < D_y_end; cur_D_row++)
    {
        if (A_row == (cur_A_row = (int)D_d_to_a_row((double)cur_D_row)))
            repeat++ ;
        else
            break ;
    }

    /* Make the screen raster */
    {
        register int D_col ;

	if (carray)
	{
            register CELL *rasptr ;
            rasptr = raster ;
            for (D_col = D_x_beg; D_col<D_x_end; D_col++ )
                *rasptr++ = carray[D_to_A_tab[D_col]] ; 
	}
	else
	{
            register DCELL *drasptr ;
            drasptr = draster ;
            for (D_col = D_x_beg; D_col<D_x_end; D_col++ )
                *drasptr++ = darray[D_to_A_tab[D_col]] ; 
	}
    }

    /* Check to see if raster contains one category */
    {
        register CELL *rasptr ;
        register DCELL *drasptr ;
        register int D_col ;
        register int cat ;
	register DCELL val;

        send_raster = 0 ;
	if(carray)
	{
           rasptr = raster ;
           cat = *rasptr ;
           for (D_col = D_x_beg; D_col<D_x_end; D_col++ )
           {
               if (*rasptr++ != cat)
               {
                   send_raster = 1 ;
                   break ;
               }
           }
        }
        else
	{
           drasptr = draster ;
           val = *drasptr ;
           for (D_col = D_x_beg; D_col<D_x_end; D_col++ )
           {
               if (*drasptr++ != val)
               {
                   send_raster = 1 ;
                   break ;
               }
           }
        }
    }

    /* Send the raster */
    if (send_raster)
    {
        R_move_abs(D_x_beg, D_row) ;
	if (carray)
	    D_raster(raster, D_x_end-D_x_beg, repeat, colors) ;
	else
	    D_d_raster(draster, D_x_end-D_x_beg, repeat, colors) ;
    }
    else
    {
	/* this really should be part of D_raster(); */
	extern int D__overlay_mode;
	if (carray)
	    D_color(*raster, colors) ;
	else
	    D_d_color(*draster, colors) ;
        while(repeat--)
        {
            R_move_abs(D_x_beg, D_row++) ;
            if (!D__overlay_mode || !G_is_null_value(*raster))
                R_cont_rel(D_x_end-D_x_beg-1, 0) ;
            else
                R_move_rel(D_x_end-D_x_beg-1, 0) ;
        }
    }

/* If picture is done, return -1 */
    if (cur_D_row >= D_y_end)
        return(-1) ;

/* Return the array row of the next row needed */
    return (cur_A_row) ;
}

D_cell_draw_setup(t, b, l, r)
    int t, b, l, r ;
{
    int D_col ;
    double D_d_to_a_col() ;
    struct Cell_head window ;
    double D_get_d_west() ;
    double D_get_d_east() ;
    double D_get_d_north() ;
    double D_get_d_south() ;

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

    D_to_A_tab = (int *)G_calloc(D_x_end+1, sizeof(int)) ;

/* construct D_to_A_tab for converting x screen Dots to x data Array values */
    for (D_col = D_x_beg; D_col<=D_x_end; D_col++)
    {
        D_to_A_tab[D_col] = (int)(D_d_to_a_col((double)D_col)) ;
    }

    if (raster)
    {
        free(raster) ;
        raster = NULL ;
    }
    if (draster)
    {
        free(draster) ;
        draster = NULL ;
    }
    return(0) ;
}

