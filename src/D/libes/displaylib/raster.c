/*  %W%  %G%  */

/* routines used by programs such as Dcell, display, combine, and weight
 * for generating raster images (for 1-byte, i.e. not super-cell, data)
 *
 * D_cell_draw_setup(t, b, l, r)
 *    int t, b, l, r    (pixle extents of display window)
 *                      (obtainable via D_get_screen_window(&t, &b, &l, &r)
 *   Sets up the environment for D_draw_cell_row.
 *
 * D_draw_cell_row(A_row, xarray)
 *    int A_row ;  
 *    CELL *xarray ;
 *   - determinew which pixle row gets the data
 *   - resamples the data to create a pixle array
 *   - determines best way to draw the array
 *      a - for single cat array, a move and a draw
 *      b - otherwise, a call to R_raster()
 *   - returns  -1 on error or end of picture
 *         or array row number needed for next pixle row.
 *
 *   presumes the map is drawn from north to south
 */

#include <stdio.h>
#include "gis.h"

static int *D_to_A_tab = NULL;
static int D_x_beg, D_y_beg, D_x_end, D_y_end ;
static int cur_D_row ;
static CELL *raster = NULL ;

D_draw_cell_row(A_row, xarray)
	int A_row ;
	CELL *xarray ;
{
	return draw_cell_row (A_row, xarray, 1);
}

D_overlay_cell_row(A_row, xarray)
	int A_row ;
	CELL *xarray ;
{
	return draw_cell_row (A_row, xarray, 0);
}

static
draw_cell_row(A_row, xarray, all)
	int A_row ;
	CELL *xarray ;
{
	int D_row ;
	int repeat ;
	char send_raster ;
	int cur_A_row ;
	double D_d_to_a_row() ;

/* Allocate memory for raster */
	if (! raster)
		raster = (CELL *)G_malloc((D_x_end-D_x_beg+1) * sizeof(CELL)) ;

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
		register CELL *rasptr ;
		register int D_col ;
		rasptr = raster ;

		for (D_col = D_x_beg; D_col<D_x_end; D_col++ )
			*rasptr++ = xarray[D_to_A_tab[D_col]] ; 
	}

	/* Check to see if raster contains one category */
	{
		register CELL *rasptr ;
		register int D_col ;
		register int cat ;
		rasptr = raster ;

		cat = *rasptr ;
		send_raster = 0 ;
		for (D_col = D_x_beg; D_col<D_x_end; D_col++ )
		{
			if (*rasptr++ != cat)
			{
				send_raster = 1 ;
				break ;
			}
		}
	}

	/* Send the raster */
	if (send_raster)
	{
		R_move_abs(D_x_beg, D_row) ;
		R_raster(D_x_end-D_x_beg, repeat, all, raster) ;
	}
	else
	{
		R_color(*raster) ;
		while(repeat--)
		{
			R_move_abs(D_x_beg, D_row++) ;
			if (all || *raster)
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
	int D_col, cellfile ;
	int cellsiz ;
	int *table_pointer ;
	int xbox[5] ;
	int ybox[5] ;
	double current, increment ;
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
	return(0) ;
}
