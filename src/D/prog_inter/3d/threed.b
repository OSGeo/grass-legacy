#include <stdio.h>
#include "gis.h"
#include "3d.h"
#include "options.h"

static CELL *cat_array ;
static CELL *elv_array_1 ;
static CELL *elv_array_2 ;
static int cell ;
static int elev ;

threed(interactive)
{
	int col_beg    ;      /* western  edge denoting window (array) */
	int col_end    ;      /* eastern  edge denoting window (array) */
	int row_beg    ;      /* southern edge denoting window (array) */
	int row_end    ;      /* northern edge denoting window (array) */
	char buffer[128] ;
	int row_dir ;
	int col_dir ;
	int break_row ;

	struct Colors colors ;

/* Set the colors for the display */
	if (G_read_colors(file, file_mapset, &colors) == -1)
	{
		sprintf(buffer,"R_color file for [%s] not available", file) ;
			G_fatal_error(buffer) ;
	}

	R_reset_colors(colors.min, colors.max,
 					colors.red, colors.grn, colors.blu) ;
	/*
	R_reset_colors(0, 0,
 					&colors.r0, &colors.g0, &colors.b0) ;
	*/

/* Open the necessary cell files */
	open_file(&cell, file, file_mapset) ;
	open_file(&elev, elevfile, elevfile_mapset) ;

/* Allocate space for arrays */
	cat_array   = G_allocate_cell_buf() ;
	elv_array_1 = G_allocate_cell_buf() ;
	elv_array_2 = G_allocate_cell_buf() ;

/* Set up for drawing map */

	switch (direction)
	{
		case NORTH_WEST:
		case WEST_NORTH:
			row_beg = window.rows - 1 ;
			row_end = 1 ;
			row_dir = -1 ;
			col_beg = window.cols - 1 ;
			col_end = 1 ;
			col_dir = -1 ;
			printf("View from North West\n") ;
			break ;

		case SOUTH_WEST :
		case WEST_SOUTH :
			row_beg = 1 ;
			row_end = window.rows - 1 ;
			row_dir = 1 ;
			col_beg = window.cols - 1 ;
			col_end = 1 ;
			col_dir = -1 ;
			printf("View from South West\n") ;
			break ;

		case NORTH_EAST:
		case EAST_NORTH:
			row_beg = window.rows - 1 ;
			row_end = 1 ;
			row_dir = -1 ;
			col_beg = 1 ;
			col_end = window.cols - 1 ;
			col_dir = 1 ;
			printf("View from North East\n") ;
			break ;

		case SOUTH_EAST :
		case EAST_SOUTH :
			row_beg = 1 ;
			row_end = window.rows - 1 ;
			row_dir = 1 ;
			col_beg = 1 ;
			col_end = window.cols - 1 ;
			col_dir = 1 ;
			printf("View from South East\n") ;
			break ;
	}

	if(interactive == 1)
	{
		setbuf(stdout,0) ;
		printf("\nRows %d to %d at:    ", row_beg, row_end) ;
	}

/* Provide for plotting from north and south if necessary */
	if ((from_northing > window.south) && (from_northing < window.north))
	{
		break_row = (int)((window.north - from_northing) / window.ns_res + .5) ;
		do_plot(row_end-row_dir,break_row-row_dir,-row_dir,col_beg,col_end,col_dir,interactive,&window) ;
		do_plot(row_beg,break_row+row_dir,row_dir,col_beg,col_end,col_dir,interactive,&window) ;
	}
	else
	{
		do_plot(row_beg,row_end,row_dir,col_beg,col_end,col_dir,interactive,&window) ;
	}

	close(cell) ;
	close(elev) ;
}

do_plot(row_beg,row_end,row_dir,col_beg,col_end,col_dir,interactive,window)
	struct Cell_head *window ;
{
	int cur_colr ;
	int screen_x[4] ;
	int screen_y[4] ;

	int scr_x1[2048] ;
	int scr_y1[2048] ;
	int scr_x2[2048] ;
	int scr_y2[2048] ;
	int *scr_x_1 ;
	int *scr_x_2 ;
	int *scr_y_1 ;
	int *scr_y_2 ;
	char flip ;
	int atcol, atrow ;
	int atcol_plus ;

	set_signals() ;

/* Now draw the map */
	G_get_map_row_nomask (elev,elv_array_1,row_beg);
	Screen_calc(elv_array_1, exag, scr_x1, scr_y1, row_beg, window, do_zero) ;

	flip = 0 ;
	cur_colr = 0 ;
	for (atrow=row_beg; atrow!=row_end; atrow+=row_dir)
	{
		if(check_signal())
			break ;

		if(interactive == 1)
			printf("\b\b\b\b%4d", atrow) ;
		switch (flip)
		{
			case 1:
				G_get_map_row_nomask (elev, elv_array_1, atrow+row_dir) ; 
				Screen_calc(elv_array_1, exag, scr_x1, scr_y1,
					atrow, window, do_zero) ;
				scr_x_1 = scr_x2 ;   scr_y_1 = scr_y2 ;
				scr_x_2 = scr_x1 ;   scr_y_2 = scr_y1 ;
				flip = 0 ;
				break ;
			case 0:
				G_get_map_row_nomask (elev, elv_array_2, atrow+row_dir) ; 
				Screen_calc(elv_array_2, exag, scr_x2, scr_y2,
					atrow, window, do_zero) ;
				scr_x_1 = scr_x1 ;   scr_y_1 = scr_y1 ;
				scr_x_2 = scr_x2 ;   scr_y_2 = scr_y2 ;
				flip = 1 ;
				break ;
		}

		G_get_map_row(cell, cat_array, atrow) ; 

	col_end += col_dir ;
	/* Color polygons */
		if (! lines_only)
		for (atcol=col_beg; atcol!=col_end; atcol+=col_dir)
		{
			atcol_plus = atcol+col_dir ;
			if(! do_zero)
			{
					if (scr_x_1[atcol] == 0 && scr_y_1[atcol] == 0)
						continue ;
					if (scr_x_2[atcol] == 0 && scr_y_2[atcol] == 0)
						continue ;
					if (scr_x_1[atcol_plus] == 0 && scr_y_1[atcol_plus] == 0)
						continue ;
					if (scr_x_2[atcol_plus] == 0 && scr_y_2[atcol_plus] == 0)
						continue ;
			}
			if (cur_colr != cat_array[atcol])
			{
					R_color(cur_colr=cat_array[atcol]) ;
			}
			screen_x[0] = scr_x_1[atcol] ;
			screen_x[1] = scr_x_1[atcol_plus] ;
			screen_x[2] = scr_x_2[atcol_plus] ;
			screen_x[3] = scr_x_2[atcol] ;
			screen_y[0] = scr_y_1[atcol] ;
			screen_y[1] = scr_y_1[atcol_plus] ;
			screen_y[2] = scr_y_2[atcol_plus] ;
			screen_y[3] = scr_y_2[atcol] ;
			draw_polygon(screen_x, screen_y, 4) ;
		}
			
		if(line_color != -1)
		{
		/* Draw n-s hash marks */
			if (line_freq)
			{
				R_standard_color(line_color) ;
				for (atcol=col_beg; atcol!=col_end; atcol+=col_dir)
				{
					if ( !(atcol%line_freq) )
						draw_line(do_zero,scr_x_1[atcol], scr_y_1[atcol], scr_x_2[atcol], scr_y_2[atcol]) ;
				}
/*
				draw_line(do_zero,scr_x_1[atcol], scr_y_1[atcol], scr_x_2[atcol], scr_y_2[atcol]) ;
				draw_line(do_zero,scr_x_1[atcol+col_dir], scr_y_1[atcol+col_dir], scr_x_2[atcol+col_dir], scr_y_2[atcol+col_dir]) ;
*/
			}

		/* Draw e-w hash marks */

			if ((line_freq && !(atrow%line_freq)) || (atrow+row_dir == row_end))
			{
				R_standard_color(cur_colr = line_color) ;
				draw_polyline(scr_x_1, scr_y_1, window->cols) ;
			}

/*
			if (line_freq && (atrow+row_dir == row_end))
			{
				R_standard_color(cur_colr = line_color) ;
				draw_polyline(scr_x_2, scr_y_2, window->cols) ;
			}
*/
		}
		else
		{
		/* Draw n-s hash marks */
			if (line_freq)
			{
				for (atcol=col_beg; atcol!=col_end; atcol+=col_dir)
				{
					if ( !(atcol%line_freq) )
					{
						R_color(cat_array[atcol]) ;
						draw_line(do_zero,scr_x_1[atcol], scr_y_1[atcol], scr_x_2[atcol], scr_y_2[atcol]) ;
					}
				}
/*
				R_color(cat_array[atcol]) ;
				draw_line(do_zero,scr_x_1[atcol], scr_y_1[atcol], scr_x_2[atcol], scr_y_2[atcol]) ;
				R_color(cat_array[atcol+col_dir]) ;
				draw_line(do_zero,scr_x_1[atcol+col_dir], scr_y_1[atcol+col_dir], scr_x_2[atcol+col_dir], scr_y_2[atcol+col_dir]) ;
*/
			}

		/* Draw e-w hash marks */
			if ((line_freq && !(atrow%line_freq))
					|| (atrow+row_dir == row_end)
					|| (atrow == row_beg))
			{
				for (atcol=col_beg; atcol!=col_end; atcol+=col_dir)
				{
					R_color(cat_array[atcol]) ;
					draw_line(do_zero,scr_x_1[atcol], scr_y_1[atcol], scr_x_1[atcol+col_dir], scr_y_1[atcol+col_dir]) ;
				}
/*
				R_color(cat_array[atcol]) ;
				draw_line(do_zero,scr_x_1[atcol], scr_y_1[atcol], scr_x_1[atcol+col_dir], scr_y_1[atcol+col_dir]) ;
*/
			}

/*
			if (line_freq && (atrow+row_dir == row_end))
			{
				for (atcol=col_beg; atcol!=col_end; atcol+=col_dir)
				{
					R_color(cat_array[atcol]) ;
					draw_line(do_zero,scr_x_2[atcol], scr_y_2[atcol], scr_x_2[atcol+col_dir], scr_y_2[atcol+col_dir]) ;
				}
				R_color(cat_array[atcol]) ;
				draw_line(do_zero,scr_x_2[atcol], scr_y_2[atcol], scr_x_2[atcol+col_dir], scr_y_2[atcol+col_dir]) ;
			}
*/
		}
	}
	R_flush() ;

	printf("\n") ;
}

static
open_file(fd, file, file_mapset)
	int *fd ;
	char *file, *file_mapset ;
{
	char buffer[128] ;
	if ((*fd = G_open_cell_old(file, file_mapset)) == -1) 
	{
		sprintf(buffer, "Cell file [%s] in [%s] not available",
			file, file_mapset) ;
		G_fatal_error(buffer) ;
	}
}
