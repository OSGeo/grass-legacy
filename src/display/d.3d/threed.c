#include <stdio.h>
#include "display.h"
#include "D.h"
#include "raster.h"
#include "gis.h"
#include "3d.h"
#include "options.h"
#include "l_proto.h"

static DCELL *cell_array ;
static int *color_buf;
static int cell ;

int threed(int interactive)
{
	int col_beg = 0;      /* western  edge denoting window (array) */
	int col_end = 0;      /* eastern  edge denoting window (array) */
	int row_beg = 0;      /* southern edge denoting window (array) */
	int row_end = 0;      /* northern edge denoting window (array) */
	char buffer[128] ;
	int row_dir = 0;
	int col_dir = 0;
	int break_row ;
	DCELL min, max ;

	struct Colors colors ;

	if (box_color != -1)
	{
		get_range(&min, &max) ;
		R_standard_color(box_color) ;
		draw_north_face(min, max) ;
		draw_south_face(min, max) ;
		draw_east_face(min, max) ;
		draw_west_face(min, max) ;
	}

	/* Set the colors for the display */
	if (G_read_colors(file, file_mapset, &colors) == -1)
	{
		sprintf(buffer,"Color file for [%s] not available", file) ;
		G_fatal_error(buffer) ;
	}

	D_set_colors (&colors);

	/* Open the necessary cell files */
	if ((cell = G_open_cell_old(file, file_mapset)) == -1)
	{
		char buffer[128] ;
		sprintf(buffer, "Raster file [%s] in [%s] not available",
		    file, file_mapset) ;
		G_fatal_error(buffer) ;
	}

	/* Allocate space for arrays */
	cell_array   = G_allocate_d_raster_buf() ;
	color_buf    = (int *) G_allocate_c_raster_buf() ;

	/* Set up for drawing map */

	switch (direction)
	{
	case NORTH_WEST:
	case WEST_NORTH:
		row_beg = window.rows - 2 ;
		row_end = 1 ;
		row_dir = -1 ;
		col_beg = window.cols - 2 ;
		col_end = 1 ;
		col_dir = -1 ;
		/*
		fprintf (stdout,"View from North West\n") ;
		*/
		break ;

	case SOUTH_WEST :
	case WEST_SOUTH :
		row_beg = 1 ;
		row_end = window.rows - 2 ;
		row_dir = 1 ;
		col_beg = window.cols - 2 ;
		col_end = 1 ;
		col_dir = -1 ;
		/*
		fprintf (stdout,"View from South West\n") ;
		*/
		break ;

	case NORTH_EAST:
	case EAST_NORTH:
		row_beg = window.rows - 2 ;
		row_end = 1 ;
		row_dir = -1 ;
		col_beg = 1 ;
		col_end = window.cols - 2 ;
		col_dir = 1 ;
		/*
		fprintf (stdout,"View from North East\n") ;
		*/
		break ;

	case SOUTH_EAST :
	case EAST_SOUTH :
		row_beg = 1 ;
		row_end = window.rows - 2 ;
		row_dir = 1 ;
		col_beg = 1 ;
		col_end = window.cols - 2 ;
		col_dir = 1 ;
		/*
		fprintf (stdout,"View from South East\n") ;
		*/
		break ;
	}

	initialize_arrays() ;

	/* Provide for plotting from north and south if necessary */
	break_row = (int)((window.north - from_northing) / window.ns_res + .5);


	if (((break_row < row_end) && (break_row > row_beg)) ||
	    ((break_row > row_end) && (break_row < row_beg)))
	{
		if (interactive)
		{
			setbuf(stdout,0);
			fprintf (stdout,"\nRows %d to %d at:    "   ,row_end, break_row);
		}
		do_plot(row_end, break_row + row_dir, -row_dir,col_beg,
			col_end,col_dir,interactive,&colors) ;

		if (interactive)
		{
			setbuf(stdout,0);
			fprintf (stdout,"\nRows %d to %d at:    "   ,row_beg,break_row);
		}
		do_plot(row_beg, break_row, row_dir,col_beg,
			col_end,col_dir,interactive,&colors) ;
	}
	else
	{
		if (interactive)
		{
			setbuf(stdout,0);
			fprintf (stdout,"\nRows %d to %d at:    "   ,row_beg, row_end);
		}
		do_plot(row_beg,row_end,row_dir,col_beg,col_end,col_dir,
			interactive,&colors) ;
	}

	if (box_color != -1)
	{
		R_standard_color(box_color) ;
		if (from_northing > window.north)
			draw_north_face(min, max) ;
		if (from_northing < window.south)
			draw_south_face(min, max) ;
		if (from_easting > window.east)
			draw_east_face(min, max) ;
		if (from_easting < window.west)
			draw_west_face(min, max) ;
	}

	R_flush() ;

	de_initialize_arrays() ;
	G_close_cell(cell) ;
	G_free_colors (&colors);
	G_free(cell_array);
	G_free (color_buf);

	return 0;
}

int do_plot(
      int row_beg,int row_end,int row_dir,
      int col_beg,int col_end,int col_dir,
      int interactive, struct Colors *colors)
{
	int cur_colr ;
	int screen_x[4] ;
	int screen_y[4] ;

	CELL *scr_x1 ;
	CELL *scr_y1 ;
	CELL *scr_x2 ;
	CELL *scr_y2 ;
	int *scr_x_1 ;
	int *scr_x_2 ;
	int *scr_y_1 ;
	int *scr_y_2 ;
	int atcol, atrow ;
	int atcol_plus ;
	int line_col_end ;

	if (interactive)
	    set_signals() ;

	/* Now draw the map */
	cur_colr = 0 ;
	line_col_end = col_end + col_dir ;
	for (atrow=row_beg; ; atrow+=row_dir)
	{
		if(interactive && check_signal())
			break ;

		if(interactive)
			fprintf (stdout,"\b\b\b\b%4d", atrow) ;

		G_get_d_raster_row(cell, cell_array, atrow) ;
		D_lookup_d_raster_colors (cell_array, color_buf, window.cols, colors);

		get_corners(atrow, &scr_x1, &scr_y1, &scr_x2, &scr_y2) ;

		scr_x_1 = scr_x1 ;   
		scr_y_1 = scr_y1 ;
		scr_x_2 = scr_x2 ;   
		scr_y_2 = scr_y2 ;

		/* Color polygons */
		if (! lines_only)
		{
			R_color(cur_colr=color_buf[col_beg]) ;
			for (atcol=col_beg; atcol!=line_col_end; atcol+=col_dir)
			{
				atcol_plus = atcol-1 ;
				if(! do_null)
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
				if (cur_colr != color_buf[atcol])
				{
					R_color(cur_colr=color_buf[atcol]) ;
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
		}

		if(line_color != -1)
			R_standard_color(line_color) ;
		/* Draw n-s hash marks */
		if (line_freq)
		{
			for (atcol=col_beg; atcol!=line_col_end; atcol+=col_dir)
			{
				if ( !(atcol%line_freq)
				    || (atcol == col_beg))
				{
					if(line_color == -1)
						R_color(color_buf[atcol]) ;
					draw_line(do_null,
					    scr_x_1[atcol],
					    scr_y_1[atcol],
					    scr_x_2[atcol],
					    scr_y_2[atcol]) ;
				}
				if (atcol == col_end)
				{
					if(line_color == -1)
						R_color(color_buf[atcol+col_dir]) ;
					draw_line(do_null,
					    scr_x_1[atcol+col_dir],
					    scr_y_1[atcol+col_dir],
					    scr_x_2[atcol+col_dir],
					    scr_y_2[atcol+col_dir]) ;
				}
			}
		}

		/* Draw e-w hash marks */
		if ((line_freq && !(atrow%line_freq))
		    || (atrow+row_dir == row_beg))
		{
			if (row_dir == 1)
				for (atcol=0; atcol!=window.cols-2; atcol++)
				{
					if(line_color == -1)
						R_color(color_buf[atcol]) ;
					draw_line(do_null,
						scr_x_1[atcol],
						scr_y_1[atcol],
						scr_x_1[atcol+1],
						scr_y_1[atcol+1]) ;
				}
			else
				for (atcol=1; atcol!=window.cols-2; atcol++)
				{
					if(line_color == -1)
						R_color(color_buf[atcol]) ;
					draw_line(do_null,
						scr_x_2[atcol],
						scr_y_2[atcol],
						scr_x_2[atcol+1],
						scr_y_2[atcol+1]) ;
				}
		}

		/* Draw e-w hash marks on last row */
		if (atrow == row_end)
		{
			if (row_dir == 1)
				for (atcol=0; atcol!=window.cols-2; atcol++)
				{
					if(line_color == -1)
						R_color(color_buf[atcol]) ;
					draw_line(do_null,
						scr_x_2[atcol],
						scr_y_2[atcol],
						scr_x_2[atcol+1],
						scr_y_2[atcol+1]) ;
				}
			else
				for (atcol=1; atcol!=window.cols-2; atcol++)
				{
					if(line_color == -1)
						R_color(color_buf[atcol]) ;
					draw_line(do_null,
						scr_x_1[atcol],
						scr_y_1[atcol],
						scr_x_1[atcol+1],
						scr_y_1[atcol+1]) ;
				}
		}
		if (atrow == row_end)
			break ;
	}

/*	fprintf (stdout,"\n") ;*/      /*removed for TclTKGRASS M.N. 7/98 */

	return 0;
}
