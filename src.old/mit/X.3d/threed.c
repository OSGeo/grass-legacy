/* @(#)threed.c	2.1   10/1/87 */

#include <stdio.h>
#include "gis.h"
#include "3d.h"
#include "options.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>

static CELL *cat_array ;
static CELL *elv_array_1 ;
static CELL *elv_array_2 ;
static int cell ;
static int elev ;

threed(interactive)
{
    int col_beg    ;      /* western edge denoting window  (array)           */
    int col_end    ;      /* eastern edge denoting window  (array)           */
    int row_beg    ;      /* southern edge denoting window (array)           */
    int row_end    ;      /* northern edge denoting window (array)           */
    char buffer[128];
    int row_dir;
    int col_dir ;
    int break_row ;


/* Open the necessary cell files */
    if ((cell = G_open_cell_old(file, file_mapset)) == -1) 
    {
	sprintf(buffer, "Cell file [%s in %s] not available",
		file, file_mapset) ;
	G_fatal_error(buffer) ;
    }

    if ((elev = G_open_cell_old(elevfile, elevfile_mapset)) == -1) 
    {
	G_fatal_error(buffer) ;
    }

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
	do_plot(row_beg,break_row+row_dir,row_dir,col_beg,col_end,col_dir,interactive) ;
	do_plot(row_end,break_row-row_dir,-row_dir,col_beg,col_end,col_dir,interactive) ;
    }
    else
    {
	do_plot(row_beg,row_end,row_dir,col_beg,col_end,col_dir,interactive) ;
    }

    free (cat_array);
    free (elv_array_1);
    free (elv_array_2);
    G_close_cell (cell);
    G_close_cell (elev);
}

do_plot(row_beg,row_end,row_dir,col_beg,col_end,col_dir,interactive)
{
    int cur_colr ;
    int screen_x[4] ;
    int screen_y[4] ;
    XPoint p_arr[4];
    extern Display *the_display;
    extern Window the_window;
    extern GC the_gc;

/* Jim - these should be allocated, based on the window */
    int scr_x1[2048] ;
    int scr_y1[2048] ;
    int scr_x2[2048] ;
    int scr_y2[2048] ;
    int *scr_x_1 ;
    int *scr_x_2 ;
    int *scr_y_1 ;
    int *scr_y_2 ;
    char buffer[128] ;
    char flip ;
    int atcol, atrow ;

    set_signals() ;

/* Now draw the map */
    G_get_map_row_nomask (elev,elv_array_1,row_beg);
    Screen_calc(elv_array_1, exag, scr_x1, scr_y1, row_beg, &window) ;

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
		Screen_calc(elv_array_1, exag, scr_x1, scr_y1, atrow, &window) ;
		scr_x_1 = scr_x2 ;   scr_y_1 = scr_y2 ;
		scr_x_2 = scr_x1 ;   scr_y_2 = scr_y1 ;
		flip = 0 ;
		break ;
	case 0:
		G_get_map_row_nomask (elev, elv_array_2, atrow+row_dir) ; 
		Screen_calc(elv_array_2, exag, scr_x2, scr_y2, atrow, &window) ;
		scr_x_1 = scr_x1 ;   scr_y_1 = scr_y1 ;
		scr_x_2 = scr_x2 ;   scr_y_2 = scr_y2 ;
		flip = 1 ;
		break ;
	}

	G_get_map_row(cell, cat_array, atrow) ; 

/* R_color polygons */
	for (atcol=col_beg; atcol!=col_end; atcol+=col_dir)
	{
	    if (cur_colr != cat_array[atcol])
	    {
	XSetForeground(the_display, the_gc, cat_array[atcol]);
	    }
	    p_arr[0].x  = (short) scr_x_1[atcol] ; 
	    p_arr[1].x  = (short) scr_x_1[atcol+col_dir] ;
	    p_arr[2].x  = (short) scr_x_2[atcol+col_dir] ;
	    p_arr[3].x  = (short) scr_x_2[atcol] ;
	    p_arr[0].y  = (short) scr_y_1[atcol] ;
	    p_arr[1].y  = (short) scr_y_1[atcol+col_dir] ;
	    p_arr[2].y  = (short) scr_y_2[atcol+col_dir] ;
	    p_arr[3].y  = (short) scr_y_2[atcol] ;

	XFillPolygon( the_display, the_window, the_gc,
			&p_arr[0], 4, Complex,
			CoordModeOrigin);
		
	}
    
/* Draw n-s hash marks */
/*
	XSetForeground(the_display, the_gc, 0);
	for (atcol=col_beg; atcol!=col_end; atcol+=col_dir)
	{
	    if ( line_freq && !(atcol%line_freq))
		draw_line(scr_x_1[atcol], scr_y_1[atcol], scr_x_2[atcol], scr_y_2[atcol]) ;
	}
	*/

/* Draw e-w hash marks */
	/*
	if ( line_freq && !(atrow%line_freq))
	{
	    R_standard_color(cur_colr = black) ;
	    draw_polyline(scr_x_1+0, scr_y_1+0, window.cols - 1) ;
	}
	*/
    }

    XFlush(the_display) ;

    printf("\n") ;
}
