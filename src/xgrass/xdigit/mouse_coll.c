/*  @(#)coll_pts.c	1.1  5/4/87  */
#include "digit.h"

/*  
*   collect_points() - collect a set of points.
*	coll_a_pnt() -  collect a point from digitizer.
*/


/*  these correspond to the cursor keys */

static mouse_get_point ();

mouse_collect_points (mode, type, Points)
	int mode;
	char type ;
	struct line_pnts *Points;
{
    int     Xraw;
    int     Yraw;
    int		Xlast ;
    int		Ylast ;
    int		button ;
    int		loop ;
    int stream_mode ;
    int run_mode ;
    double	*xptr ;
    double	*yptr ;
    char message[128] ;
    int *n_points ;
    int cnt;
    if (Points->alloc_points == 0)
    {
	dig_alloc_points (Points, 500);
    }
    n_points = &(Points->n_points);
    mode = POINT;
    run_mode = mode ;
    xptr = Points->x ;
    yptr = Points->y ;


    write_info(1, " # Points       Easting     Northing") ;
    write_info(2, " POINT mode ") ;

    {
    Xlast = 0 ;
    Ylast = 0 ;
    *n_points = 0 ;
    stream_mode = 0 ;
    sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, 0.0, 0.0);
    add_info(1, message) ;
    }

    loop = 1 ;

    Xraw = Yraw = 100;

    cnt = 0;
    /*  digitizing loop  */
    if (type != DOT)
    {
    	show_select_dialog ("back up 1 point", "done", "Digitize a point", 0) ;
    }
    /*
    else
    	show_select_dialog (NULL, "abort", "Digitize a point", 0) ;
	*/
    while (loop)
    {
	if (type == DOT)		/* scs added code */
	{
	    get_location_with_pointer ( &Xraw, &Yraw, &button);
	}
	else
	{
	    button = mouse_get_point (&Xraw, &Yraw, *n_points);
	}

	switch (button) {
	    case DONE:
		XFlush (dpy);
		if (type == DOT)
		    return (mode);
		loop = 0 ;
/*-->*/		if (*n_points > 0 ) /*ADDED*/
		    write_info(1, " processing..") ;
		continue ;
		break ;

	    case BACKUP:
		if (type == DOT)	/* abort */
		    return (mode);
		if (*n_points <= 0)
		{
		    BEEP;
		    *n_points = 0; /* just in case */
		    continue;
		}
		
		(--(*n_points));
		xptr--;
		yptr--;
		if (*n_points)
		    utm_to_screen (*(xptr-1), *(yptr-1), &Xraw, &Yraw);
		else
		    utm_to_screen (*(xptr), *(yptr), &Xraw, &Yraw);
		switch (*n_points) {
		    case 0:
			plot_points( type, 1, xptr, yptr, CLR_ERASE, CLR_ERASE);
			break;
		    case 1:
			plot_points( type, 2, xptr-1, yptr-1, CLR_ERASE, 0);
			plot_points( type, 1, xptr-1, yptr-1, 
						    CLR_0_NODE, CLR_0_NODE);
			break;
		    default:
			plot_points( type, 2, xptr-1, yptr-1, CLR_ERASE, 0);
			break;
		}
		continue ;
		break ;

	    case DIG_POINT:
		break ;
	    default:
		break ;

	}		/*  end of switch  */

	if (run_mode == POINT  && button != FIND )
	continue ;


	/*  digitizer sitting in the same place  */
	if (*n_points)
	{
	    if ( Xlast == Xraw  &&  Ylast == Yraw)
		continue ;
	}

	Xlast = Xraw ;
	Ylast = Yraw ;

	/* 
	** + 2 added for DOT
	*/
	if ((*n_points) + 2 >= Points->alloc_points)
	{
	    dig_alloc_points (Points, Points->alloc_points + 50);

	    xptr = Points->x + *n_points ;
	    yptr = Points->y + *n_points ;
	}

	/*  convert raw coor. to utm coor.  */
	screen_to_utm (Xraw, Yraw, xptr, yptr) ;
	(*n_points)++ ;
	
	/*  update the monitor for each new point  */
	if (run_mode == POINT)
	{
	    /*  blot the 1st node, but after that its a line  */

	    if (*n_points == 1)
		plot_points( type, 1, xptr, yptr, CLR_HIGHLIGHT, CLR_HIGHLIGHT);
	    else
		plot_points( type, *n_points, Points->x, Points->y, CLR_HIGHLIGHT, 0);
	}



	/*  show user coor. of new point  */
	sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
    	write_info(1, " # Points       Easting     Northing") ;
	add_info(1, message) ;

	if (type == DOT)
	{
	    loop = 0;
	    Points->x[1] = Points->x[0];
	    Points->y[1] = Points->y[0];
	    (*n_points)++ ;
	}


	/*  advance pointers for the next set of coor. */
	xptr++ ;
	yptr++ ;

    }
    write_info(2, " ") ;


    /*  black at the line on the monitor  exactly as it is now  */
    plot_points( type, 1, Points->x, Points->y, CLR_ERASE, CLR_ERASE);
    if (*n_points)
	plot_points( type, *n_points, Points->x, Points->y, CLR_ERASE, CLR_ERASE);


    return(stream_mode) ;

}	/*  coll_pts()  */




/* x = y = 0.0  on abort.  */
static
mouse_get_point (screen_x, screen_y, cnt)
    int    *screen_x, *screen_y;
    int cnt;
{
    int  button;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    char header[50];
    int prev;


    sprintf  (header, "Point number %d\n", cnt);

    if (cnt)
	get_location_with_line (*screen_x, *screen_y, screen_x, screen_y, &button);
    else
	get_location_with_pointer ( screen_x, screen_y, &button);

    return (button);


    /*
    screen_to_utm(screen_x, screen_y, &ux2, &uy2);

    sprintf(buffer," EAST:  %10.2f", ux1>ux2?ux1:ux2);
	write_info(1, buffer);
    sprintf(buffer," NORTH: %10.2f", uy1>uy2?uy1:uy2);
	add_info(1, buffer);
	*/
}
