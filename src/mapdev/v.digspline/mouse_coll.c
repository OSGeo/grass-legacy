/*  @(#)coll_pts.c	1.1  5/4/87  */
#include "digit.h"

/*  
*   collect_points() - collect a set of points.
*	coll_a_pnt() -  collect a point from digitizer.
*/


/*  these correspond to the cursor keys */
#define		DIG_POINT	1
#define		BACKUP		2
#define		STOP_DIG	3

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

    Clear_info() ;

    Write_info(1, " # Points       Easting     Northing") ;

    {
    Xlast = 0 ;
    Ylast = 0 ;
    *n_points = 0 ;
    stream_mode = 0 ;
    sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, 0.0, 0.0);
    Write_info(2, message) ;
    Write_info(4, " POINT mode ") ;
    }

    loop = 1 ;

    Xraw = Yraw = 100;

    cnt = 0;
    /*  digitizing loop  */
    while (loop)
    {
/*-->*/ if (type == DOT)		/* scs added code */
	{
	   Write_base(12, "    Buttons:") ;
	   Write_base(13, "       left:  Digitize a site") ;
	   Write_base(14, "       Middle:  Abort/Quit") ;
	   Write_base(15, "       Right:   Abort/Quit") ;

	   Write_info(2, "") ;

/*	   button = mouse_get_point (&Xraw, &Yraw, *n_points);*/
/*-->*/	   R_get_location_with_pointer ( &Xraw, &Yraw, &button); /*ADDED*/
/*-->*/	   flush_keyboard (); /*ADDED*/
/*-->*/	}
	else
	    button = mouse_get_point (&Xraw, &Yraw, *n_points);


	switch (button) {
	    case STOP_DIG:
		if (type == DOT)
		    return (mode);
		loop = 0 ;
/*-->*/		if (*n_points > 0 ) /*ADDED*/
		    Write_info(3, " processing..") ;
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
		
		Write_info(2, "") ;
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
			plot_points( type, 1, xptr-1, yptr-1, CLR_0_NODE, CLR_0_NODE);
			break;
		    default:
			plot_points( type, 2, xptr-1, yptr-1, CLR_ERASE, 0);
			break;
		}
		continue ;
		break ;

	    case DIG_POINT:
	    default:
		break ;

	}		/*  end of switch  */

	if (run_mode == POINT  && button != DIG_POINT )
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
	Write_info(2, message) ;

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

    _Clear_base ();
    _Write_base(10, header);
    _Write_base(12, "    Buttons:                                    ");
    _Write_base(13, "       Left:   Mark a point                     ");
    if (cnt)
	_Write_base(14, "       Middle: Backup one point                 ");
    else
	_Write_base(14, "       Middle: (Backup one point)               ");
    Write_base (15, "       Right:  Quit digitizing                  ");


    if (cnt)
	R_get_location_with_line (*screen_x, *screen_y, screen_x, screen_y, &button);
    else
	R_get_location_with_pointer ( screen_x, screen_y, &button);

    flush_keyboard ();
    return (button);


    /*
    screen_to_utm(screen_x, screen_y, &ux2, &uy2);

    sprintf(buffer," EAST:  %10.2f", ux1>ux2?ux1:ux2);
	Write_info(3, buffer);
    sprintf(buffer," NORTH: %10.2f", uy1>uy2?uy1:uy2);
	Write_info(4, buffer);
	*/
}
