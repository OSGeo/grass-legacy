


/*  @(#)coll_pts.c    1.1  5/4/87  */

/*  
*   collect_points() - collect a set of points.
*    coll_a_pnt() -  collect a point from digitizer.
*/

#include "../../digit/digit.h"
#include "../../digit/globals.h"

/*  these correspond to the cursor keys */
#define	DIG_POINT    1
#define	STOP_DIG    2
#define	UPDATE_MON    3
#define	TOGGLE_MODE    4

collect_points(mode, type, n_points, xarr, yarr)
    int  mode,  type;
    int  *n_points;
    double **xarr, **yarr;
{
	int     Xraw;
	int     Yraw;
	int	looping ;
	int	Xlast ;
	int	Ylast ;
	int Error ;
	int pickup_point ;
	int stream_mode ;
	int	i ;

	double	*xptr ;
	double	*yptr ;
	char message[128] ;
	char achar ;
	char *dig_falloc() ;
	char *dig_frealloc() ;
	static int alloc_points = 0 ;
	static double *xarray, *yarray ;

	if (! alloc_points)
	{
		alloc_points     = 1000 ;
		xarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
		yarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	}

	Clear_info() ;
	if (mode == STREAM)
	{
		Write_info(1,"Place cursor on starting point") ;
		Write_info(2,"Hit <space> bar to start and stop digitizing") ;
		Get_curses_char(&achar) ;
	}

	D_clear_driver() ;

	xptr = xarray ;
	yptr = yarray ;

	Write_info(1, " # Points       Easting     Northing") ;

	if (mode == STREAM)
	{
		Error = D_readall (&Xraw, &Yraw) ;
		transform_a_into_b ((double) Xraw, (double) Yraw, xptr, yptr) ;
		Xlast = Xraw ;
		Ylast = Yraw ;

		*n_points = 1 ;
		Write_info(1, " # Points       Easting     Northing") ;
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
		Write_info(2, message) ;
		xptr++ ;
		yptr++ ;
		Write_info(4, " STREAM mode") ;
	}
	else
	{
		Xlast = 0.0 ;
		Ylast = 0.0 ;
		*n_points = 0 ;
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, 0.0, 0.0);
		Write_info(2, message) ;
		Write_info(4, " POINT mode") ;
	}

	looping = 1 ;
	stream_mode = 0 ;
	set_keyboard() ;

	while ( looping)
	{
		pickup_point = 0 ;

		if (key_hit(message))
		{
			if (*message == ' ')
			{
				looping = 0 ;
				continue ;
			}

		    /*  want to toggle mode of digitizing  */
			if (*message == 'm')
			{
				if (mode == POINT)
				{
					mode = STREAM ;
					Write_info(4, " STREAM mode") ;
				}
				else
				{
					mode = POINT ;
					Write_info(4, " POINT mode") ;
				}
			}
			else
			if (*message == 'u')
			{
				Write_info(2, "    Updating  MONITOR") ;
				plot_points( type, *n_points, xarray, yarray,
					CLR_HIGHLIGHT, 0);
				Write_info(2, "    finished: continue  digitizing...") ;
				continue ;
			}
			else
			if (*message == 'p')
			{
				pickup_point = 1 ;
			}
				
		}

		if (mode == STREAM)
			stream_mode = 1 ;

	/* get string of all info from * digitizer */
		Error = D_readall (&Xraw, &Yraw) ;
		if (Error == -1)
		{
			printf("Digitizer read error:  quitting\n") ;
			close_down(-1) ;
		}

		if (mode == POINT && pickup_point == 0)
			continue ;

		if (abs(Xraw - Xlast) < 2 && abs(Yraw - Ylast) < 2)
			continue ;
		
		Xlast = Xraw ;
		Ylast = Yraw ;

		if (*n_points + 1 == alloc_points)
		{
			int old;

			old = alloc_points;
			alloc_points += 200 ;
			xarray = (double *)dig_frealloc((char *)xarray, alloc_points, sizeof(double), old);
			yarray = (double *)dig_frealloc((char *)yarray, alloc_points, sizeof(double), old);
			xptr = xarray + *n_points ;
			yptr = yarray + *n_points ;
		}

	    /*  convert raw coor. to utm coor  */
		transform_a_into_b ((double) Xraw, (double) Yraw, xptr, yptr) ;
		(*n_points)++ ;

	    /*  update the monitor for each new point  */
		if (mode == POINT)
				
		{
		/*  blot the 1st node, but after that its a line  */
	    	if (*n_points == 1)
				plot_points( type, 1, xptr, yptr,
			    	CLR_HIGHLIGHT, CLR_HIGHLIGHT);
	    	else
				plot_points( type, *n_points, xarray, yarray,
			    	CLR_HIGHLIGHT, 0);

		}
				
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
		Write_info(2, message) ;
		xptr++ ;
		yptr++ ;

	}

	unset_keyboard() ;


    /*  black at the line on the monitor  exactly as it is now  */
    plot_points( type, 1, xarray, yarray, CLR_ERASE, CLR_ERASE);
    plot_points( type, *n_points, xarray, yarray, CLR_ERASE, CLR_ERASE);

	*xarr = xarray ;
	*yarr = yarray ;
	return(stream_mode) ;

}

