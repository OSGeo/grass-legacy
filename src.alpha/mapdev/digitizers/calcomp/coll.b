/*  @(#)coll_pts.c	1.1  5/4/87  */

/*  
*   collect_points() - collect a set of points.
*	coll_a_pnt() -  collect a point from digitizer.
*/


#include "../../digit/digit.h"
#include "../../digit/globals.h"

/*  these correspond to the cursor keys */
#define		DIG_POINT	1
#define		STOP_DIG	2
#define		UPDATE_MON	3
#define		TOGGLE_MODE	4

collect_points(mode, type, n_points, xarr, yarr)
	int mode, type ;
	int *n_points ;
	double **xarr, **yarr ;
{
	int     Xraw;
	int     Yraw;
	int		Xlast ;
	int		Ylast ;
	int		KeyHit ;
	int		last_key ;
	int		loop ;
	int stream_mode ;
	int run_mode ;
	double	*xptr ;
	double	*yptr ;
	char message[128] ;
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

	run_mode = mode ;
	xptr = xarray ;
	yptr = yarray ;

	Clear_info() ;

/*  delay a heartbeat whilst they take finger off the keys, clear digitizer  */
	delay(5) ;
	D_flush() ;

	Write_info(1, " # Points       Easting     Northing") ;

	if (run_mode == STREAM)
	{

		while ( D_readall (&Xraw, &Yraw) < 0)
			D_ask_if_err() ;
		transform_a_into_b ((double) Xraw, (double) Yraw, xptr, yptr) ;
		Xlast = Xraw ;
		Ylast = Yraw ;

		*n_points = 1 ;
		stream_mode = 1 ;
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
		Write_info(2, message) ;
		xptr++ ;
		yptr++ ;
		Write_info(4, " STREAM mode ") ;
	}
	else
	{
		Xlast = 0.0 ;
		Ylast = 0.0 ;
		*n_points = 0 ;
		stream_mode = 0 ;
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, 0.0, 0.0);
		Write_info(2, message) ;
		Write_info(4, " POINT mode ") ;
	}

	loop = 1 ;
	last_key = -1 ;

/*  digitizing loop  */
	while (loop)
	{

		while ( (KeyHit = D_readall (&Xraw, &Yraw)) < 0)
			D_ask_if_err() ;


	/* in the space of a heartbeat that the user has held 
	*  the cursor key down, the program has read the cursor
	*  two or three times.  to keep from un-doing what they just did
	*  check the last cursor key that was hit.
	*/

		if (KeyHit == last_key)
			continue ;
		else
			last_key = -1 ;


	/*  hit a cursor key  */
		if (KeyHit)
		{

			switch (KeyHit)
			 {
				case STOP_DIG:
					loop = 0 ;
					Write_info(3, " proccessing..") ;
					continue ;
					break ;

				case TOGGLE_MODE:
					if (run_mode == POINT)
					{
						stream_mode = 1 ;
						run_mode = STREAM ;
						Write_info(4, " STREAM mode ") ;
					}
					else
					{
						run_mode = POINT ;
						Write_info(4, " POINT mode ") ;
					}

					last_key = KeyHit ;
					break ;

				case UPDATE_MON:
					Write_info(2, "    Updating  MONITOR") ;
					plot_points( type, *n_points, xarray, yarray,
						CLR_HIGHLIGHT, 0);
					Write_info(2, "    finished: continue  digitizing...") ;
					last_key = KeyHit ;
					continue ;
					break ;

				default:
					last_key = KeyHit ;
					break ;

			 }		/*  end of switch  */
		}

		if (run_mode == POINT  && KeyHit != DIG_POINT )
			continue ;


		if (abs(Xraw - Xlast) < 2 && abs(Yraw - Ylast) < 2)
			continue ;
		
		Xlast = Xraw ;
		Ylast = Yraw ;

		if (*n_points + 1 == alloc_points)
		{
			alloc_points += 100 ;
			xarray = (double *)dig_frealloc((char *)xarray, alloc_points, sizeof(double));
			yarray = (double *)dig_frealloc((char *)yarray, alloc_points, sizeof(double));
			xptr = xarray + *n_points ;
			yptr = yarray + *n_points ;
		}

	    /*  convert raw coor. to utm coor.  */
		transform_a_into_b ((double) Xraw, (double) Yraw, xptr, yptr) ;
		(*n_points)++ ;

	    /*  update the monitor for each new point  */
		if (run_mode == POINT)
		{
		/*  blot the 1st node, but after that its a line  */

			if (*n_points == 1)
				plot_points( type, 1, xptr, yptr,
				    CLR_HIGHLIGHT, CLR_HIGHLIGHT);
			else
				plot_points( type, *n_points, xarray, yarray,
				    CLR_HIGHLIGHT, 0);
		}
				

	    /*  show user coor. of new point  */
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
		Write_info(2, message) ;

	    /*  advance pointers for the next set of coor. */
		xptr++ ;
		yptr++ ;

	}


    /*  black at the line on the monitor  exactly as it is now  */
    plot_points( type, 1, xarray, yarray, CLR_ERASE, CLR_ERASE);
    plot_points( type, *n_points, xarray, yarray, CLR_ERASE, CLR_ERASE);


    /*  repoint the passed pointers */
	*xarr = xarray ;
	*yarr = yarray ;

	return(stream_mode) ;

}	/*  coll_pts()  */


/*

coll_a_pnt ( x, y)
	double	*x ;
	double	*y ;
{

	int  KeyHit ;
	int  Xraw, Yraw ;

	double  X, Y ;

	D_flush() ;

	while ( (KeyHit = D_readall (&Xraw, &Yraw)) < 0)
        {
                        printf("calling D_ask_if_err\n");
			D_ask_if_err() ;
        }
	
	transform_a_into_b ((double) Xraw, (double) Yraw, &X, &Y) ;

	*x = X ;
	*y = Y ;

	return(KeyHit) ;

}		
*/
