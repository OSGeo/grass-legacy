/*  @(#)coll_pts.c	2.1  6/26/87  */

/*  
*   collect_points() - collect a set of points.
*	coll_a_pnt() -  collect a point from digitizer.
*/

#include "../../v.digit/digit.h"
#include "../../v.digit/globals.h"

#include <sgtty.h>

collect_points(mode, type, n_points, xarr, yarr)
	int mode, type ;
	int *n_points ;
	double **xarr, **yarr ;
{
	int     Xraw;
	int     Yraw;
	int     KpdChar;
	int     KpdStat;
	int     FtswStat;
	int		KeyHit ;
	int		Xlast ;
	int		Ylast ;
	int Error ;
	int stream_mode ;
	int run_mode ;
	int save_mode ;

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

	if (type == DOT)
	{
	    run_mode = POINT ;
	    save_mode = mode;
	}
	else
	    run_mode = mode ;


	Clear_info() ;
	if (run_mode == STREAM)
	{
		Write_info(1,"Place cursor on starting point") ;
		Write_info(2,"Hit <space> bar to start and stop digitizing") ;
		Get_curses_char(&achar) ;
	}

	xptr = xarray ;
	yptr = yarray ;

	Write_info(1, " # Points       Easting     Northing") ;

	if (run_mode == STREAM)
	{
		Error = D_readall (&Xraw, &Yraw, &FtswStat, &KpdStat, &KpdChar) ;
		Error = D_readall (&Xraw, &Yraw, &FtswStat, &KpdStat, &KpdChar) ;
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

	KeyHit = 0 ;
	stream_mode = 0 ;

	while (! KeyHit)
	{

		if (ioctl (0, FIONREAD, &KeyHit) < 0)
		{
			printf ("\nioctl failed");
			close_down (-1);
		}
		if (KeyHit)
		{
			Get_curses_char(message) ;

			if (*message != ' ')
				KeyHit = 0 ;
		
		    /*  want to toggle mode of digitizing  */
			if (*message == 'm')
			{
			    if (type != DOT)
				if (run_mode == POINT)
				{
					run_mode = STREAM ;
					Write_info(4, " STREAM mode") ;
				}
				else
				{
					run_mode = POINT ;
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
				
		}

		if (run_mode == STREAM)
			stream_mode = 1 ;

	/* get string of all info from * digitizer */
		Error = D_readall (&Xraw, &Yraw, &FtswStat, &KpdStat, &KpdChar) ;
		if (Error == -1)
		{
			printf("Digitizer read error:  quitting\n") ;
			close_down(-1) ;
		}

		if (run_mode == POINT && FtswStat == 0)
			continue ;

		if (abs(Xraw - Xlast) < 2 && abs(Yraw - Ylast) < 2)
			continue ;
		
		Xlast = Xraw ;
		Ylast = Yraw ;

		/* +2 added for DOT */
		if (*n_points + 2 == alloc_points)
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

		if (type == DOT)
		{
		    KeyHit = ' ';  /* cause break in while */
		    xarray[1] = xarray[0];
		    yarray[1] = yarray[0];
		    (*n_points)++;
		}


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
				
		sprintf(message, "   %6d   %12.2lf %12.2lf", *n_points, *xptr, *yptr);
		Write_info(2, message) ;
		xptr++ ;
		yptr++ ;

	}


    /*  black at the line on the monitor  exactly as it is now  */
    plot_points( type, 1, xarray, yarray, CLR_ERASE, CLR_ERASE);
    plot_points( type, *n_points, xarray, yarray, CLR_ERASE, CLR_ERASE);

	*xarr = xarray ;
	*yarr = yarray ;

	if (type == DOT)
	    return (save_mode);
	return(stream_mode) ;

}	/*  coll_pts()  */


