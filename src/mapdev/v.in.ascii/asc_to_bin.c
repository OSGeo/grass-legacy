/*  @(#)asc_to_bin.c	2.1  6/26/87  */

/*
*  May need some changes for 3.0 digit files.  POINT type
*/

#include <stdio.h>
#include "Vect.h"
#include "Vect.h"
#include "gis.h"

#define BUFFSIZE 128

int asc_to_bin(
	FILE *ascii ,
	struct Map_info *Map)
{
	char ctype ;
	char buff[BUFFSIZE] ;
	double *xarray ;
	double *yarray ;
	double *x, *y ;
	int n_points ;
	int type ;
	int alloc_points ;
	int end_of_file ;
	struct line_pnts *Points;

	/* Must always use this to create an initialized  line_pnts structure */
	Points = Vect_new_line_struct ();

	end_of_file = 0 ;
	alloc_points     = 1000 ;
	xarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	yarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;

	if (NULL == fgets(buff,BUFFSIZE,ascii))
	    return 0;

	for(;;)
	{
the_switch:
		sscanf(buff, "%1c", &ctype) ;
		switch(ctype)
		{
		case 'A':
			type = AREA ;
			break ;
		case 'L':
			type = LINE ;
			break ;
		case 'P':
			type = DOT ;
			break ;
		case 'a':
			type = DEAD_AREA ;
			break ;
		case 'l':
			type = DEAD_LINE ;
			break ;
		case 'p':
			type = DEAD_DOT ;
			break ;
		case 'E': case 'e':
			return 0;
		default:
			fprintf (stdout,"?? %s\n", buff) ;
			if (NULL == fgets(buff,BUFFSIZE,ascii))
				return 0;
			goto the_switch ;
		}

	/* Collect the points */
		n_points = 0 ;
		x = xarray ;
		y = yarray ;
		if (NULL == fgets(buff,BUFFSIZE,ascii))
		    break;

		for(;;)
		{
			sscanf(buff, "%1c%lf%lf", &ctype, y, x) ;

			if (ctype != ' ')
				break ;
			
			n_points++ ;
			x++;
			y++ ;

			if (n_points >= alloc_points)
			{
				xarray = (double *)dig_frealloc((char *)xarray, alloc_points + 1000, sizeof(double), alloc_points);
				yarray = (double *)dig_frealloc((char *)yarray, alloc_points + 1000, sizeof(double), alloc_points);
				alloc_points = n_points + 1000 ;
				x = xarray + n_points ;
				y = yarray + n_points ;
			}

			if (NULL == fgets(buff,BUFFSIZE,ascii) )
			{
				end_of_file = 1 ;
				break ;
			}
		}

		if (n_points < 2)
		{
		    if (!n_points)
			continue;
		    /*
		    ** deal with situation where ASCII file has
		    ** single coordinate pair.
		    ** we make a degenerate line segment out of it.
		    */
		    if (n_points)
		    {
			if (2 >= alloc_points)
			{
				xarray = (double *)dig_frealloc((char *)xarray, alloc_points + 1000, sizeof(double), alloc_points);
				yarray = (double *)dig_frealloc((char *)yarray, alloc_points + 1000, sizeof(double), alloc_points);
				alloc_points = n_points + 1000 ;
				x = xarray + n_points ;
				y = yarray + n_points ;
			}

			xarray[1] = xarray[0];
			yarray[1] = yarray[0];
			n_points = 2;
		    }
		}


		/* Allocation is handled for line_pnts */
		if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, n_points))
		    G_fatal_error ("Out of memory");

		Vect_write_line (Map,  (unsigned int) type, Points);

		if (end_of_file)
			return 0;
	}
}
