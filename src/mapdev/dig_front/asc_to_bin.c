/*  @(#)asc_to_bin.c	2.1  6/26/87  */

/*
*  May need some changes for 3.0 digit files.  POINT type
*/

#include "digit.h"

asc_to_bin(ascii, binary)
	FILE *binary, *ascii ;
{
	char ctype ;
	char buff[128] ;
	char *fgets() ;
	double *xarray ;
	double *yarray ;
	double *x, *y ;
	int i ;
	int n_points ;
	int type ;
	int alloc_points ;
	int end_of_file ;

	end_of_file = 0 ;
	alloc_points     = 1000 ;
	xarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	yarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;

	if (NULL == fgets(buff,128,ascii))
	    return ;

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
			return ;
		default:
			printf("?? %s\n", buff) ;
			if (NULL == fgets(buff,128,ascii))
				return ;
			goto the_switch ;
		}

	/* Collect the points */
		n_points = 0 ;
		x = xarray ;
		y = yarray ;
		if (NULL == fgets(buff,128,ascii))
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
				xarray = (double *)dig_frealloc((char *)xarray, alloc_points + 100, sizeof(double), alloc_points);
				yarray = (double *)dig_frealloc((char *)yarray, alloc_points + 100, sizeof(double), alloc_points);
				alloc_points = n_points + 100 ;
				x = xarray + n_points ;
				y = yarray + n_points ;
			}

			if (NULL == fgets(buff,128,ascii) )
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
				xarray = (double *)dig_frealloc((char *)xarray, alloc_points + 100, sizeof(double), alloc_points);
				yarray = (double *)dig_frealloc((char *)yarray, alloc_points + 100, sizeof(double), alloc_points);
				alloc_points = n_points + 100 ;
				x = xarray + n_points ;
				y = yarray + n_points ;
			}

			xarray[1] = xarray[0];
			yarray[1] = yarray[0];
			n_points = 2;
		    }
		}


		dig_Write_line (binary, (char) type, xarray, yarray, n_points);

		if (end_of_file)
			return ;
	}
}
