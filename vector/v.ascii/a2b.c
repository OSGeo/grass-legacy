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
	double *zarray ;	
	double *x, *y, *z;
	int i, n_points, n_coors, n_cats ;
	int type;
	int alloc_points ;
	int end_of_file ;
	struct line_pnts *Points;
	struct line_cats *Cats;	
	GRASS_V_FIELD catn;
	GRASS_V_CAT  cat;

	/* Must always use this to create an initialized  line_pnts structure */
	Points = Vect_new_line_struct ();
	Cats = Vect_new_cats_struct ();	

	end_of_file = 0 ;
	alloc_points     = 1000 ;
	xarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	yarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	zarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;	


	while ( fgets(buff,BUFFSIZE,ascii) != NULL  )
	{
		n_cats=0;
		if (  sscanf(buff, "%1c%d%d", &ctype, &n_coors, &n_cats) < 2  || n_coors < 0 || n_cats < 0 ) {
		    fprintf (stderr,"Error reading ascii file:\n%s\n", buff) ;
		    return 0;
		}    
		if (!n_points)
		    continue;

		switch(ctype)
		{
		case 'A':
			type = BOUNDARY ;
			break ;
		case 'B':
			type = BOUNDARY ;
			break ;
		case 'C':
			type = CENTROID ;
			break ;			
		case 'L':
			type = LINE ;
			break ;
		case 'P':
			type = DOT ;
			break ;
		case 'a':
			type = DEAD_BOUNDARY ;
			break ;
		case 'b':
			type = DEAD_BOUNDARY ;
			break ;
		case 'c':
			type = DEAD_CENTROID ;
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
			fprintf (stderr,"Error reading ascii file:\n%s\n", buff) ;
			return 0;
		}

		/* Collect the points */
		n_points = 0 ;
		x = xarray ;
		y = yarray ;
		z = zarray ;		

		for( i=0; i<n_coors; i++)
		{
			if ( fgets(buff,BUFFSIZE,ascii) == NULL) {
			    fprintf (stderr,"End of ascii file reached before end of coordinates\n") ;
			    return 0;
			} 
			*z=0;
			if ( sscanf(buff, "%lf%lf%lf", y, x, z) < 2 ) {			
			    fprintf (stderr,"Error reading ascii file:\n%s\n", buff) ;
			    return 0;
			}    
			
			n_points++;
			x++;
			y++;
			z++;			
	


			if (n_points >= alloc_points)
			{
				xarray = (double *)dig_frealloc((char *)xarray, alloc_points + 1000, sizeof(double), alloc_points);
				yarray = (double *)dig_frealloc((char *)yarray, alloc_points + 1000, sizeof(double), alloc_points);
				zarray = (double *)dig_frealloc((char *)yarray, alloc_points + 1000, sizeof(double), alloc_points);				
				alloc_points = n_points + 1000 ;
				x = xarray + n_points ;
				y = yarray + n_points ;
				z = zarray + n_points ;				
			}
		}

		if (n_points == 1)
		{
		    /*
		    ** deal with situation where ASCII file has
		    ** single coordinate pair.
		    ** we make a degenerate line segment out of it.
		    */
		    xarray[1] = xarray[0];
		    yarray[1] = yarray[0];
		    zarray[1] = yarray[0];			
		    n_points = 2;
		}

		/* Collect the cats */
		for( i=0; i<n_cats; i++)
		{
			if ( fgets(buff,BUFFSIZE,ascii) == NULL) {
			    fprintf (stderr,"End of ascii file reached before end of categories.\n") ;
			    return 0;
			} 
			if ( sscanf(buff, "%u%u", &catn, &cat) != 2 ) {
			    fprintf (stderr,"Error reading categories:\n%s\n", buff) ;
			    return 0;
			}    
			Vect_cat_set ( Cats, catn, cat );
		}

		/* Allocation is handled for line_pnts */
		if (0 > Vect_copy_xyz_to_pnts (Points, xarray, yarray, zarray, n_points))
		    G_fatal_error ("Out of memory");

	    	Vect_write_line ( Map, type, Points, Cats );

		Vect_reset_cats ( Cats ); 
	}
	return 0;	
}
