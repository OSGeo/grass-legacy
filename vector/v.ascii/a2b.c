#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"

#define BUFFSIZE 128

int err ( char *errstr );

int asc_to_bin(
	FILE *ascii ,
	struct Map_info *Map,
	int pnt)
{
	char ctype ;
	char buff[BUFFSIZE], *bptr;
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
	int catn;
	int cat;

	/* Must always use this to create an initialized  line_pnts structure */
	Points = Vect_new_line_struct ();
	Cats = Vect_new_cats_struct ();	

	end_of_file = 0 ;
	//alloc_points     = 1000 ;
	alloc_points     = 1;
	xarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	yarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;
	zarray = (double *) dig_falloc(alloc_points, sizeof(double)) ;	


	while ( fgets(buff,BUFFSIZE,ascii) != NULL  )
	{
	    if ( !pnt ) {
		n_cats=0;
		if (  sscanf(buff, "%1c%d%d", &ctype, &n_coors, &n_cats) < 2  || n_coors < 0 || n_cats < 0 ) 
		    return ( err ( buff) );

		if (!n_points)
		    continue;

		
		switch(ctype)
		{
		case 'A':
			type = GV_BOUNDARY ;
			break ;
		case 'B':
			type = GV_BOUNDARY ;
			break ;
		case 'C':
			type = GV_CENTROID ;
			break ;			
		case 'L':
			type = GV_LINE ;
			break ;
		case 'P':
			type = GV_POINT ;
			break ;
		case 'F':
			type = GV_FACE ;
			break ;
		case 'K':
			type = GV_KERNEL ;
			break ;
		case 'a':
		case 'b':
		case 'c':
		case 'l':
		case 'p':
			type = 0; /* dead -> ignore */
			break;
		default:
			fprintf (stderr,"Error reading ascii file:\n%s\n", buff) ;
			return 0;
		}

		n_points = 0 ;
		x = xarray ;
		y = yarray ;
		z = zarray ;		

	        /* Collect the points */
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
			G_debug( 5, "coor in: %s -> x = %f y = %f z = %f", G_chop(buff), *x, *y, *z);
			
			n_points++;
			x++;
			y++;
			z++;			
	
			if (n_points >= alloc_points)
			{
				xarray = (double *)dig_frealloc((char *)xarray, alloc_points + 1000, sizeof(double), alloc_points);
				yarray = (double *)dig_frealloc((char *)yarray, alloc_points + 1000, sizeof(double), alloc_points);
				zarray = (double *)dig_frealloc((char *)zarray, alloc_points + 1000, sizeof(double), alloc_points);				
				alloc_points = n_points + 1000 ;
				x = xarray + n_points ;
				y = yarray + n_points ;
				z = zarray + n_points ;				
			}
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
            } else {  /* points format */
		type = GV_POINT;
		if (  sscanf(buff, "%lf|%lf", xarray, yarray) != 2 ) 
		    return ( err ( buff) );
		
		bptr = strchr( buff, '|' );

		if ( Map->head.with_z ) {
		    bptr = strchr( bptr + 1, '|' );
		    if ( bptr == NULL )
		        return ( err ( buff) );
		    
		    if (  sscanf(bptr + 1, "%lf", zarray) != 1 ) 
		        return ( err ( buff) );
		} 
		n_points = 1 ;
		
		/* category */
		bptr = strchr( bptr + 1, '|' );

		if ( bptr ) {
		    if (  sscanf(bptr + 1, "%d", &cat) == 1 ) 
		        Vect_cat_set ( Cats, 1, cat );
		}	
	    }
	    /* Allocation is handled for line_pnts */
	    if (0 > Vect_copy_xyz_to_pnts (Points, xarray, yarray, zarray, n_points))
		G_fatal_error ("Out of memory");

	    if ( type > 0 )
		Vect_write_line ( Map, type, Points, Cats );
    
	    Vect_reset_cats ( Cats ); 
	}
	return 0;	
}

int err ( char *errstr ) {

    fprintf (stderr,"Error reading ascii file:\n%s\n", errstr) ;
    return 0;
}

