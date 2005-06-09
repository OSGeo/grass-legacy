#include "Vect.h"
#include "gis.h"
#include "local_proto.h"

int bin_to_asc(
    FILE *ascii,
    FILE *att,
    struct Map_info *Map,
    int ver,
    int format)
{
	int type, ctype, i, cat;
	double *xptr, *yptr, *zptr, x, y;
	static struct line_pnts *Points;
	char buf1[100], buf2[100];
        struct line_cats *Cats;

	Points = Vect_new_line_struct ();	/* init line_pnts struct */
        Cats = Vect_new_cats_struct (); 


	/* by default, read_next_line will NOT read Dead lines */
	/*  but we can override that (in Level I only) by specifying */
	/*  the type  -1, which means match all line types */

	Vect_rewind ( Map );

	while(1)
	{
	        if (-1 == (type = Vect_read_next_line (Map, Points, Cats)))
		    return (-1);

		if (type == -2)	/* EOF */
	            return (0);

		if ( format == FORMAT_POINT && !(type & GV_POINTS) ) continue;

		if ( ver < 5 ) { Vect_cat_get ( Cats, 1, &cat ); }

		switch(type)
		{
		case GV_BOUNDARY:
		    	if ( ver == 5 )
			    ctype = 'B';
			else 
			    ctype = 'A';
			break;
		case GV_CENTROID:
			if ( ver < 5 ) { 
			    if ( att != NULL ) {
				if ( cat > 0 ) {
				    fprintf(att, "A %f %f %d\n", Points->x[0], Points->y[0], cat);
				}
			    }
			    continue; 
			}
			ctype = 'C';			
			break;			
		case GV_LINE:
			ctype = 'L';
			break;
		case GV_POINT:
			ctype = 'P';
			break;
		case GV_FACE:
			ctype = 'F';
			break;
		case GV_KERNEL:
			ctype = 'K';
			break;
		default:
			ctype = 'X';
			fprintf (stderr, "got type %d\n", (int) type);
			break;
		}

		if ( format == FORMAT_POINT ) {
		    /* fprintf(ascii, "%c", ctype); */
		    G_format_easting (Points->x[0], buf1, -1);
		    G_format_northing (Points->y[0], buf2, -1);
		    if ( Map->head.with_z  && ver == 5 ) {
			fprintf(ascii, "%s|%s|%f", buf1, buf2, Points->z[0]);
		    }
		    else {
			fprintf(ascii, "%s|%s", buf1, buf2);
		    }
		    if ( Cats->n_cats > 0 )		
	                fprintf(ascii, "|%d", Cats->cat[0]);
	            fprintf(ascii, "\n");
		} else { 
		    /* FORMAT_STANDARD */
		    if ( ver == 5 && Cats->n_cats > 0 )
			fprintf(ascii, "%c  %d %d\n", ctype, Points->n_points, Cats->n_cats);		
		    else 
			fprintf(ascii, "%c  %d\n", ctype, Points->n_points);

		    xptr = Points->x;
		    yptr = Points->y;
		    zptr = Points->z;
		    
		    while (Points->n_points--)
		    {
			    if ( ver == 4 ) {
			        G_format_northing (*yptr++, buf1, -1);
			        G_format_easting (*xptr++, buf2, -1);
			    } else {
			        G_format_easting (*xptr++, buf1, -1);
			        G_format_northing (*yptr++, buf2, -1);
			    }
			    
			    if ( Map->head.with_z  && ver == 5 ) {
				fprintf(ascii, " %-12s %-12s %-12f\n", buf1, buf2, *zptr++);
			    }
			    else {
				fprintf(ascii, " %-12s %-12s\n", buf1, buf2);
			    }
		    }

		    if ( ver == 5 ) {
			for ( i=0; i< Cats->n_cats; i++ ) {		
			    fprintf(ascii, " %-5d %-10d\n", Cats->field[i], Cats->cat[i]);
			}
		    } else {
			if ( cat > 0 ) {
			    if ( type == GV_POINT ) {
				fprintf(att, "P %f %f %d\n", Points->x[0], Points->y[0], cat);
			    } else {
				x = (Points->x[1] + Points->x[0] ) / 2;
				y = (Points->y[1] + Points->y[0] ) / 2;
				fprintf(att, "L %f %f %d\n", x, y, cat);
			    }
			}
		    }
		}
	}

	/* not reached */
}

