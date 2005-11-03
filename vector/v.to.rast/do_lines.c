#include "gis.h"
#include "dbmi.h" 
#include "Vect.h"
#include "local.h"


static int plot_line(double *, double *, int, int);
static int plot_points (double *,double *,int);


int do_lines ( struct Map_info *Map, struct line_pnts *Points, dbCatValArray *Cvarr, int ctype, int field,
	       int use, double value, int value_type)
{
    double min, max, u;
    int nlines, type, ret, cat, no_contour = 0;
    int index;
    int count, j;
    struct line_cats *Cats;
    CELL cval;
    DCELL dval;

    Cats = Vect_new_cats_struct ();
    
    nlines = Vect_get_num_lines (Map);
    count = 0;
    for ( index = 1; index <= nlines; index++) {
	type = Vect_read_line ( Map, Points, Cats, index );
	Vect_cat_get (Cats, field, &cat);
	if ( cat < 0 ) { continue; } 

	if ( use == USE_ATTR ) {
	    if ( ctype == DB_C_TYPE_INT ) {
		ret = db_CatValArray_get_value_int ( Cvarr, cat, &cval );
		if ( ret != DB_OK ) {
		    G_warning ("No record for line (cat = %d)", cat );
		    continue ;
		}
		set_cat (cval);
	    } else if ( ctype == DB_C_TYPE_DOUBLE ) {
		ret = db_CatValArray_get_value_double ( Cvarr, cat, &dval );
		if ( ret != DB_OK ) {
		    G_warning ("No record for line (cat = %d)", cat );
		    continue ;
		}
		set_dcat ( dval);
	    } else {
		G_fatal_error ("Column type  not supported" );
	    }
	} else if  ( use == USE_CAT ) {
	    set_cat (cat);
	} else if ( use == USE_VAL ) {
	    if ( value_type == USE_CELL )
		set_cat ( (int) value);
	    else
		set_dcat ( value );
	} else if ( use == USE_Z ) {

	    if ( type & GV_POINTS ) { 
	        min = Points->z[0];	
	    } else if ( type & GV_LINES ) {
		min = max = Points->z[0];
		for ( j = 1; j < Points->n_points; j++) {
		    if ( Points->z[j] < min ) min = Points->z[j];
		    if ( Points->z[j] > max ) max = Points->z[j];
		}
		if ( min != max ) {
		    no_contour++;
		    continue;
		}
	    }
		
	    set_dcat ( min );
	} else if (use == USE_D) {
            min = 360.;
            max = 0.;

            for (j = 1; j < Points->n_points; j++) {
                u = deg_angle(Points->x[j], Points->y[j],
                              Points->x[j-1], Points->y[j-1]);

                if (u < min)
                    min = u;
                if (u > max)
                    max = u;
            }
        }

	if ( (type & GV_LINES ) ) {
	    plot_line (Points->x, Points->y, Points->n_points, use);
	} else if ( type & GV_POINTS ) {
	    plot_points (Points->x, Points->y, Points->n_points);
	} else {
	    continue;
	}
	count++;
    }
    
    if ( no_contour > 0 )
	G_warning ("%d lines with varying height were not written to raster", no_contour );

    Vect_destroy_cats_struct ( Cats ); 

    return nlines;
}


static int plot_line(double *x,double *y,int n, int use)
{
    while (--n > 0)
    {
        if (use == USE_D)
            set_dcat((DCELL)deg_angle(x[1], y[1], x[0], y[0]));

	G_plot_line2 (x[0],y[0],x[1],y[1]);
	x++;
	y++;
    }

  return 0;
}


static double Pi;

/* cos of the angle between two vectors is (a . b)/|a||b| */
double
v2angle(double v1[2], double v2[2], double mag1, double mag2)
{
    double costheta = (v1[0] * v2[0] + v1[1] * v2[1])/(mag1 * mag2);

    return (acos(costheta));
}


double
deg_angle(double x0, double y0, double x1, double y1)
{
    double v1[2], v2[2];
    double mag1, mag2;
    double v_ang, deg_a;
    static int first = 1;

    if (first) {
        first = 0;
        Pi = 4.0 * atan(1.0);
    }

    v1[0] = 1;
    v1[1] = 0;
    v2[0] = x0 - x1;
    v2[1] = y0 - y1;

    mag1 = 1.0;
    mag2 = sqrt(((v2[0] * v2[0]) + (v2[1] * v2[1])));
    v_ang = v2angle(v1, v2, mag1, mag2);

    if (y0 < y1)
        v_ang = (2.0 * Pi) - v_ang;

    deg_a = v_ang * 360.0 / (2.0 * Pi);
    /* fprintf(stderr, "\n %f", deg_a); */

    return deg_a;
}


static int plot_points (double *x,double *y,int n)
{
    /* only plot the first point */
    if (n > 0) G_plot_point (*x, *y);

    return 0;
}

