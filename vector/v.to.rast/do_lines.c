#include "gis.h"
#include "dbmi.h" 
#include "Vect.h"
#include "local.h"

static int plot_line(double *,double *,int);
static int plot_points (double *,double *,int);

int do_lines ( struct Map_info *Map, struct line_pnts *Points, dbCatValArray *Cvarr, int ctype, int field,
	       int use, double value, int value_type)
{
    int nlines, type, ret, cat;
    int index;
    int count;
    struct line_cats *Cats;
    CELL cval;
    DCELL dval;

    Cats = Vect_new_cats_struct ();
    
    nlines = Vect_get_num_lines (Map);
    count = 0;
    for ( index = 1; index <= nlines; index++) {
	type = Vect_read_line ( Map, Points, Cats, index );
	Vect_cat_get (Cats, field, &cat);
	if ( cat <= 0 ) { continue; } 

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
	} else {
	    if ( value_type == USE_CELL )
		set_cat ( (int) value);
	    else
		set_dcat ( value );
	}

	if ( (type & GV_LINES ) ) {
	    plot_line (Points->x, Points->y, Points->n_points);
	} else if ( type & GV_POINTS ) {
	    plot_points (Points->x, Points->y, Points->n_points);
	} else {
	    continue;
	}
	count++;
    }

    Vect_destroy_cats_struct ( Cats ); 
    return nlines;
}

static int plot_line(double *x,double *y,int n)
{
    while (--n > 0)
    {
	G_plot_line2 (x[0],y[0],x[1],y[1]);
	x++;
	y++;
    }

  return 0;
}

static int plot_points (double *x,double *y,int n)
{
    /* only plot the first point */
    if (n > 0) G_plot_point (*x, *y);

    return 0;
}

