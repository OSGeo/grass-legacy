#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"
#include "local.h"

static
struct list
{
    double size;
    int index;
    CELL cat;
} *list;
static int nareas ;
static int compare(const void *, const void *);

int do_areas ( struct Map_info *Map,struct line_pnts *Points, dbCatValArray *Cvarr, int ctype, int field,
	       int use, double value, int value_type)
{
    int i,index, ret;
    CELL  cval, cat;
    DCELL dval;

    if (nareas <= 0) return 0;

    for (i = 0; i < nareas; i++) {
        /* Note: in old version (grass5.0) there was a check here if the current area 
	*        is identical to previous one. I don't see any reason for this in topological vectors */
	index = list[i].index;
	cat   = list[i].cat;
	G_debug ( 3, "Area cat = %d", cat );

	if ( ISNULL ( &cat ) ) { /* No centroid or no category */
	    set_cat (cat);
	} else {
	    if ( use == USE_ATTR ) {
		if ( ctype == DB_C_TYPE_INT ) {
		    ret = db_CatValArray_get_value_int ( Cvarr, cat, &cval );
		    if ( ret != DB_OK ) {
			G_warning ("No record for area (cat = %d)", cat );
			SETNULL( &cval );
		    }
		    set_cat (cval);
		} else if ( ctype == DB_C_TYPE_DOUBLE ) {
		    ret = db_CatValArray_get_value_double ( Cvarr, cat, &dval );
		    if ( ret != DB_OK ) {
			G_warning ("No record for area (cat = %d)", cat );
			SETDNULL( &dval );
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
         }

	if(Vect_get_area_points (Map, index, Points) <= 0) {
	    fprintf (stderr, "*** Get area [%d] failed ***\n", index);
	    return -1;
	}

	G_plot_polygon (Points->x, Points->y, Points->n_points);
    }
    return nareas;
}

int sort_areas ( struct Map_info *Map, struct line_pnts *Points, int field)
{
    int i, centroid;
    struct line_cats *Cats;
    CELL cat;

    G_begin_polygon_area_calculations();
    Cats = Vect_new_cats_struct ();

    /* first count valid areas */
    nareas = Vect_get_num_areas ( Map );
    if (nareas == 0) return 0; 

    /* allocate list to hold valid area info */
    list = (struct list *) G_calloc (nareas, sizeof (struct list));

    /* store area size,cat,index in list */
    for (i = 0; i < nareas; i++) {
	list[i].index = i + 1;
	Vect_get_area_points (Map, i + 1, Points);
	list[i].size = G_area_of_polygon (Points->x, Points->y, Points->n_points);
	
	centroid = Vect_get_area_centroid ( Map, i + 1 );
	if ( centroid <= 0 ) {
	    SETNULL( &cat );
	    G_warning ("Area without centroid (may be OK for island)");
	} else {
	    Vect_read_line ( Map, NULL, Cats, centroid );
	    Vect_cat_get (Cats, field, &cat);
	    if ( cat < 0 ) {
		SETNULL( &cat );
		G_warning ("Area centroid without category");
	    } 
	}
	list[i].cat = cat;
    }

    /* sort the list by size */
    qsort (list, nareas, sizeof(struct list), compare);

    return nareas;
}

static int compare (const void *aa, const void *bb)
{
    const struct list *a = aa, *b = bb;
    if (a->size < b->size)
	return 1;
    if (a->size > b->size)
	return -1;
    return 0;
}

