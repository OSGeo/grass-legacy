/* ***************************************************************
 * *
 * * MODULE:       v.digit
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Edit vector
 * *              
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include "global.h"
/* Maximum value for field */
int (*MaxFieldCat)[2];
int nMaxFieldCat, aMaxFieldCat;

/* Init cats */
void
cat_init (struct Map_info *Map)
{
    int i, line, nlines;
    struct line_cats *Cats;

    G_debug (2, "cat_init()" );
    Cats = Vect_new_cats_struct (); 
    
    /* Max cats */
    nMaxFieldCat = 0;
    aMaxFieldCat = 10; /* allocated space */
    MaxFieldCat = (void *) G_malloc ( (aMaxFieldCat) * sizeof(int) * 2 ); 

    /* Read the map and set maximum categories */
    nlines = Vect_get_num_lines ( Map );
/*     G_message(_("Reading vector: ")); */
    for ( line = 1; line <= nlines; line++ ) {
/* 	G_percent(line, nlines, 1); */
	Vect_read_line ( Map, NULL, Cats, line ); 
	for ( i = 0; i < Cats->n_cats; i++ ) {
	    G_debug(2,"i=%d, Cats->n_cats=%d, Cats->field[%d]=%d, Cats->cat[%d]=%d\n",
		    i,Cats->n_cats, i,Cats->field[i],i,Cats->cat[i]);
	    if ( (cat_max_get(Cats->field[i])) < Cats->cat[i] ) {
		cat_max_set(Cats->field[i],Cats->cat[i]);
	    }
	}
    }
}

/* get maximum cat for field */
int
cat_max_get ( int field )
{
    int i;
    
    G_debug (2, "cat_max_get() field = %d", field );

    for ( i = 0; i < nMaxFieldCat; i++ ) {
	if ( MaxFieldCat[i][0] == field ) {
	    return (MaxFieldCat[i][1]);
	}
    }

    return 0;
}
    
/* set maximum cat for field */
void
cat_max_set ( int field, int cat)
{
    int i;
    
    G_debug (2, "cat_max_set() field = %d cat = %d", field, cat );

    for ( i = 0; i < nMaxFieldCat; i++ ) {
	if ( MaxFieldCat[i][0] == field ) {
	    MaxFieldCat[i][1] = cat;
	    return;
	}
    }
    /* Field not found -> add new */
    if ( nMaxFieldCat == aMaxFieldCat ) {
        aMaxFieldCat += 10;
        MaxFieldCat = (void *) G_realloc ( MaxFieldCat, (aMaxFieldCat) * sizeof(int) * 2 );
    }
    MaxFieldCat[nMaxFieldCat][0] = field;
    MaxFieldCat[nMaxFieldCat][1] = cat;
    nMaxFieldCat++;
}

