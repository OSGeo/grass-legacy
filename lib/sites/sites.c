/*-
 * These functions and definitions support the site format for 5.0
 * (format proposed by Dave Gerdes):
 *
 * easting|northing|[z|[d4|]...][#category_int] [ [@attr_text OR %flt] ... ]
 *
 * to allow multidimensions (everything preceding the last '|') and any
 * number of text or numeric attribute fields.
 *
 * Author: James Darrell McCauley <mccauley@ecn.purdue.edu>
 * 31 Jan 1994
 */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "gis.h"
#include "site.h"
#include "dbmi.h"
#include "Vect.h"

int site_att_cmp ( const void *pa, const void *pb) {
    const SITE_ATT *a = pa, *b = pb;
    
    return a->cat - b->cat;
}

/*-
 * Reads ptr and returns 0 on success,
 *                      -1 on EOF,
 *                      -2 on other fatal error or insufficient data,
 *                       1 on format mismatch (extra data)
 */
int G_site_get ( FILE *fptr, Site *s)
{
    int    i, type, cat;
    struct Map_info *Map;
    static struct line_pnts *Points = NULL;
    static struct line_cats *Cats = NULL;
    SITE_ATT *sa;

    Map = (struct Map_info *) fptr;

    if ( Points == NULL ) Points = Vect_new_line_struct ();
    if ( Cats == NULL ) Cats = Vect_new_cats_struct ();

    while ( 1 ) {
       	type = Vect_read_next_line (Map, Points, Cats);
       	
	if ( type == -1 ) return -2; /* Error */ 
	if ( type == -2 ) return -1; /* EOF */ 
	if ( type != GV_POINT ) continue; /* Is not point */ 
	
	Vect_cat_get (Cats, 1, &cat);

	G_debug (4, "Site: %f|%f|%f|#%d", Points->x[0], Points->y[0], Points->z[0], cat);
        
	s->east = Points->x[0];
	s->north = Points->y[0];
	if ( Vect_is_3d(Map) ) s->dim[0] = Points->z[0];

	s->ccat = cat;
    
	/* find att */

	if ( Map->n_site_att > 0 ) {
	    sa = (SITE_ATT *) bsearch ( (void *) &cat, (void *)Map->site_att, Map->n_site_att, 
					 sizeof(SITE_ATT), site_att_cmp );
	    
	    if ( sa == NULL ) {
		G_warning ( "Attributes for category %d not found", cat );
		for ( i = 0; i < Map->n_site_dbl; i++ ) s->dbl_att[i] = 0;
		for ( i = 0; i < Map->n_site_str; i++ ) G_strncpy (s->str_att[i], "", MAX_SITE_STRING);
	    } else { 
		for ( i = 0; i < Map->n_site_dbl; i++ ) s->dbl_att[i] = sa->dbl[i];
		for ( i = 0; i < Map->n_site_str; i++ ) 
		    G_strncpy (s->str_att[i], sa->str[i], MAX_SITE_STRING);
	    }
	}
    
	return 0;
    }
}

/* Writes a site to file open on fptr. */
int G_site_put ( FILE *fptr, Site *s)
{
    struct Map_info *Map;
    static struct line_pnts *Points = NULL;
    static struct line_cats *Cats = NULL;

    Map = (struct Map_info *) fptr;

    if ( Points == NULL ) Points = Vect_new_line_struct ();
    if ( Cats == NULL ) Cats = Vect_new_cats_struct ();

    Vect_reset_line (Points);
    Vect_reset_cats (Cats);

    Vect_append_point (Points, s->east, s->north, 0.0);

    G_debug (4, "cattype = %d", s->cattype);

    if ( s->cattype == FCELL_TYPE || s->cattype == DCELL_TYPE )
	G_fatal_error ( "Category must be integer" );

    if ( s->cattype == CELL_TYPE )
        Vect_cat_set (Cats, 1, s->ccat);

    Vect_write_line (Map, GV_POINT, Points, Cats);

    return 0;
}

/*-
 * Tries to guess the format of a sites list (the dimensionality,
 * the presence/type of a category, and the number of string and decimal
 * attributes) by reading the first record in the file.
 * Reads ptr and returns 0 on success,
 *                      -1 on EOF,
 *                      -2 for other error.
 */
int G_site_describe ( FILE *ptr,
  int *dims,int *cat,int *strs,int *dbls)
{
    struct Map_info *Map;

    Map = (struct Map_info *) ptr;
    
    if ( Vect_is_3d(Map) ) {
	G_debug (1, "Vector is 3D -> number of site dimensions is 3");
	*dims = 3;
    } else {
	G_debug (1, "Vector is 2D -> number of site dimensions is 2");
	*dims = 2;
    }

    *cat=CELL_TYPE;

    /* attributes ignored for now, later read from DB */
    *dbls = Map->n_site_dbl;
    *strs = Map->n_site_str;

    return 0;
}

int G_site_put_head ( FILE *ptr, Site_head *head)
/*-
 * Writes site_head struct.
 */
{
    struct Map_info *Map;
    static char buf[128];

    Map = (struct Map_info *) ptr;

    if (head->name!=NULL)
	Vect_set_map_name (Map, head->name);
      
    if (head->desc!=NULL)
	Vect_set_comment (Map, head->name);

    /*
    if (head->form!=NULL)
        fprintf(ptr,"form|%s\n",head->form);
    if (head->labels!=NULL)
        fprintf(ptr,"labels|%s\n",head->labels);
    */
    /* time could be in (char *) stime, (struct TimeStamp *) time, 
       both, or neither */
    if (head->stime!=NULL || head->time !=NULL) {
        if (head->time != NULL) {   /* TimeStamp struct has precendence */
            G_format_timestamp (head->time, buf);
	    Vect_set_date (Map, buf);
        } else if (head->stime != NULL) {  /* next check string */
            if (head->time==NULL) {
                if ((head->time=(struct TimeStamp *) G_malloc(sizeof(struct TimeStamp)))==NULL)
	            G_fatal_error("Memory error in writing timestamp");
	        else 
	            if (G_scan_timestamp (head->time, head->stime) < 0) {         
	                G_warning("Illegal TimeStamp string");
	                return -1; /* added to prevent crash 5/2000 MN*/
	        }
            }
            G_format_timestamp (head->time, head->stime);
	    Vect_set_date (Map, head->stime);
        }
    }
    return 0;
}

/*-
 * Fills in site_head struct.
 */
int G_site_get_head (FILE *ptr, Site_head *head)
{
    struct Map_info *Map;

    Map = (struct Map_info *) ptr;

    head->name = Vect_get_name(Map);
    head->desc = Vect_get_comment(Map);
    head->form = NULL;
    head->labels = NULL;
    head->stime = Vect_get_date(Map);
    head->time=NULL;

    if ( head->stime && strlen(head->stime) > 0 ) {
       if ((head->time=(struct TimeStamp *) G_malloc(sizeof(struct TimeStamp)))==NULL)
           G_fatal_error("Memory error in allocating timestamp");
       if (G_scan_timestamp (head->time , head->stime)<0) {
           G_warning(datetime_error_msg());
           
	   head->time=NULL;
           head->stime=NULL;
       }
    }
  
    return 0;
}

/*-************************************************************************
 *   char *
 *   G_ask_sites_new(prompt, name))
 *       asks user to input name of a new site list file
 *
 *   char *
 *   G_ask_sites_old(prompt, name)
 *       asks user to input name of an existing site list file
 *
 *   char *
 *   G_ask_sites_any(prompt, name)
 *       asks user to input any site list name
 *
 *   char *
 *   G_ask_sites_in_mapset(prompt, name)
 *       asks user to input name of an existing site list file in
 *       current mapset
 *
 *   parms:
 *      char *prompt    optional prompt for user
 *      char *name      buffer to hold name of map found
 *
 *   returns:
 *      char *pointer to a string with name of mapset
 *       where file was found, or NULL if not found
 *
 *   note:
 *      rejects all names that begin with .
 **********************************************************************
 *
 *  FILE *
 *  G_sites_open_old (name, mapset)
 *      opens the existing site list file 'name' in the 'mapset'
 *
 *  FILE *
 *  G_sites_open_new (name)
 *      opens a new site list file 'name' in the current mapset
 *
 *  parms
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 **********************************************************************/

char * G_find_sites (char *name,char *mapset)
{
  return G_find_vector (name, mapset);
}

char * G_find_sites2 (char *name,char *mapset)
{
  return G_find_vector2 (name, mapset);
}

char * G_ask_sites_new (char *prompt,char *name)
{
  return G_ask_new (prompt, name, "vector", "vector");
}

char * G_ask_sites_old (char *prompt,char *name)
{
  return G_ask_old (prompt, name, "vector", "vector");
}

char * G_ask_sites_any (char *prompt,char *name)
{
  return G_ask_any (prompt, name, "vector", "vector", 1);
}

char * G_ask_sites_in_mapset (char *prompt,char *name)
{
  return G_ask_in_mapset (prompt, name, "vector", "vector");
}


FILE * G_sites_open_old (char *name,char *mapset)
{
    struct Map_info *Map;
    struct field_info *fi;
    int more, nrows, row, ncols, col, ndbl, nstr, adbl, astr, ctype;
    SITE_ATT *sa;
    
    dbDriver *driver;
    dbString stmt;
    dbCursor cursor;
    dbTable  *table;
    dbColumn *column;
    dbValue  *value;
    
    G_warning ( "Vector used instead of sites.");
    
    Map = (struct Map_info *) G_malloc ( sizeof(struct Map_info) );

    Vect_set_open_level (1);
    Vect_open_old (Map, name, mapset);

    G_debug ( 1, "Vector map opened");

    /* Load attributes */
    Map->site_att = NULL;
    Map->n_site_att = 0;
    Map->n_site_dbl = 0;
    Map->n_site_str = 0;

    fi = Vect_get_field(Map, 1);
    if ( fi == NULL ) {  /* not attribute table */ 
	G_debug ( 1, "No attribute table" );
	return (FILE *) Map;
    }
    
    driver = db_start_driver_open_database ( fi->driver, fi->database );
    if ( driver == NULL )
    	G_fatal_error ( "Cannot open database %s by driver %s", fi->database, fi->driver );
	
    db_init_string (&stmt);
    db_set_string ( &stmt, "select * from ");
    db_append_string ( &stmt, fi->table );

    if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
	G_fatal_error ("Cannot select attributes.");

    nrows = db_get_num_rows ( &cursor );
    G_debug ( 1, "%d rows selected from vector attribute table", nrows);
    
    Map->site_att = (SITE_ATT *) malloc ( nrows * sizeof(SITE_ATT) );
    Map->n_site_att = nrows;

    table = db_get_cursor_table (&cursor);
    ncols = db_get_table_number_of_columns(table);

    row = 0;
    adbl = astr = 0;
    while (1) {
	if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	    G_fatal_error ("Cannot fetch row.");

	if (!more) break;

	/* Get number of each type */
	if ( row == 0 ) {
	    for (col = 0; col < ncols; col++) {
		column = db_get_table_column ( table, col );
		ctype =  db_sqltype_to_Ctype ( db_get_column_sqltype(column) );

	        if ( strcmp(db_get_column_name(column),fi->key) == 0 )
		    continue;
		    
		switch ( ctype ) {
		    case DB_C_TYPE_INT:
		    case DB_C_TYPE_DOUBLE:
			adbl++;
			break;
		    case DB_C_TYPE_STRING:
		    case DB_C_TYPE_DATETIME:
			astr++;
			break;
		}
	    }
	    Map->n_site_dbl = adbl;
	    Map->n_site_str = astr;
    	    G_debug ( 1, "adbl = %d astr = %d", adbl, astr );
	}

	sa = &(Map->site_att[row]);
	sa->dbl = (double *) malloc ( adbl * sizeof(double) );
	sa->str = (char **) malloc ( astr * sizeof(char *) );

	ndbl = nstr = 0;
	for (col = 0; col < ncols; col++) {
	    column = db_get_table_column ( table, col );
	    ctype =  db_sqltype_to_Ctype ( db_get_column_sqltype(column) );
	    value  = db_get_column_value(column);

	    if ( strcmp(db_get_column_name(column),fi->key) == 0 ) {
		sa->cat = db_get_value_int(value); 
	    } else {
		switch ( ctype ) {
		    case DB_C_TYPE_INT:
			sa->dbl[ndbl] = db_get_value_int(value); 
			ndbl++;
			break;
		    case DB_C_TYPE_DOUBLE:
			sa->dbl[ndbl] = db_get_value_double(value); 
			ndbl++;
			break;
		    case DB_C_TYPE_STRING:
			sa->str[nstr] = G_store ( db_get_value_string(value) ) ; 
			nstr++;
			break;
		    case DB_C_TYPE_DATETIME:
			sa->str[nstr] = ""; /* TODO */
			nstr++;
			break;
		}
	    }
	}
	row++;
    }
    db_close_database_shutdown_driver ( driver );
    
    /* sort attributes */
    qsort ( (void *)Map->site_att, Map->n_site_att, sizeof(SITE_ATT), site_att_cmp );
    
    return (FILE *) Map;
}

FILE * G_sites_open_new (char *name)
{
    struct Map_info *Map;
    
    G_warning ( "Vector used instead of sites.");
    G_warning ( "Site/vector attributes ignored.");
    
    Map = (struct Map_info *) G_malloc ( sizeof(struct Map_info) );

    Vect_open_new (Map, name, 0);

    G_debug ( 1, "New vector map opened");
    return (FILE *) Map;
}

void G_sites_close ( FILE * ptr)
{
    int i, j;
    struct Map_info *Map;

    Map = (struct Map_info *) ptr;

    if (Map->mode == GV_MODE_WRITE || Map->mode == GV_MODE_RW)
	Vect_build (Map, stderr);
    
    Vect_close ( Map );

    for ( i = 0; i < Map->n_site_att; i++ ) {
	free ( Map->site_att[i].dbl );
        
	for ( j = 0; j < Map->n_site_str; j++ ) 
	    free ( Map->site_att[i].str[j] );
	
	free ( Map->site_att[i].str );
    }
    free (Map->site_att);

    G_free ( Map );
}

/*********************************************/
/* The following functions are obsolete.     */
/* They are retained here only for backwards */
/* compatability while porting applications  */
/*********************************************/
FILE *G_fopen_sites_old (char *name, char *mapset)
{
    return G_sites_open_old (name, mapset);
}

FILE *G_fopen_sites_new (char *name)
{
    return G_sites_open_new (name);
}

int G_get_site ( FILE *fd, double *east,double *north, char **desc)
{
    char buf[400];
    char temp[400];
    char ebuf[128], nbuf[128];
    static char *desc_ptr = NULL;

    /* TODO ? */
    G_fatal_error ( "G_get_site() not yet updated.");

    return -1;
}

int G_put_site ( FILE *fd, double east,double north, char *desc)
{
    char ebuf[128], nbuf[128];
    int fmt;

    /* TODO ? */
    G_fatal_error ( "G_put_site() not yet updated.");
    
    return 0;
}


