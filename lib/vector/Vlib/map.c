/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdio.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>
#include "Vect.h"
#include "dbmi.h"

/*!
 \fn int Vect_copy_map_lines ( struct Map_info *In, struct Map_info *Out )
 \brief copy all alive elements of opened vector map to another opened vector map
 \return 0 on success, 1 on error
 \param  in Map_info structure, out Map_info structure
*/
int 
Vect_copy_map_lines ( struct Map_info *In, struct Map_info *Out )
{
    int    i, type, nlines, ret;
    struct line_pnts *Points;
    struct line_cats *Cats;

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
   
    if ( Vect_level ( In ) < 1 )
	G_fatal_error ("Vect_copy_map_lines(): input vector is not open");
    
    ret = 0;
    /* Note: sometimes is important to copy on level 2 (pseudotopo centroids) 
     *       and sometimes on level 1 if build take too long time */
    if ( Vect_level ( In ) >= 2 ) { 
	nlines = Vect_get_num_lines ( In );
	for ( i = 1; i <= nlines; i++ ) {
	    type =  Vect_read_line (In, Points, Cats, i);
	    if ( type == -1 ) {
		G_warning ("Cannot read vector file\n" );
		ret = 1;
		break;
	    } 
	    if ( type == 0 ) continue; /* dead line */

	    Vect_write_line ( Out, type, Points, Cats );
	}
    } else {  /* Level 1 */
	Vect_rewind ( In );
	while ( 1 ) {
	    type =  Vect_read_next_line (In, Points, Cats);
	    if ( type == -1 ) {
		G_warning ("Cannot read vector file\n" );
		ret = 1;
		break;
	    } else if ( type == -2 ) { /* EOF */ 
		break;
	    } else if ( type == 0 ) { /* dead line */
		continue;
	    }
	    Vect_write_line ( Out, type, Points, Cats );
	}
    }
    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);

    return ret;
}

/*!
 \fn int Vect_copy ( char *in, char *mapset, char *out, FILE *msgout )
 \brief copy a map including attribute tables
 \return -1 error, 0 success
 \param in input vector
 \param out output vector
 \param msgout output file for messages or NULL 
*/
int 
Vect_copy ( char *in, char *mapset, char *out, FILE *msgout )
{
    int i, n, ret, type;
    struct Map_info In, Out;
    struct field_info *Fi, *Fin;

    G_debug (3, "Copy vector '%s' in '%s' to '%s'", in, mapset, out );

    /* Open input */
    Vect_set_open_level (2);
    Vect_open_old (&In, in, mapset);
    
    /* Open output */
    Vect_open_new (&Out, out, Vect_is_3d(&In) );

    /* Copy history */
    Vect_hist_copy (&In, &Out);
    Vect_hist_command ( &Out );
    
    /* Copy lines */
    ret = Vect_copy_map_lines ( &In, &Out );
    if ( ret == 1 ) {
	G_warning ( "Cannot copy vector lines" );
	return -1;
    }

    /* Copy tables */
    n = Vect_get_num_dblinks ( &In );
    type = GV_1TABLE;
    if ( n > 1 ) type = GV_MTABLE;
    for ( i = 0; i < n; i++ ) {
	Fi = Vect_get_dblink ( &In, i );
	if ( Fi == NULL ) {
	    G_warning ( "Cannot get db link info" );
	    Vect_close ( &In );
	    Vect_close ( &Out );
	    return -1;
	}
	Fin = Vect_default_field_info ( &Out, Fi->number, Fi->name, type );
        G_debug (3, "Copy drv:db:table '%s:%s:%s' to '%s:%s:%s'", 
	              Fi->driver, Fi->database, Fi->table, Fin->driver, Fin->database, Fin->table );
	Vect_map_add_dblink ( &Out, Fi->number, Fi->name, Fin->table, Fi->key, Fin->database, Fin->driver);
        
	ret = db_copy_table ( Fi->driver, Fi->database, Fi->table, 
		    Fin->driver, Vect_subst_var(Fin->database,&Out), Fin->table );
	if ( ret == DB_FAILED ) {
	    G_warning ( "Cannot copy table" );
	    Vect_close ( &In );
	    Vect_close ( &Out );
	    return -1;
	}
    }
    
    Vect_build ( &Out, msgout );
    Vect_close ( &In );
    Vect_close ( &Out );

    return 0;
}


/*!
 \fn int Vect_delete ( char *map )
 \brief delete a map including attribute tables
 \return -1 error, 0 success
 \param  map name
*/
int 
Vect_delete ( char *map )
{
    int i, n, ret;
    struct Map_info Map;
    struct field_info *Fi;
    char   buf[5000];
    DIR    *dir;
    struct dirent *ent; 

    G_debug (3, "Delete vector '%s'", map );

    G_chop ( map );

    if ( map == NULL || strlen ( map ) == 0 ) {
	G_warning ( "Wrong map name '%s'", map );
	return -1;
    }

    /* Open input */
    Vect_set_open_level (1); /* Topo not needed */
    ret = Vect_open_old (&Map, map, G_mapset());
    if ( ret < 1 ) {
	G_warning ( "Cannot open vector %s", map );
	return -1;
    }

    /* Delete all tables, NOT external like shapefile */
    if ( Map.format == GV_FORMAT_NATIVE ) {
	n = Vect_get_num_dblinks ( &Map );
	for ( i = 0; i < n; i++ ) {
	    Fi = Vect_get_dblink ( &Map, i );
	    if ( Fi == NULL ) {
		G_warning ( "Cannot get db link info" );
		Vect_close ( &Map );
		return -1;
	    }
	    G_debug (3, "Delete drv:db:table '%s:%s:%s'", Fi->driver, Fi->database, Fi->table);
	    
	    ret = db_table_exists ( Fi->driver, Fi->database, Fi->table );
	    if ( ret == -1 ) {
		G_warning ( "Cannot get info if table '%s' linked to vector exists.", Fi->table );
		Vect_close ( &Map );
		return -1;
            }
	    
	    if ( ret == 1 ) {
		ret = db_delete_table ( Fi->driver, Fi->database, Fi->table );
		if ( ret == DB_FAILED ) {
		    G_warning ( "Cannot delete table" );
		    Vect_close ( &Map );
		    return -1;
		}
	    } else {
		G_warning ( "Table '%s' linked to vector did not exist.", Fi->table );
	    }
	}
    }
    Vect_close ( &Map );

    /* Delete all files from vector/name directory */
    sprintf ( buf, "%s/%s/vector/%s", G_location_path(), G_mapset(), map );
    G_debug (3, "opendir '%s'", buf ); 
    dir = opendir( buf );
    if (dir == NULL) {
	G_warning ( "Cannot open directory '%s'", buf );
	return -1;
    }

    while ( (ent = readdir (dir)) ) {
	if ( (strcmp (ent->d_name, ".") == 0) || (strcmp (ent->d_name, "..") == 0) ) continue;
	sprintf ( buf, "%s/%s/vector/%s/%s", G_location_path(), G_mapset(), map, ent->d_name );
	G_debug (3, "delete file '%s'", buf );
	ret = unlink ( buf );
	if ( ret == -1 ) { 
	    G_warning ( "Cannot delete file '%s'", buf );
	    closedir (dir);
	    return -1;
	}
    }
    closedir (dir);

    sprintf ( buf, "%s/%s/vector/%s", G_location_path(), G_mapset(), map );
    G_debug (3, "delete directory '%s'", buf );
    ret = rmdir ( buf );
    if ( ret == -1 ) { 
	G_warning ( "Cannot delete directory '%s'", buf );
	return -1;
    }

    return 0;
}

/*!
 \fn int Vect_copy_tables ( struct Map_info *In, struct Map_info *Out, int field )
 \brief Copy map tables. All if field = 0, or table defined by given field if field > 0
 \return 0 on success, -1 on error
 \param  in Map_info structure, out Map_info structure, field number 
*/
int 
Vect_copy_tables ( struct Map_info *In, struct Map_info *Out, int field )
{
    int i, n, ret, type;
    struct field_info *Fi, *Fin;

    G_debug (2, "Vect_copy_tables()");

    n = Vect_get_num_dblinks ( In );
    type = GV_1TABLE;
    if ( n > 1 ) type = GV_MTABLE;

    for ( i = 0; i < n; i++ ) {
	Fi = Vect_get_dblink ( In, i );
	if ( Fi == NULL ) {
	    G_warning ( "Cannot get db link info" );
	    return -1;
	}
	if ( field > 0 && Fi->number != field ) continue;

	Fin = Vect_default_field_info ( Out, Fi->number, Fi->name, type );
        G_debug (2, "Copy drv:db:table '%s:%s:%s' to '%s:%s:%s'", 
	              Fi->driver, Fi->database, Fi->table, Fin->driver, Fin->database, Fin->table );
	
	ret = Vect_map_add_dblink ( Out, Fi->number, Fi->name, Fin->table, Fi->key, Fin->database, Fin->driver);
	if ( ret == -1 ) {
	    G_warning ( "Cannot add database link" );
	    return -1;
	}
        
	ret = db_copy_table ( Fi->driver, Fi->database, Fi->table, 
		    Fin->driver, Vect_subst_var(Fin->database,Out), Fin->table );
	if ( ret == DB_FAILED ) {
	    G_warning ( "Cannot copy table" );
	    return -1;
	}
    }

    return 0;
}

/*!
 \fn int Vect_copy_table ( struct Map_info *In, struct Map_info *Out, int field_in, 
                           int field_out, char *field_name, int type )
 \brief Copy map table.
 \return 0 on success, -1 on error
 \param In 
 \param Out
 \param field_in
 \param field_out
 \param field_name 
 \param type
*/
int 
Vect_copy_table ( struct Map_info *In, struct Map_info *Out, int field_in, 
	           int field_out,  char *field_name, int type )
{
    return Vect_copy_table_by_cats ( In, Out, field_in, field_out, field_name, type, NULL, 0); 
}

/*!
 \fn int Vect_copy_table_by_cats ( struct Map_info *In, struct Map_info *Out, int field_in, 
                           int field_out, char *field_name, int type, int *cats, int ncats )
 \brief Copy map table.
 \return 0 on success, -1 on error
 \param In 
 \param Out
 \param field_in
 \param field_out
 \param field_name 
 \param type
 \param cats pointer to array of cats or NULL
 \param ncats number of cats in 'cats'
*/
int 
Vect_copy_table_by_cats ( struct Map_info *In, struct Map_info *Out, int field_in, 
	           int field_out,  char *field_name, int type, int *cats, int ncats )
{
    int    ret;
    struct field_info *Fi, *Fin;
    char   *name, *key;

    G_debug (2, "Vect_copy_table(): field_in = %d field_out = %d", field_in, field_out);

    Fi = Vect_get_field ( In, field_in );
    if ( Fi == NULL ) {
	G_warning ( "Cannot get db link info" );
	return -1;
    }

    if ( field_name != NULL ) name = field_name;
    else name = Fi->name;
    
    Fin = Vect_default_field_info ( Out, field_out, name, type );
    G_debug (3, "Copy drv:db:table '%s:%s:%s' to '%s:%s:%s'", 
		  Fi->driver, Fi->database, Fi->table, Fin->driver, Fin->database, Fin->table );
    
    ret = Vect_map_add_dblink ( Out, Fin->number, Fin->name, Fin->table, Fi->key, Fin->database, Fin->driver);
    if ( ret == -1 ) {
	G_warning ( "Cannot add database link" );
	return -1;
    }
    
    if ( cats ) 
	key = Fi->key;
    else 
	key = NULL;
    
    ret = db_copy_table_by_ints ( Fi->driver, Fi->database, Fi->table, 
		Fin->driver, Vect_subst_var(Fin->database,Out), Fin->table, key, cats, ncats );
    if ( ret == DB_FAILED ) {
	G_warning ( "Cannot copy table" );
	return -1;
    }

    return 0;
}
