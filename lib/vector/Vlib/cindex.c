/*****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek
*
* PURPOSE:      Functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"

static void check_status ( struct Map_info *Map ){
    if ( !Map->plus.cidx_up_to_date ) 
	G_fatal_error("Category index is not up to date");
}

/*!
 \fn int Vect_cidx_get_num_fields ( struct Map_info *Map )
 \brief get nuber of field in category index
 \return number of fields 
 \param Map_info structure
*/
int 
Vect_cidx_get_num_fields ( struct Map_info *Map ) 
{
    check_status ( Map );

    return ( Map->plus.n_cidx );
}

/*!
 \fn int Vect_cidx_get_field_number ( struct Map_info *Map, int index )
 \brief get field number for given index 
 \return field number 
 \param Map_info structure
 \param index field index: from 0 to Vect_cidx_get_num_fields()-1
*/
int 
Vect_cidx_get_field_number ( struct Map_info *Map, int index ) 
{
    check_status ( Map );

    if ( index >= Map->plus.n_cidx )
	G_fatal_error("Field index >= number of fields");
    
    return ( Map->plus.cidx[index].field );
}

/*!
 \fn int Vect_cidx_get_field_index ( struct Map_info *Map, int field )
 \brief get field index for given field number
 \return field index or -1 if not found 
 \param Map_info structure
 \param field field number 
*/
int 
Vect_cidx_get_field_index ( struct Map_info *Map, int field ) 
{
    int i;
    struct Plus_head *Plus ;
    
    G_debug (2, "Vect_cidx_get_field_index() field = %d", field); 

    check_status ( Map );
    Plus = &(Map->plus);

    for ( i = 0; i < Plus->n_cidx; i++ ) {
        if (  Plus->cidx[i].field == field )
	    return i;
    }

    return (-1);
}

/*!
 \fn int Vect_cidx_get_num_unique_cats_by_index ( struct Map_info *Map, int index )
 \brief get nuber of unique categories for given field index 
 \return number of unique categories or -1 on error
 \param Map_info structure
 \param field 
*/
int 
Vect_cidx_get_num_unique_cats_by_index ( struct Map_info *Map, int index ) 
{
    check_status ( Map );
    if ( index >= Map->plus.n_cidx )
	G_fatal_error("Field index >= number of fields");

    return ( Map->plus.cidx[index].n_ucats );
}

/*!
 \fn int Vect_cidx_get_num_cats_by_index ( struct Map_info *Map, int index )
 \brief get nuber of categories for given field index 
 \return number of categories or -1 on error
 \param Map_info structure
 \param index 
*/
int 
Vect_cidx_get_num_cats_by_index ( struct Map_info *Map, int index ) 
{
    check_status ( Map );
    if ( index >= Map->plus.n_cidx )
	G_fatal_error("Field index >= number of fields");

    return ( Map->plus.cidx[index].n_cats );
}

/*!
 \fn int Vect_cidx_get_num_types_by_index ( struct Map_info *Map, int field_index )
 \brief get nuber of types for given field index 
 \return number of types or -1 on error
 \param Map_info structure
 \param index 
*/
int 
Vect_cidx_get_num_types_by_index ( struct Map_info *Map, int field_index ) 
{
    check_status ( Map );
    if ( field_index >= Map->plus.n_cidx )
	G_fatal_error("Field index >= number of fields");

    return ( Map->plus.cidx[field_index].n_types );
}

/*!
 \fn int Vect_cidx_get_type_count_by_index ( struct Map_info *Map, int field_index, int type_index, 
                                    int *type, int *count )
 \brief get type count field index and type index
 \return 1 OK
 \return 0 on error
 \param Map_info structure
 \param index 
*/
int 
Vect_cidx_get_type_count_by_index ( struct Map_info *Map, int field_index, int type_index, int *type, int *count) 
{
    check_status ( Map );
    if ( field_index >= Map->plus.n_cidx )
	G_fatal_error("Field index >= number of fields");

    *type = Map->plus.cidx[field_index].type[type_index][0];
    *count = Map->plus.cidx[field_index].type[type_index][1];
    
    return (1);
}

/*!
 \fn int Vect_cidx_get_type_count ( struct Map_info *Map, int field, int type )
 \brief get count of features of certain type by field and type
 \return feature count, 0 if no features, no such field or no such type in cidx
 \param Map_info structure
 \param field 
 \param type 
*/
int 
Vect_cidx_get_type_count ( struct Map_info *Map, int field, int type) 
{
    int i, fi, count = 0;

    G_debug (3, "Vect_cidx_get_type_count() field = %d, type = %d", field, type); 
    
    check_status ( Map );
    
    if ( (fi = Vect_cidx_get_field_index(Map, field)) < 0 ) return 0;  /* field not found */
    G_debug (3, "field_index = %d", fi); 
    
    G_debug (3, "ntypes = %d", Map->plus.cidx[fi].n_types); 
    for ( i = 0; i < Map->plus.cidx[fi].n_types; i++ ) {
        int tp, cnt;
           
        tp = Map->plus.cidx[fi].type[i][0];
        cnt = Map->plus.cidx[fi].type[i][1];	    
	if ( tp & type ) count += cnt;
        G_debug (3, "%d tp = %d, cnt= %d count = %d", i, tp, cnt, count); 
    }
    
    return (count);
}

/*!
 \fn int Vect_cidx_get_cat_by_index ( struct Map_info *Map, int field_index, int cat_index, 
	                     int *cat, int *type, int * id)
 \brief get nuber of categories for given field and category index 
 \return 1 OK
 \return 0 on error
 \param Map
 \param field_index
 \param cat_index
 \param cat
 \param type
 \param id
*/
int 
Vect_cidx_get_cat_by_index ( struct Map_info *Map, int field_index, int cat_index, 
	                     int *cat, int *type, int * id)
{
    check_status ( Map ); /* This check is slow ? */

    if ( field_index >= Map->plus.n_cidx || field_index < 0 ||  cat_index >= Map->plus.cidx[field_index].n_cats )
	G_fatal_error("Field/cat index out of range");

    *cat = Map->plus.cidx[field_index].cat[cat_index][0];
    *type = Map->plus.cidx[field_index].cat[cat_index][1];
    *id = Map->plus.cidx[field_index].cat[cat_index][2];

    return 1;
}

/* Compare by cat */
static int cmp_cat ( const void *pa, const void *pb )
{
    int *p1 = (int*) pa;
    int *p2 = (int*) pb;

    if ( *p1 < p2[0] ) return -1;
    if ( *p1 > p2[0] ) return 1;
    return 0;
}   

/*!
 \fn int Vect_cidx_find_next ( struct Map_info *Map, int field_index, int cat, int type_mask,
                                  int start_index, int *type, int *id )
 \brief find next line/area id for given category, start_index and type_mask 
 \return index to array
 \return -1 not found
 \param Map
 \param field_index
 \param cat category
 \param type_mask requested type
 \param start_index start search at this index (0 - whole category index)
 \param type returned type
 \param id returned line/area id
*/
int 
Vect_cidx_find_next ( struct Map_info *Map, int field_index, int cat, int type_mask,
                                  int start_index, int *type, int *id )
{
    int    *catp, cat_index;
    struct Cat_index *ci;

    G_debug (3, "Vect_cidx_find_next() cat = %d, type_mask = %d, start_index = %d", cat, type_mask, start_index);

    check_status ( Map ); /* This check is slow ? */
    *type = *id = 0;

    if ( field_index >= Map->plus.n_cidx )
	G_fatal_error("Field index out of range");

    if ( start_index < 0 ) start_index = 0;
    if ( start_index >= Map->plus.cidx[field_index].n_cats ) return -1; /* outside range */
    
    /* pointer to beginning of searched part of category index */
    ci = &(Map->plus.cidx[field_index]);

    catp = bsearch ( &cat, ci->cat + start_index * 3 * sizeof(int), ci->n_cats - start_index, 
	             3 * sizeof(int), cmp_cat);

    G_debug (3, "catp = %p", catp);
    if ( !catp ) return -1;

    /* get index from pointer, the difference between pointers is using sizeof(int) !!! */
    cat_index = (catp - (int *)ci->cat) / 3;
    
    do {
        G_debug (3, "  cat_index = %d", cat_index);
	if ( ci->cat[cat_index][1] & type_mask ) {
	    *type = ci->cat[cat_index][1];
	    *id = ci->cat[cat_index][2];
            G_debug (3, "  type match -> record found");
	    return cat_index;
	}
	cat_index++;
    } while ( cat_index < ci->n_cats );
    
    return -1;
}

#define SEP "------------------------------------------------------------------------------------------\n"

/*!
 \fn int Vect_cidx_dump ( struct Map_info *Map, FILE *out )
 \brief write category index in text form to file
 \return 1 on success, 0 on error
 \param Map_info structure
 \param out output file
*/
int 
Vect_cidx_dump ( struct Map_info *Map, FILE *out ) 
{
    int i, field, nfields, ntypes;

    G_debug ( 2, "Vect_cidx_dump()"); 
    
    check_status ( Map );

    nfields = Vect_cidx_get_num_fields ( Map );
    fprintf (out, "---------- CATEGORY INDEX DUMP: Number of layers: %d "
	          "--------------------------------------\n", nfields );
    
    for (i = 0; i < nfields; i++ ) {
	int j, nucats, ncats; 
	
	field = Vect_cidx_get_field_number ( Map, i );
	nucats = Vect_cidx_get_num_unique_cats_by_index ( Map, i );
	ncats = Vect_cidx_get_num_cats_by_index ( Map, i );
        ntypes = Vect_cidx_get_num_types_by_index ( Map, i );

        fprintf (out, "Field %6d  number of unique cats: %7d  number of cats: %7d  number of types: %d\n", 
		               field, nucats, ncats, ntypes ); 
	fprintf (out, SEP);
	
        fprintf (out, "            type |     count\n" );
	for ( j = 0; j < ntypes; j++ ) {
	    int type, count;
            Vect_cidx_get_type_count_by_index ( Map, i, j, &type, &count );
	    fprintf (out, "           %5d | %9d\n", type, count );
	}
	
        fprintf (out, " category | type | line/area\n" );
	for ( j = 0; j < ncats; j++ ) {
	    int cat, type, id; 
	    Vect_cidx_get_cat_by_index ( Map, i, j, &cat, &type, &id );
	    fprintf (out, "%9d | %4d | %9d\n", cat, type, id );
	}
    
	fprintf (out, SEP);
    }

    return 1;
}

/*!
 \fn int Vect_cidx_save ( struct Map_info *Map )
 \brief write category index to file
 \return 0 on success
 \return 1 on error
 \param Map_info structure
*/
int 
Vect_cidx_save ( struct Map_info *Map ) 
{
    struct Plus_head *plus ;
    char   fname[1024], buf[1024];
    GVFILE  fp;

    G_debug (2, "Vect_cidx_save()"); 
    check_status ( Map );

    plus = &(Map->plus);
    
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    G__file_name (fname, buf, GV_CIDX_ELEMENT, Map->mapset);
    G_debug (2, "Open cidx: %s", fname);
    dig_file_init ( &fp );
    fp.file = fopen( fname, "w");
    if ( fp.file ==  NULL) {
        G_warning("Can't open cidx file for write: %s\n", fname);
	return 1;
    }

    /* set portable info */
    dig_init_portable ( &(plus->cidx_port), dig__byte_order_out ());

    if ( 0 > dig_write_cidx (&fp, plus) ) {
        G_warning ("Error writing out category index file.\n");
	return 1;
    }
    
    fclose( fp.file );

    return 0;
}

/*!
 \fn int Vect_cidx_open ( struct Map_info *Map )
 \brief read category index from file if exists
 \return 0 on success 
 \return 1 if file does not exist
 \return -1 error, file exists but cannot be read
 \param Map_info structure
*/
int 
Vect_cidx_open ( struct Map_info *Map, int head_only ) 
{
    int ret;
    char buf[500], file_path[2000];
    GVFILE fp;
    struct Plus_head *Plus;
    struct stat info;
    
    G_debug (2, "Vect_cidx_open(): name = %s mapset= %s", Map->name, Map->mapset);

    Plus = &(Map->plus);
    
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    G__file_name ( file_path, buf, GV_CIDX_ELEMENT, Map->mapset);

    if (stat (file_path, &info) != 0) /* does not exist */
	return 1;
	

    dig_file_init ( &fp );
    fp.file = G_fopen_old (buf, GV_CIDX_ELEMENT, Map->mapset);

    if ( fp.file == NULL ) { /* category index file is not available */
	G_warning( "Cannot open category index file for vector '%s@%s'.", Map->name, Map->mapset);
	return -1;
    }
  
    /* load category index to memory */
    dig_cidx_init ( Plus);
    ret = dig_read_cidx ( &fp, Plus, head_only );
    
    fclose ( fp.file );  
    
    if ( ret == 1 ) {
	G_debug (3, "Cannot read cidx");
	return -1;
    }

    return 0;
}

