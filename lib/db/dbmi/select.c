#include <stdlib.h>
#include <string.h>
#include <gis.h>
#include <dbmi.h>

static int cmp();

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
/* selet array of ordered integers
 *
 * return: number of selected values
 *         -1 on error
 */

int db_select_int (dbDriver *driver, char *tab, char *col, char *where, int **pval)
{
    int type, more, alloc, count;
    int *val;
    char buf[1024], *sval;
    dbString stmt;
    dbCursor cursor;
    dbColumn *column;
    dbValue *value;
    dbTable *table;
    
    G_debug (3, "db_select_int()" );
    
    /* allocate */
    alloc = 1000;
    val = (int *) G_malloc ( alloc * sizeof(int));

    if ( where == NULL || strlen(where) == 0 )
        snprintf(buf,1023, "SELECT %s FROM %s", col, tab);
    else
        snprintf(buf,1023, "SELECT %s FROM %s WHERE %s", col, tab, where);

    G_debug (3, "  SQL: %s", buf );
    
    db_init_string ( &stmt);
    db_append_string ( &stmt, buf);

    if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
            return (-1);

    table = db_get_cursor_table (&cursor);
    column = db_get_table_column(table, 0); /* first column */
    value  = db_get_column_value(column);
    type = db_get_column_sqltype(column);
    type = db_sqltype_to_Ctype(type);

    /* fetch the data */
    count = 0;
    while(1)
      {
        if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	    return (-1);

        if (!more) break;  
						
	if ( count == alloc )
	  {
            alloc += 1000;		  
            val = (int *) G_realloc ( val, alloc * sizeof(int));
	  }
	
	switch ( type )
	  {
	    case ( DB_C_TYPE_INT ):
                val[count] = db_get_value_int(value);
	        break;
	    case ( DB_C_TYPE_STRING ):
                sval = db_get_value_string(value);
                val[count] = atoi(sval);
	        break;
	    case ( DB_C_TYPE_DOUBLE ):
                val[count] = (int) db_get_value_double(value);
	        break;
            default:
	    	return (-1);
	  }
	count++;
    }

    db_close_cursor(&cursor);
    db_free_string ( &stmt );

    qsort( (void *)val, count, sizeof(int), cmp);

    *pval = val; 

    return (count);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
/* selet one (first) value from table/column for key/id
 *
 * return: number of selected values
 *         -1 on error
 */
int db_select_value (dbDriver *driver, 
	char *tab, /* table name */
	char *key, /* key column name (integer) */
	int  id,   /* identifier in key column */
	char *col, /* the name of column to select the value from */
	dbValue *val) /* pointer to existing dbValue to store within */ 
{
    int  more, count;
    char buf[1024];
    dbString stmt;
    dbCursor cursor;
    dbColumn *column;
    dbValue *value;
    dbTable *table;
    
    sprintf( buf, "SELECT %s FROM %s WHERE %s = %d\n", col, tab, key, id);
    db_init_string ( &stmt);
    db_append_string ( &stmt, buf);

    if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
        return (-1);

    table = db_get_cursor_table (&cursor);
    column = db_get_table_column(table, 0); /* first column */
    value  = db_get_column_value(column);

    /* fetch the data */
    count = 0;
    while(1) {
        if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	    return (-1);

        if (!more) break;  
	if ( count == 0 ) db_copy_value ( val, value );
	count++;
    }
    db_close_cursor(&cursor);
    db_free_string ( &stmt );

    return (count);
}

int cmp ( const void *pa, const void *pb)
{
    int *p1 = (int *) pa;    
    int *p2 = (int *) pb;

    if( *p1 < *p2 ) return -1;
    if( *p1 > *p2 ) return 1;
    return 0;
}

int cmpcat ( const void *pa, const void *pb)
{
    dbCatVal *p1 = (dbCatVal *) pa;    
    dbCatVal *p2 = (dbCatVal *) pb;

    if( p1->cat < p2->cat ) return -1;
    if( p1->cat > p2->cat ) return 1;
    return 0;
}

int cmpcatkey ( const void *pa, const void *pb)
{
    int *p1 = (int *) pa;    
    dbCatVal *p2 = (dbCatVal *) pb;

    if( *p1 < p2->cat ) return -1;
    if( *p1 > p2->cat ) return 1;
    return 0;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
/* selet pairs key/value to array, values are sorted by key
 *
 * return: number of selected values
 *         -1 on error
 */

int db_select_CatValArray ( dbDriver *driver, char *tab, char *key, char *col, char *where, 
	                 dbCatValArray *cvarr )
{
    int  i, type, more, nrows;
    char buf[1024];
    dbString stmt;
    dbCursor cursor;
    dbColumn *column;
    dbValue *value;
    dbTable *table;
    
    G_debug (3, "db_select_db_select_CatValArray ()" );
    
    db_init_string ( &stmt);
    
    sprintf( buf, "SELECT %s, %s FROM %s", key, col, tab);
    db_set_string ( &stmt, buf);

    if ( where != NULL && strlen(where) > 0 ) {
        db_append_string ( &stmt, " WHERE ");
        db_append_string ( &stmt, where );
    }

    G_debug (3, "  SQL: %s", db_get_string ( &stmt ) );
    
    if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
            return (-1);

    nrows = db_get_num_rows ( &cursor );
    G_debug (3, "  %d rows selected", nrows );
    if ( nrows < 0 ) G_fatal_error ( "Cannot select rows from database");
	
    db_CatValArray_alloc( cvarr, nrows );

    table = db_get_cursor_table (&cursor);

    /* Check if key column is integer */
    column = db_get_table_column(table, 0); 
    type = db_sqltype_to_Ctype( db_get_column_sqltype(column) );
    G_debug (3, "  key type = %d", type );

    if ( type != DB_C_TYPE_INT ) {
	G_fatal_error ( "Key column type is not integer" );
    }	

    column = db_get_table_column(table, 1); 
    type = db_sqltype_to_Ctype( db_get_column_sqltype(column) );
    G_debug (3, "  col type = %d", type );

    if ( type != DB_C_TYPE_INT && type != DB_C_TYPE_DOUBLE ) {
	G_fatal_error ( "Column type not supported by db_select_to_array()" );
    }	

    /* fetch the data */
    for ( i = 0; i < nrows; i++ ) {
        if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	    return (-1);

	column = db_get_table_column(table, 0); /* first column */
	value  = db_get_column_value(column);
	cvarr->value[i].cat = db_get_value_int(value);

	column = db_get_table_column(table, 1);
	value  = db_get_column_value(column);
	switch ( type ) {
	    case ( DB_C_TYPE_INT ):
                cvarr->value[i].val.i = db_get_value_int(value);
	        break;
	    case ( DB_C_TYPE_DOUBLE ):
                cvarr->value[i].val.d = db_get_value_double(value);
	        break;
            default:
	    	return (-1);
	}
    }
    cvarr->n_values = nrows;

    db_close_cursor(&cursor);
    db_free_string ( &stmt );

    if ( type == DB_C_TYPE_INT ) 
        qsort( (void *) cvarr->value, nrows, sizeof(dbCatVal), cmpcat);
    else if ( type == DB_C_TYPE_DOUBLE ) 
        qsort( (void *) cvarr->value, nrows, sizeof(dbCatVal), cmpcat);

    return (nrows);
}

/* find value by key
*  returns: DB_FAILED, DB_OK
*/
int
db_CatValArray_get_value_int ( dbCatValArray *arr, int key, int *val )
{
    dbCatVal *catval;
    
    catval = bsearch ( (void *) &key, arr->value, arr->n_values, sizeof ( dbCatVal ), cmpcat );
    if ( catval == NULL ) { return DB_FAILED; }

    *val = catval->val.i;
    
    return DB_OK;
}

/* find value by key
*  returns: 0 not found, 1 OK
*/
int
db_CatValArray_get_value_double ( dbCatValArray *arr, int key, double *val )
{
    dbCatVal *catval;

    G_debug (3, "db_CatValArray_get_value_double(), key = %d", key );
    
    catval = bsearch ( (void *) &key, arr->value, arr->n_values, sizeof ( dbCatVal ), cmpcatkey );
    if ( catval == NULL ) { return DB_FAILED; }

    *val = catval->val.d;
    
    return DB_OK;
}

