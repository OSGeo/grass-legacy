/*
 * $Id$
 */

#include <stdlib.h>
#include "gis.h"
#include <dbmi.h>
#include <stdio.h>

static int cmp();
static int srch();

int db_rcls (dbRclsRule *rule, dbCatValI **rcl, int *num)
{
    char buf[256];
    char *key;
    int i, j, nalloc=0, nused=0, nmax=0;
    char sel[1024];
    int more;
    int oldcat, found;
    int *fcat;  /* array for index of first structure written to *lrcl for each rule */
    dbConnection connection;    
    dbString stmt;
    dbDriver *driver;
    dbHandle handle;  
    dbCursor cursor;
    dbColumn *column;
    dbValue *value;
    dbTable *table;
    dbCatValI *lrcl, *cval;

    /* allocate fcat */
    fcat = (int *) G_calloc ( rule->count+1, sizeof(int));

    /* get connection parameters */
    db_get_connection( &connection );

    /* get key column name */
    if ( rule->key != NULL )
        key = rule->key;
    else
    {    
	key = connection.keycol;
	if ( strlen (key) == 0 )
	{
	    sprintf (buf, "key column was not specified");
	    G_fatal_error (buf);
	    exit(1);
	}
    }

    /* start default driver */
    driver = db_start_driver(NULL); 
    if (driver == NULL)
    {
        sprintf (buf, "cannot open dbmi driver");
        G_fatal_error (buf);
        exit(1);
    }

    /* open default database */
    db_init_handle (&handle);
    db_set_handle (&handle, NULL, NULL);
    if (db_open_database(driver, &handle) != DB_OK)
    {
        sprintf (buf, "cannot open database");
        G_fatal_error (buf);
        exit(1);
    }

    /* find size of array needed for categories to avoid reallocating */
    for(i=0;i<rule->count;i++)
    {
	snprintf(sel,1024,
	    "SELECT COUNT(*) FROM %s WHERE %s > 0 and %s",rule->table,key,rule->where[i]);

#ifdef DEBUG
	printf ("SQL: %s\n", sel);
#endif 

	db_init_string ( &stmt);
	db_append_string ( &stmt, sel);

        if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
            return DB_FAILED;

	table = db_get_cursor_table (&cursor);

	/* fetch the data */
	if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	    return DB_FAILED;

        column = db_get_table_column(table, 0); /* first column (key) */
	value  = db_get_column_value(column);
	nmax += db_get_value_int(value);
	
	db_close_cursor(&cursor);
    }
    
    /* alloc reclass table */
    lrcl = (dbCatValI *) G_calloc ( nmax, sizeof(dbCatValI));

    /* SQL */
    for(i=0;i<rule->count;i++)
    {
	fcat[i+1] = fcat[i];  /* index for first structere used for next rule */
	snprintf(sel,1024,
	    "SELECT DISTINCT %s FROM %s WHERE %s > 0 and ( %s ) ORDER BY %s",key,rule->table,key,rule->where[i], key);
#ifdef DEBUG
	printf ("SQL: %s\n", sel);
#endif 

	db_init_string ( &stmt);
	db_append_string ( &stmt, sel);

        if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
            return DB_FAILED;

	table = db_get_cursor_table (&cursor);

	/* fetch the data */
	while(1)
        {
	    if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
	        return DB_FAILED;

	    if (!more)
	        break;  
						
            column = db_get_table_column(table, 0); /* first column (key) */
	    value  = db_get_column_value(column);
	    oldcat = db_get_value_int(value);

	    found=0;
	    for(j=0; j<= i; j++) /* go through processed rules   */
	    {
		cval = (dbCatValI *) bsearch((void *) &oldcat, &lrcl[fcat[j]], fcat[j+1]-fcat[j], sizeof(dbCatValI), srch);
	        if ( cval != NULL)            /* oldcat already exist, */
		{                             /* usually should not happen - results of rules overlap , */
	            cval->val = rule->cat[i]; /* replace by new one */
		    found = 1;
		    break;
	        }
	    }	    
	    
	    if ( found == 0 && nused < nmax) /* usually nused should be < nmax if no changes happend in DB since we found nmax */
	    {
		lrcl[nused].cat = oldcat;
		lrcl[nused].val = rule->cat[i];
		nused++;
		fcat[i+1] = nused;  /* first index for next rule */
	    }
	}
	db_close_cursor(&cursor);
    }

    db_close_database(driver);
    db_shutdown_driver(driver);

    qsort( (void *)lrcl, nused, sizeof(dbCatValI), cmp);

    *rcl = lrcl; 
    *num = nused;

    return DB_OK;
}

int cmp ( const void *pa, const void *pb)
{
    dbCatValI *p1 = (dbCatValI *) pa;    
    dbCatValI *p2 = (dbCatValI *) pb;

    if( p1->cat < p2->cat)
        return -1;
    if( p1->cat > p2->cat)
        return 1;
    return 0;
}

int srch ( const void *pa, const void *pb)
{
    int       *p1 = (int *) pa;    
    dbCatValI *p2 = (dbCatValI *) pb;

    if( *p1 < p2->cat)
        return -1;
    if( *p1 > p2->cat)
        return 1;
    return 0;
}
