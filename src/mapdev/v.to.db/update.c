#include "global.h"
#include "dbmi.h"

static int srch(); 

int 
update (void)
{
    int      i, more, *catexst, *cex, upd, fcat;
    char     buf1[1024], buf2[1024];
    dbString stmt; 
    dbDriver *driver;
    dbHandle handle;
    dbCursor cursor;
    dbTable  *table;      
    dbColumn *column;
    dbValue  *value;

    vstat.dupl=0;     
    vstat.exist=0;
    vstat.notexist=0;    
    vstat.update=0;
    vstat.error=0;
    db_init_string (&stmt);	
    driver = db_start_driver(NULL);
    if (driver == NULL) exit(-1);  
    
    db_init_handle (&handle);
    db_set_handle (&handle, NULL, NULL);
    if (db_open_database(driver, &handle) != DB_OK) exit(-1);  
    
    /* select count of existing categories */
    snprintf (buf1,1023, "select count(*) from %s", options.table);    
    db_set_string (&stmt, buf1);
    if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
        return (ERROR);
    table = db_get_cursor_table (&cursor);
    if( db_fetch (&cursor, DB_NEXT, &more ) != DB_OK ) return ERROR;
    column = db_get_table_column(table, 0);
    value  = db_get_column_value(column);
    vstat.select = db_get_value_int(value); 
    db_close_cursor(&cursor);
    
    /* allocate array */
    catexst = (int *) G_malloc (vstat.select * sizeof(int));
    
    /* select existing categories */
    snprintf (buf1,1023, "select %s from %s order by %s", options.key, options.table, options.key);    
    db_set_string (&stmt, buf1);
    if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
        return (ERROR);
    table = db_get_cursor_table (&cursor);
    i=0;
    while(1) {
	if ( db_fetch (&cursor, DB_NEXT, &more ) != DB_OK ) return ERROR;
	if (!more) break;    
	column = db_get_table_column(table, 0);
	value  = db_get_column_value(column);
	if ( i < vstat.select ) catexst[i] = db_get_value_int(value);
	i++;
    }

    db_close_cursor(&cursor);
    
    /* create beginning of stmt */
    switch (options.option) {
        case O_CAT:	 
	    snprintf (buf1,1023, "insert into %s ( %s ) values ", options.table, options.key);
            break;	

        case O_COUNT:
        case O_LENGTH:
	case O_AREA:
        case O_LABEL:		
	    snprintf (buf1,1023, "update %s set %s =", options.table, options.col1);
            break;

        case O_COOR:
	    snprintf (buf1,1023, "update %s set ", options.table);	
            break;
    } 

    /* update */
    for ( i = 0; i < vstat.rcat; i++ ) {
	switch (options.option) {
    	    case O_CAT:	 
	        snprintf (buf2,1023, "%s ( %d )", buf1, list_ci[i].cat);
		fcat = list_ci[i].cat;
        	break;	

    	    case O_COUNT:
	        snprintf (buf2,1023, "%s %d where %s = %d", buf1, list_ci[i].i1, options.key,  list_ci[i].cat);
        	fcat = list_ci[i].cat;
		break;

    	    case O_LABEL:
    		snprintf (buf2,1023, "%s '%s' where %s = %d", buf1, list_cc[i].c1, options.key,  list_cc[i].cat);
		fcat = list_cc[i].cat;
        	break;
	
    	    case O_LENGTH:
	    case O_AREA:
    		snprintf (buf2,1023, "%s %f where %s = %d", buf1, list_cd[i].d1, options.key,  list_cd[i].cat);
		fcat = list_cd[i].cat;
        	break;

    	    case O_COOR:
		if ( list_ci2d[i].i1 > 1 ){
		    fprintf (stderr, "category %d has more elements > null values\n", list_ci2d[i].cat);
		    vstat.dupl++;
		    continue;
		}		
    		snprintf (buf2,1023, "%s %s = %f, %s = %f  where %s = %d", buf1, options.col1, list_ci2d[i].d1, options.col2, list_ci2d[i].d2, options.key,  list_ci2d[i].cat);    		
		fcat = list_ci2d[i].cat;
		break;
	} 

	db_set_string (&stmt, buf2);
	
	/* category exist in DB ? */
	cex = (int *) bsearch((void *) &fcat, catexst, vstat.select, sizeof(int), srch);
	
        if ( options.option == O_CAT && cex == NULL ){
	    upd = 1;
	    vstat.notexist++;
	}
	if ( options.option == O_CAT && cex != NULL ){
	    fprintf (stderr, "cat %d: row already exists (not inserted)\n", fcat);
	    upd = 0;
	    vstat.exist++;
	}     
        if ( options.option != O_CAT && cex != NULL ){
	    upd = 1;
	    vstat.exist++;
	}
	if ( options.option != O_CAT && cex == NULL ){
	    fprintf (stderr, "cat %d row does not exist (not updated)\n", fcat);
	    upd = 0;
	    vstat.notexist++;
	}
	if ( options.sql )  fprintf (stdout, "%s\n", db_get_string (&stmt) );
	else
	    if ( upd == 1 ){
		if ( db_execute_immediate (driver, &stmt) == DB_OK ){
		    vstat.update++;
		} else {    
		    vstat.error++;
		    if ( vstat.error >= vstat.maxerror )	{
			fprintf (stderr, "Maximum number of errors reached! Updating broken.\n");
			break;
		    }	
		}
	    }	
    }

    free(catexst);	
    db_close_database(driver);
    db_shutdown_driver(driver); 
    db_free_string (&stmt);

    return OK;
}

int srch ( const void *pa, const void *pb)
{
    int       *p1 = (int *) pa;
    int       *p2 = (int *) pb;    

    if( *p1 < *p2 )
    return -1;
    if( *p1 > *p2)
    return 1;
    return 0;
}  
