#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"

/*!
 \fn 
 \brief 
 \return: 1 exist, 0 doesn't exist, -1 error
 \param 
*/
int
db_table_exists ( char *drvname, char *dbname, char *tabname)
{
    dbDriver *driver;
    dbString *names;
    int i, count, found = 0;
    int full = 0;
    char buf[1000];
    char *bufp, *c;

    if ( strchr ( tabname, '.' ) ) 
	full = 1;
    
    driver = db_start_driver_open_database ( drvname, dbname );
    if ( driver == NULL ) {
        G_warning ( "Cannot open database '%s' by driver '%s'", dbname, drvname );
	return -1;
    }
    
    /* The table tabname can be either fully qualified in form table.schema,
     * or it can be only table name. If the name is fully qualified, compare whole name,
     * if it is not, compare only table names */
    
    /* user tables */
    if( db_list_tables (driver, &names, &count, 0) != DB_OK) return (-1);

    for (i = 0; i < count; i++) {
	strcpy ( buf, db_get_string (&names[i]) );
	bufp = buf;
	if ( !full && (c=strchr(buf,'.')) ) {
		bufp = c+1;
	}
	G_debug ( 2, "table = %s -> %s", buf, bufp );
	if ( G_strcasecmp( tabname, bufp) == 0 ) {
            found = 1;
	    break;
	}
    }
    db_free_string_array(names, count);
    
    if ( !found ) {    /* system tables */
	if( db_list_tables (driver, &names, &count, 1) != DB_OK) return (-1);

	for (i = 0; i < count; i++) {
	    strcpy ( buf, db_get_string (&names[i]) );
	    bufp = buf;
	    if ( !full && (c=strchr(buf,'.')) ) {
	        bufp = c+1;
	    }
	    if ( G_strcasecmp( tabname, bufp) == 0 ) {
		found = 1;
		break;
	    }
	}
	db_free_string_array(names, count);
    }
    db_close_database_shutdown_driver ( driver );

    return (found);
}
