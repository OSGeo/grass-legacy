#include <stdlib.h>
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

    driver = db_start_driver_open_database ( drvname, dbname );
    if ( driver == NULL ) {
        G_warning ( "Cannot open database '%s' by driver '%s'", dbname, drvname );
	return -1;
    }
    
    /* user tables */
    if( db_list_tables (driver, &names, &count, 0) != DB_OK) return (-1);

    for (i = 0; i < count; i++) {
	if ( G_strcasecmp( tabname, db_get_string (&names[i])) == 0 ) {
            found = 1;
	    break;
	}
    }
    db_free_string_array(names, count);
    
    if ( !found ) {    /* system tables */
	if( db_list_tables (driver, &names, &count, 1) != DB_OK) return (-1);

	for (i = 0; i < count; i++) {
	    if ( G_strcasecmp( tabname, db_get_string (&names[i])) == 0 ) {
		found = 1;
		break;
	    }
	}
	db_free_string_array(names, count);
    }
    db_close_database_shutdown_driver ( driver );

    return (found);
}
