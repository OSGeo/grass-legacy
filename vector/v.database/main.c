/***************************************************************
 *
 * MODULE:       v.database
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Set default settings for vector map attribute storage
 *               
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"

int
main(int argc, char *argv[])
{
    struct Flag *print;
    struct Option *driver, *database, *schema;
    struct GModule *module;
    dbConnection  connection;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    
    print = G_define_flag();
    print->key               = 'p';
    print->description       = "print current values";
    
    driver = G_define_option() ;
    driver->key        = "driver" ;
    driver->type       = TYPE_STRING ;
    driver->required   = NO  ;
    driver->multiple   = NO ;
    driver->description = "Driver name." ;
    driver->options    = db_list_drivers();

    database = G_define_option() ;
    database->key        = "database" ;
    database->type       = TYPE_STRING ;
    database->required   = NO  ;
    database->multiple   = NO ;
    database->description = "Database name." ;

    schema = G_define_option() ;
    schema->key        = "schema" ;
    schema->type       = TYPE_STRING ;
    schema->required   = NO  ;
    schema->multiple   = NO ;
    schema->description = "Database schema. Don't use this option if schemas are not supported "
	                 "by driver/database server.";

    /* Set description */
    module              = G_define_module();
    module->description = "Set default driver / database for new vector attributes.";

    if(G_parser(argc, argv)) exit(1);

    /* set */
    if ( driver->answer )
	G_setenv2 ( "GV_DRIVER", driver->answer, G_VAR_MAPSET );
		
    if ( database->answer )
	G_setenv2 ( "GV_DATABASE", database->answer, G_VAR_MAPSET );

    if ( schema->answer )
	G_setenv2 ( "GV_SCHEMA", schema->answer, G_VAR_MAPSET );

    /* Set also DB_DRIVER and DB_DATABASE if not yet set */
    db_get_connection( &connection );
    if ( driver->answer && database->answer && !connection.driverName && !connection.databaseName ) {
        G_warning ( "Database connection (for db.* modules) also set to:\n"
                     "driver: %s\ndatabase: %s", driver->answer, database->answer );
	connection.driverName = driver->answer;
	connection.databaseName = database->answer;
	db_set_connection( &connection );
    }

    /* get and print */
    if( print->answer)  {
        fprintf(stdout, "driver:%s\n", G__getenv2( "GV_DRIVER", G_VAR_MAPSET) );
        fprintf(stdout, "database:%s\n", G__getenv2( "GV_DATABASE", G_VAR_MAPSET) );    
        fprintf(stdout, "schema:%s\n", G__getenv2( "GV_SCHEMA", G_VAR_MAPSET) );    
    }

    exit(0);
}
