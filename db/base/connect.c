#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "codes.h"
#include "glocale.h"

/* database for DBF can be written with variables:
   database=$GISDBASE/$LOCATION_NAME/$MAPSET/dbf
 */

int
main(int argc, char *argv[])
{
    dbConnection conn;
    struct Flag *print;
/*    struct Option *driver, *database, *user, *password, *keycol;*/
    struct Option *driver, *database, *schema;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    
    print = G_define_flag();
    print->key               = 'p';
    print->description       = "print current connection parameters and exit";
    
    driver = G_define_option() ;
    driver->key        = "driver" ;
    driver->type       = TYPE_STRING ;
    driver->options    = db_list_drivers();
    driver->required   = NO  ;
    driver->multiple   = NO ;
    driver->description= "driver name:" ;
    driver->answer = db_get_default_driver_name();

    database = G_define_option() ;
    database->key        = "database" ;
    database->type       = TYPE_STRING ;
    database->required   = NO  ;
    database->multiple   = NO ;
    database->description= "Database name:" ;
    database->answer = db_get_default_database_name();

    schema = G_define_option() ;
    schema->key        = "schema" ;
    schema->type       = TYPE_STRING ;
    schema->required   = NO  ;
    schema->multiple   = NO ;
    schema->answer     = db_get_default_schema_name();
    schema->description = "Database schema. Don't use this option if schemas are not supported "
                          "by driver/database server.";
    
/* commented due to new mechanism:
    user = G_define_option() ;
    user->key        = "user" ;
    user->type       = TYPE_STRING ;
    user->required   = NO  ;
    user->multiple   = NO ;
    user->description= "User:" ;    

    password = G_define_option() ;
    password->key        = "password" ;
    password->type       = TYPE_STRING ;
    password->required   = NO  ;
    password->multiple   = NO ;
    password->description= "Password:" ;
*/

    /* Set description */
    module              = G_define_module();
    module->description = ""\
    "Connect to the database through DBMI.";


    if(G_parser(argc, argv))
	exit(ERROR);

    /* set connection*/
    if( !print->answer) 
    {
        db_get_connection( &conn );  /* read current */
	
	if ( driver->answer )
	    conn.driverName = driver->answer;
	    	    
	if ( database->answer )
	    conn.databaseName = database->answer;

	if ( schema->answer )
	    conn.schemaName = schema->answer;

/* commented due to new mechanism:
	if ( user->answer )
	    conn.user = user->answer;

	if ( password->answer )
	    conn.password = password->answer;
*/

	db_set_connection( &conn );    
	
/*	if ( (conn.password != NULL) && ( strlen (conn.password) > 0))
	    fprintf(stdout, "Warning: Your password was written to %s in readable form!!!\n", getenv ("GISRC"));    
*/
    }
    
    /* get and print connection */
    db_get_connection( &conn );    
    
    fprintf(stdout, _("driver:%s\n"), conn.driverName);
    fprintf(stdout, _("database:%s\n"), conn.databaseName);    
    fprintf(stdout, _("schema:%s\n"), conn.schemaName);    
/* commented due to new mechanism:
    fprintf(stdout, "user:%s\n", conn.user);
    fprintf(stdout, "password:%s\n", conn.password);    
 */
    exit(OK);
}

