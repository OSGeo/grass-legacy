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
    struct Option *driver, *database, *schema, *group;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    
    print = G_define_flag();
    print->key               = 'p';
    print->description       = _("print current connection parameters and exit");
    
    driver = G_define_option() ;
    driver->key        = "driver" ;
    driver->type       = TYPE_STRING ;
    driver->options    = db_list_drivers();
    driver->required   = NO  ;
    driver->multiple   = NO ;
    driver->description= _("driver name:") ;
    driver->answer = db_get_default_driver_name();

    database = G_define_option() ;
    database->key        = "database" ;
    database->type       = TYPE_STRING ;
    database->required   = NO  ;
    database->multiple   = NO ;
    database->description= _("Database name:") ;
    database->answer = db_get_default_database_name();

    schema = G_define_option() ;
    schema->key        = "schema" ;
    schema->type       = TYPE_STRING ;
    schema->required   = NO  ;
    schema->multiple   = NO ;
    schema->answer     = db_get_default_schema_name();
    schema->description = _("Database schema. Don't use this option if schemas are not supported "
                          "by driver/database server.");
    
    group = G_define_option() ;
    group->key        = "group" ;
    group->type       = TYPE_STRING ;
    group->required   = NO  ;
    group->multiple   = NO ;
    group->answer     = db_get_default_group_name();
    group->description = _("Default group of database users to which select privilege is granted.");

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
    module->description = _(""\
    "Connect to the database through DBMI.");


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

	if ( group->answer )
	    conn.group = group->answer;

	db_set_connection( &conn );    
	
    }
    else{
	/* get and print connection */
	if (db_get_connection( &conn ) == DB_OK){
	    G_message( _("driver:%s\n"), conn.driverName);
	    G_message( _("database:%s\n"), conn.databaseName);    
	    G_message( _("schema:%s\n"), conn.schemaName);    
	    G_message( _("group:%s\n"), conn.group);    
	}
	else
	    G_fatal_error(_("No db connection settings defined. Set with db.connect"));
    }

    return 0;
}

