#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "glocale.h"

int
main(int argc, char *argv[])
{
    struct Option *driver, *database, *user, *password;
    struct GModule *module;
    
    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module              = G_define_module();
    module->description = "Set user/password for driver/database.";
    
    driver = G_define_option() ;
    driver->key        = "driver" ;
    driver->type       = TYPE_STRING ;
    driver->options    = db_list_drivers();
    driver->required   = YES;
    driver->multiple   = NO ;
    driver->description= "Driver name" ;
    driver->answer     = db_get_default_driver_name();

    database = G_define_option() ;
    database->key        = "database" ;
    database->type       = TYPE_STRING ;
    database->required   = YES ;
    database->multiple   = NO ;
    database->description= "Database name" ;
    database->answer     = db_get_default_database_name();

    user = G_define_option() ;
    user->key        = "user" ;
    user->type       = TYPE_STRING ;
    user->required   = NO  ;
    user->multiple   = NO ;
    user->description= "User" ;    

    password = G_define_option() ;
    password->key        = "password" ;
    password->type       = TYPE_STRING ;
    password->required   = NO  ;
    password->multiple   = NO ;
    password->description= "Password" ;

    if(G_parser(argc, argv))
	exit(1);

    /* set connection */
    if (  db_set_login ( driver->answer, database->answer, user->answer, password->answer ) == DB_FAILED ) {
	G_fatal_error ( _("Cannot set user/password") );
    }

    if ( password->answer )
        G_warning ( "The password was stored in file." );
	
    exit(0);
}

