#include <stdio.h>
#include "gis.h"
#include "dbmi.h"
#include "codes.h"

int main(argc, argv)
int argc ;
char **argv ;
{
    dbConnection conn;
    struct Flag *print;
    struct Option *driver, *database, *location, *user, *password, *keycol;
    
    print = G_define_flag();
    print->key               = 'p';
    print->description       = "Use -p flag to print only current connection parameters";    
    
    driver = G_define_option() ;
    driver->key        = "driver" ;
    driver->type       = TYPE_STRING ;
    driver->required   = NO  ;
    driver->multiple   = NO ;
    driver->description= "DBMI driver name:" ;
     
    database = G_define_option() ;
    database->key        = "database" ;
    database->type       = TYPE_STRING ;
    database->required   = NO  ;
    database->multiple   = NO ;
    database->description= "Database name:" ;

    location = G_define_option() ;
    location->key        = "location" ;
    location->type       = TYPE_STRING ;
    location->required   = NO  ;
    location->multiple   = NO ;
    location->description= "Database location:" ;
    
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

    keycol = G_define_option() ;
    keycol->key        = "key" ;
    keycol->type       = TYPE_STRING ;
    keycol->required   = NO  ;
    keycol->multiple   = NO ;
    keycol->description= "Key column:" ;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    /* Check command line */
    if (G_parser(argc, argv))
	exit(-1);

    /* set connection*/
    if( !print->answer) 
    {
        db_get_connection( &conn );  /* read current */
	
	if ( driver->answer )
	    conn.driverName = driver->answer;
	    
	if ( database->answer )
	    conn.databaseName = database->answer;

	if ( location->answer )
	    conn.location = location->answer;

	if ( user->answer )
	    conn.user = user->answer;

	if ( password->answer )
	    conn.password = password->answer;

	if ( keycol->answer )
	    conn.keycol = keycol->answer;

	db_set_connection( &conn );    
	
	if ( (conn.password != NULL) && ( strlen (conn.password) > 0))
	    fprintf(stdout, "Warning: Your password was written to %s in readable form!!!\n", getenv ("GISRC"));    

    }
    
    /* get and print connection */
    db_get_connection( &conn );    
    
    fprintf(stdout, "driver:%s\n", conn.driverName);
    fprintf(stdout, "database:%s\n", conn.databaseName);    
    fprintf(stdout, "location:%s\n", conn.location);
    fprintf(stdout, "user:%s\n", conn.user);
    fprintf(stdout, "password:%s\n", conn.password);    
    fprintf(stdout, "key:%s\n", conn.keycol);    

    exit(OK);
}

