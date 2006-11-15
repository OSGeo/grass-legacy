#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>

int
main(int argc, char *argv[])
{
    struct Option *driver, *database, *user, *password;
    struct GModule *module;
    char answer[200];
    
    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module              = G_define_module();
    module->keywords    = _("database, SQL");
    module->description = _("Set user/password for driver/database");
    
    driver = G_define_option() ;
    driver->key        = "driver" ;
    driver->type       = TYPE_STRING ;
    driver->options    = db_list_drivers();
    driver->required   = YES;
    driver->multiple   = NO ;
    driver->description= _("Driver name") ;
    driver->answer     = db_get_default_driver_name();

    database = G_define_option() ;
    database->key        = "database" ;
    database->type       = TYPE_STRING ;
    database->required   = YES ;
    database->multiple   = NO ;
    database->description= _("Database name") ;
    database->answer     = db_get_default_database_name();

    user = G_define_option() ;
    user->key        = "user" ;
    user->type       = TYPE_STRING ;
    user->required   = NO  ;
    user->multiple   = NO ;
    user->description= _("User") ;

    password = G_define_option() ;
    password->key        = "password" ;
    password->type       = TYPE_STRING ;
    password->required   = NO  ;
    password->multiple   = NO ;
    password->description= _("Password") ;

    if(G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /* set connection */
    if (!password->answer && isatty(fileno(stdin)) ){
        for(;;) {
          do {
              fprintf (stderr,_("\nEnter database password for connection <%s:%s:user=%s>\n"), driver->answer, database->answer, user->answer);
              fprintf (stderr, _("Hit RETURN to cancel request\n"));
              fprintf (stderr,">");
          } while(!G_gets(answer));
          G_strip(answer);
          if(strlen(answer)==0) {
	     G_message(_("Exiting. Not changing current settings"));
	     return -1;
	  } else {
	     G_message(_("New password set."));
	     password->answer = G_store(answer);
	     break;
	  }
	}
    }
    if (  db_set_login ( driver->answer, database->answer, user->answer, password->answer ) == DB_FAILED ) {
	G_fatal_error ( _("Cannot set user/password") );
    }

    if ( password->answer )
        G_warning ( _("The password was stored in file.") );
	
    exit(EXIT_SUCCESS);
}

