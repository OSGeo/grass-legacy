#include <stdlib.h>
#include "dbmi.h"
#include "gis.h"
#include "codes.h"
#include "glocale.h"

void parse_command_line();

struct {
	char *driver;
	char *location;
} parms;


int
main(int argc, char *argv[])
{
    dbDriver *driver;
    dbHandle *handles;
    dbString locations;
    int nlocs = 0;
    int count, i;

    db_init_string ( &locations );
    parse_command_line(argc, argv);

    if ( parms.location ) {
        db_set_string ( &locations, parms.location );
	nlocs = 1;
    }
    
    driver = db_start_driver (parms.driver);
    if (driver == NULL)
        G_fatal_error(_("Cannot start driver '%s'."), parms.driver);
        
    if(db_list_databases (driver, &locations, nlocs, &handles, &count) != DB_OK)
	G_fatal_error(_("Cannot list databases."));

    db_shutdown_driver (driver);

    for (i = 0; i < count; i++) {
	fprintf(stdout, "%s", db_get_handle_dbname(&handles[i]));
	fprintf(stdout, "\n");
    }
    exit(OK);
}

void
parse_command_line(int argc, char *argv[])
{
    struct Option *driver, *location;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->options     = db_list_drivers();
    driver->required 	= NO;               /* changed yo NO by RB, 4/2000 */
    driver->description = _("driver name");

    location 		  = G_define_option();
    location->key 	  = "location";
    location->type 	  = TYPE_STRING;
    location->required 	  = NO;
    location->multiple 	  = YES;
    location->description = _("database location");

    
    /* Set description */
    module              = G_define_module();
    module->description = _("List all databases for a given driver and location.");

    if(G_parser(argc, argv))
        exit(ERROR);

    parms.driver     = driver->answer;
    parms.location   = location->answer;
}
