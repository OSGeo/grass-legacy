#include "dbmi.h"
#include "gis.h"
#include "codes.h"
#include <stdlib.h>

void parse_command_line();

struct {
	char *driver;
} parms;


int
main(int argc, char *argv[])
{
    dbDriver *driver;
    dbHandle *handles;
    int nlocs;
    int count, i;

    parse_command_line(argc, argv);

    driver = db_start_driver (parms.driver);
    if (driver == NULL)
    {
	fprintf (stderr, "Can't run driver %s\n", parms.driver);
	exit(ERROR);
    }
    if(db_list_databases (driver, NULL, nlocs, &handles, &count) != DB_OK)
	exit(ERROR);
    db_shutdown_driver (driver);

    for (i = 0; i < count; i++)
    {
	fprintf (stdout,"%s", db_get_handle_dbname(&handles[i]));
	fprintf (stdout,"\n");
    }
    exit(OK);
}

void
parse_command_line(int argc, char *argv[])
{
    struct Option *driver;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->options     = db_list_drivers();
    driver->required 	= NO;               /* changed yo NO by RB, 4/2000 */
    driver->description = "driver name";

    /* Set description */
    module              = G_define_module();
    module->description = ""\
    "List all databases for a given driver.";

    if(G_parser(argc, argv))
            exit(ERROR);

    parms.driver     = driver->answer;
}
