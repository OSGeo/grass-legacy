/*
 * $Id$
 */

#include "dbmi.h"
#include "gis.h"
#include "codes.h"

void parse_command_line();
void get_locations();

struct {
	char *driver;
	char **locations;
	int l;
} parms;

main(argc, argv) char *argv[];
{
    dbDriver *driver;
    dbHandle *handles;
    dbString *locations;
    int nlocs;
    int count, i;

    parse_command_line(argc, argv);

    driver = db_start_driver (parms.driver);
    if (driver == NULL)
    {
	fprintf (stderr, "Can't run driver %s\n", parms.driver);
	exit(ERROR);
    }
    get_locations (&locations, &nlocs);
    if(db_list_databases (driver, locations, nlocs, &handles, &count) != DB_OK)
	exit(ERROR);
    db_shutdown_driver (driver);

    for (i = 0; i < count; i++)
    {
	fprintf (stdout,"%s", db_get_handle_dbname(&handles[i]));
	if(parms.l)
	    fprintf (stdout," %s", db_get_handle_dbpath(&handles[i]));
	fprintf (stdout,"\n");
    }
    exit(OK);
}

void
parse_command_line(argc, argv) char *argv[];
{
    struct Option *driver, *location;
    struct Flag *l;

    driver 		= G_define_option();
    driver->key 	= "driver";
    driver->type 	= TYPE_STRING;
    driver->required 	= NO;               /* changed yo NO by RB, 4/2000 */
    driver->description = "driver name";

    location 		= G_define_option();
    location->key 	= "location";
    location->type 	= TYPE_STRING;
    location->required 	= NO;
    location->multiple 	= YES;
    location->description= "database location(s)";

    l 			= G_define_flag();
    l->key 		= 'l';
    l->description	= "output database location also";
    
    G_disable_interactive();

    if (argc > 1) {
	if (G_parser(argc, argv)) exit(ERROR);
    }

    parms.driver     = driver->answer;
    parms.l          = l->answer;
    parms.locations  = location->answers; /* NOTE THE PLURAL 'answers' HERE */
}

void
get_locations (locations, nlocs)
    dbString **locations;
    int *nlocs;
{
    int i,j, count;
    char *p, *colon;
    dbString *list;


/* count the locations. Note each 'locations' may itself be a ':' separated list */

    *locations = NULL;
    *nlocs = 0;
    if (parms.locations == NULL)
	return;

    count = 0;
    for (i = 0; p = parms.locations[i]; i++)
    {
	while (*p)
	{
	    if (*p != ':')
		count++;
	    if (colon = G_index (p, ':'))
		p = colon + 1;
	    else
		break;
	}
    }
    *locations = list = db_alloc_string_array (*nlocs = count);

    for (j = 0, i = 0; p = parms.locations[i]; i++)
    {
	while (*p)
	{
	    if (colon = G_index(p, ':'))
		*colon = 0;
	    if (*p)
	    {
		if (j == count)
		{
		    db_error ("OOPS - programmer goofed");
		    exit(ERROR);
		}
		db_set_string (&list[j++], p);
	    }
	    if (colon == NULL)
		break;
	    p = colon + 1;
	}
    }
}
