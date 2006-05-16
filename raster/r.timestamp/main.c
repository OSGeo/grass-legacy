#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>

int main (int argc, char *argv[])
{
    struct GModule *module;
    struct Option *map, *date;
    struct TimeStamp ts;
    char *name;
    char *mapset;
    int modify;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
		_("Print/add/remove a timestamp for a raster map.");
				        
    map = G_define_standard_option(G_OPT_R_MAP);

    date = G_define_option();
    date->key = "date";
    date->key_desc = "timestamp";
    date->required = NO;
    date->type = TYPE_STRING;
    date->description = _("Datetime, datetime1/datetime2, or none");

    if (G_parser(argc,argv))
	exit(EXIT_FAILURE);

    name = map->answer;

    modify = date->answer != NULL;

    if (modify)
	mapset = G_find_cell(name,G_mapset());
    else
	mapset = G_find_cell(name,"");
    if(mapset == NULL)
	G_fatal_error ( "Map <%s> not found %s", name,
	    modify ? "in current mapset" : "");

    if (!modify)
    {
	if (G_read_raster_timestamp(name, mapset, &ts) == 1)
	{
	    G__write_timestamp(stdout, &ts);
	    exit(EXIT_SUCCESS);
	}
	else
	    exit(EXIT_FAILURE);
    }
    if (strcmp(date->answer,"none") == 0)
    {
	G_remove_raster_timestamp(name);
	exit(EXIT_SUCCESS);
    }
    
    if(1 == G_scan_timestamp (&ts, date->answer))
    {
	G_write_raster_timestamp(name, &ts);
	exit(EXIT_SUCCESS);
    }
    else
 	G_fatal_error(_("Invalid timestamp"));

    exit(EXIT_SUCCESS);
}
