#include "gis.h"

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
		"Print/add/remove a timestamp for a raster map.";
				        
    map = G_define_option();
    map->key = "map";
    map->required = YES;
    map->type = TYPE_STRING;
    map->gisprompt = "old,cell,raster";
    map->description = "raster map name";

    date = G_define_option();
    date->key = "date";
    date->key_desc = "timestamp";
    date->required = NO;
    date->type = TYPE_STRING;
    date->description = "datetime, datetime1/datetime2, or none";

    if (G_parser(argc,argv))
	exit(0);

    name = map->answer;

    modify = date->answer != NULL;

    if (modify)
	mapset = G_find_cell(name,G_mapset());
    else
	mapset = G_find_cell(name,"");
    if(mapset == NULL)
    {
	fprintf (stderr, "map <%s> not found %s\n", name,
	    modify ? "in current mapset" : "");
	exit(1);
    }

    if (!modify)
    {
	if (G_read_raster_timestamp(name, mapset, &ts) == 1)
	{
	    G__write_timestamp(stdout, &ts);
	    exit(0);
	}
	else
	    exit(1);
    }
    if (strcmp(date->answer,"none") == 0)
    {
	G_remove_raster_timestamp(name);
	exit(0);
    }

    G_scan_timestamp (&ts, date->answer);
    G_write_raster_timestamp(name, &ts);
    exit(0);
}
