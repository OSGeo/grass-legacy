/* based on r.timestamp by Michael Shapiro and v.timestamp by Markus Neteler:
 * 
 * r3.timestamp by Michael Pelizzari 3/2001
 *                 michael.pelizzari@lmco.com
 *                 Lockheed Martin Space Systems
 *
 * Stamps grid3 files with date and time.  This main.c is linked to functions 
 * currently residing in src/libes/gis/timestamp.c
 *
 */

#include "gis.h"

int main (int argc, char *argv[])
{
    struct Option *map, *date;
    struct TimeStamp ts;
    char *name;
    char *mapset;
    int modify;

    G_gisinit (argv[0]);

    map = G_define_option();
    map->key = "grid3";
    map->required = YES;
    map->type = TYPE_STRING;
    map->description = "input grid3 filename";

    date = G_define_option();
    date->key = "date";
    date->key_desc = "timestamp";
    date->required = NO;
    date->type = TYPE_STRING;
    date->description = "datetime, datetime1/datetime2, or none";

    if (G_parser(argc,argv) < 0)
	exit(-1);

    name = map->answer;

    modify = date->answer != NULL;

    if (modify)
	mapset = G_find_grid3(name,G_mapset());
    else
	mapset = G_find_grid3(name,"");

    if(mapset == NULL)
    {
	fprintf (stderr, "grid3 <%s> not found %s\n", name,
	    modify ? "in current mapset" : "");
	exit(1);
    }

    if (!modify)
    {
	if (G_read_grid3_timestamp(name, mapset, &ts) == 1)
	{
	    G__write_timestamp(stdout, &ts);
	    exit(0);
	}
	else
	    exit(1);
    }
    if (strcmp(date->answer,"none") == 0)
    {
	G_remove_grid3_timestamp(name);
	exit(0);
    }

    G_scan_timestamp (&ts, date->answer);
    G_write_grid3_timestamp(name, &ts);
    exit(0);
}
