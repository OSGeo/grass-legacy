/* based on r.timestamp by Michael Shapiro and v.timestamp by Markus Neteler:
 * 
 * r3.timestamp by Michael Pelizzari 3/2001
 *                 michael.pelizzari@lmco.com
 *                 Lockheed Martin Space Systems
 *
 * Stamps grid3 files with date and time.  This main.c is linked to functions 
 * currently residing in lib/gis/timestamp.c
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/G3d.h>
#include <grass/glocale.h>

int main(int argc, char *argv[])
{
    struct Option *map, *date;
    struct TimeStamp ts;
    char *name;
    char *mapset;
    int modify;
    struct GModule *module;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = _("print/add/remove a timestamp for a 3D raster map");

    map = G_define_option();
    map->key = "map";
    map->required = YES;
    map->type = TYPE_STRING;
    map->gisprompt = "old,grid3,3d raster";
    map->description = _("input grid3 filename");

    date = G_define_option();
    date->key = "date";
    date->key_desc = "timestamp";
    date->required = NO;
    date->type = TYPE_STRING;
    date->description = _("datetime, datetime1/datetime2, or none");

    if (G_parser(argc, argv) < 0)
	exit(EXIT_FAILURE);

    name = map->answer;

    modify = date->answer != NULL;

    if (modify)
	mapset = G_find_grid3(name, G_mapset());
    else
	mapset = G_find_grid3(name, "");

    if (mapset == NULL) {
	G_fatal_error(_("grid3 <%s> not found %s\n"), name,
		      modify ? "in current mapset" : "");
	exit(EXIT_FAILURE);
    }

    if (!modify) {
	if (G_read_grid3_timestamp(name, mapset, &ts) == 1) {
	    G__write_timestamp(stdout, &ts);
	    exit(EXIT_SUCCESS);
	}
	else
	    exit(EXIT_FAILURE);
    }
    if (strcmp(date->answer, "none") == 0) {
	G_remove_grid3_timestamp(name);
	exit(EXIT_SUCCESS);
    }

    G_scan_timestamp(&ts, date->answer);
    G_write_grid3_timestamp(name, &ts);
    exit(EXIT_SUCCESS);
}
