/* based on r.timestamp by Michael Shapiro
 * 
 *      Copyright (C) 2000 by the GRASS Development Team
 *      Author:  Markus Neteler <neteler@geog.uni-hannover.de>
 *               Institute of Physical Geography and Landscape Ecology
 *               University of Hannover, Germany
 *
 *      This program is free software under the GPL (>=v2)
 *      Read the file COPYING coming with GRASS for details.
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"

int main (int argc, char *argv[])
{
    struct Option *map, *date;
    struct Map_info Map;
    struct TimeStamp ts;
    char *name;
    char *mapset;
    int modify;
    int level;
    char err_msg[1000];
    
    G_gisinit (argv[0]);

    map = G_define_option();
    map->key = "vect";
    map->required = YES;
    map->type = TYPE_STRING;
    map->description = "input vector filename";

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

    if (NULL == (mapset = G_find_file2 ("dig", name, G_mapset())))
        {
           sprintf (err_msg, "Could not find file '%s'", name);
           G_fatal_error (err_msg);
         }

    if (strcmp (mapset, G_mapset()))
           G_fatal_error ("File must be in current mapset");

    level = Vect_open_old (&Map, name, mapset);
    
    if (level < 1)
           G_fatal_error ("File open failed");

    if (level < 2)
    {
       fprintf (stdout,"\n");
       fprintf (stdout,"v.support has not been run.  \n");
       fprintf (stdout,"\n");
       exit (-1);
    }
                 
    if(mapset == NULL)
    {
	fprintf (stderr, "map <%s> not found %s\n", name,
	    modify ? "in current mapset" : "");
	exit(1);
    }

    if (!modify)
    {
	if (G_read_vector_timestamp(name, mapset, &ts) == 1)
	{
	    G__write_timestamp(stdout, &ts);
	    exit(0);
	}
	else
	    exit(1);
    }
    if (strcmp(date->answer,"none") == 0)
    {
	G_remove_vector_timestamp(name);
	exit(0);
    }

    G_scan_timestamp (&ts, date->answer);
    G_write_vector_timestamp(name, &ts);
    Vect_close (&Map);
        
    exit(0);
}
