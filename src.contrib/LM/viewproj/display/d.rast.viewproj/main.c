/*
****************************************************************************
*
* MODULE:       d.rast.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To display a raster map in a map projection.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*************************************************************************
d.rast.viewproj

This is a modified version of d.rast (Grass 4.1).
It displays raster maps in various map projections
which are calculated on-the-fly using PROJ.

Projection infomation must be set (via d.set.viewproj and d.mon.viewproj) 
before calling d.rast.viewproj.

Sharif Razzaque June 1995
*************************************************************************
*/

#include <stdlib.h>

#include "config.h"	/* For Grass 5.0 - Bev Wallace */
#include "gis.h"
#include "display.h"	/* For D_* - Bev Wallace */
#include "raster.h"	/* For R_* - Bev Wallace */

extern int Dcell_viewproj (char *name, char *mapset, int overlay);


int main (int argc, char **argv)
{
    char *mapset ;
    char *name ;
    int overlay;
    struct Option *map;
    struct Flag *flag_o;


/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;


/* set up command line */
    map              = G_define_option();
    map->key         = "map";
    map->type        = TYPE_STRING;
    map->required    = YES;
    map->gisprompt   = "old,cell,raster" ;
    map->description = "Raster map to be displayed";

    flag_o = G_define_flag();
    flag_o->key = 'o';
    flag_o->description = "Overlay (non-zero values only)";

    if (G_parser(argc, argv))
	exit(1);

    name = map->answer;
    overlay = flag_o->answer;


/* Make sure map is available */
    mapset = G_find_cell2 (name, "") ;
    if (mapset == NULL)
    {
		char buf[256];
        sprintf(buf,"Raster map [%s] not available", name);
        G_fatal_error(buf) ;
    }

    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");

    Dcell_viewproj(name, mapset, overlay ) ;

    R_close_driver();

    exit(0);
}
