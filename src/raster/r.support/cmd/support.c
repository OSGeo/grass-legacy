/*
* $Id$
*
****************************************************************************
*
* MODULE:       r.support (GRASS core)
* AUTHOR(S):    Original Michael  Shapiro - CERL
*               Preliminary parser support by Markus Neteler
* PURPOSE:      Build support file for raster map
*               - Edit the header
*               - Update the stats (histogram,range)
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public 
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    char *name;
    char *inmapset;
    char rname[256], rmapset[256];
    struct Cell_head cellhd;
    int row, col, null_fd;
    char element[300], path[400];
    unsigned char *null_bits;
    char buf[1024];
    int cellhd_ok;
    int is_reclass;
    int error();
    char question[100];
    struct GModule *module;
    struct Option *map;
    struct Flag *rangeflag;
    
    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
		"Allows the user to create and/or modify raster map layer "
		"support files. Note: Interactive mode offers more functionality than "
		"command line mode.";

    map = G_define_option();            
    map->key = "map";
    map->required = YES;
    map->type = TYPE_STRING;
    map->gisprompt = "old,cell,raster";
    map->description = "raster map name";

    rangeflag = G_define_flag() ;
    rangeflag->key         = 'r';
    rangeflag->description = "Update the stats (histogram, range)";

    if (G_parser(argc,argv))
            exit(1);

    name = map->answer;

    inmapset = G_find_cell2 (name, "");
    if (inmapset == NULL)
    {
     fprintf(stderr,"%s: raster map not found\n", name);
     exit(1);
    }
                                

/* cell header */
    cellhd_ok = G_get_cellhd (name, G_mapset(), &cellhd) >= 0 ;
    is_reclass = (G_is_reclass (name, G_mapset(), rname, rmapset) > 0);

/* edit the header in ../inter/ */

/* check the histogram and range */
  check_stats (name, rangeflag->answer);

/* Edit the category file in ../inter/ */
   
/* color table in ../inter/ */
  
/* Edit the history file in ../inter/ */
  
/* null file in ../inter/ */
   /* This might be added here, too (12/2000) */
  
   
  exit(0);
}

int G_clear_screen (void){return 0;}
