/***************************************************************************
 * $Id$
 *
 * MODULE: 	i.grey.scale
 * AUTHOR(S):	Michael Shapiro, CERL
 *              Andreas Lange - <andreas.lange@rhein-main.de>
 * PURPOSE: 	Assign histogram contrast stretch grey scale color
 *              table to a raster map layer.
 * 
 * COPYRIGHT:  	(C) 2000 by the GRASS Development Team
 *
 *   	    	This program is free software under the GPL (>=v2)
 *   	    	Read the file COPYING that comes with GRASS for details.
 ****************************************************************************
 * $Log$
 * Revision 1.2  2000-11-06 19:44:54  andreas
 * made program command line (with G_parser) interface
 *
 */

#include <string.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    struct { struct Option *name; } parm;
    
    char *name, *mapset;
    
    parm.name = G_define_option();
    parm.name->key = "map";
/*    parm.name->key_desc = "";*/
    parm.name->type = TYPE_STRING;
    parm.name->required = YES;
    parm.name->description = "Raster map layer to assign a histogram contrast stretch grey scale color table to";
    parm.name->gisprompt = "old,cell,raster";
    
    G_gisinit (argv[0]);
    
    if (G_parser(argc, argv))
      exit(-1);
      
    name = parm.name->answer;  
    
    if (name == NULL)
  	G_fatal_error("no raster map");
  	
    mapset = G_find_cell2(name, "");
    if (mapset == NULL)
    	G_fatal_error("mapset not found");
    grey_scale(name, mapset);  
    
    /*
    {
	for (i = 1; i < argc; i++)
	{
	    strcpy (name, argv[i]);
	    if (mapset = G_find_cell2 (name, ""))
		grey_scale (name, mapset);
	    else
		fprintf (stderr, "%s not found\n", name);
	}
    }
    else
    {
	while(mapset = G_ask_cell_old ("which layer needs a grey scale?",name))
	    grey_scale (name, mapset);
    }
*/
    return 0;
}
