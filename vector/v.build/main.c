/* ***************************************************************
 * *
 * * MODULE:       v.build
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Build topology
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/

#include "gis.h"
#include "Vect.h"

int 
main (int argc, char *argv[])
{
    struct GModule *module;
    struct Option *map_opt, *opt;
    struct Map_info Map;
    int    i, build = 0, dump = 0, sdump = 0;
    
    map_opt = G_define_standard_option(G_OPT_V_MAP);
    
    opt = G_define_option();
    opt->key = "option";
    opt->type =  TYPE_STRING;
    opt->options = "build,dump,sdump";
    opt->required = NO;
    opt->multiple = YES;
    opt->answer = "build";
    opt->description  = "Build topology or dump topology or spatial index to stdout";
    
    G_gisinit(argv[0]);
    if (G_parser (argc, argv))
	exit(-1); 
   
    module = G_define_module(); 
    module->description = "Creates topology for GRASS vector data.";
  
    i = 0; 
    while (opt->answers[i]) {
	if ( *opt->answers[i] == 'b')  build = 1;
        else if ( *opt->answers[i] == 'd')  dump = 1;
        else if ( *opt->answers[i] == 's')  sdump = 1;

	i++;
    }
    
    /* build topology */
    if ( build ) { 
	/* open input vector */
	if ( G_find_vector2 (map_opt->answer, G_mapset()) == NULL)
	     G_fatal_error ("Could not find input %s\n", map_opt->answer);
	
	Vect_set_open_level (1); 
	Vect_open_old (&Map, map_opt->answer, G_mapset()); 

	Vect_build ( &Map, stdout );
        
    }
    /* dump topology */
    if (dump || sdump) {
        if ( !build ) { 
	    Vect_set_open_level (2);
	    Vect_open_old (&Map, map_opt->answer, G_mapset()); 
        }
        if (dump)
	    Vect_topo_dump ( &(Map.plus), stdout );

        if (sdump)
	    Vect_spatial_index_dump ( &(Map.plus), stdout );
    }

    Vect_close (&Map);
    
    exit(0) ;
}


