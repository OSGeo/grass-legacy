/********************************************************************/
/*                                                                  */
/* v.to.db - load values from vector to database                    */
/*                                                                  */
/* Radim Blazek, Radim.Blazek@dhv.cz, 6/2000                        */
/*                                                                  */
/* This file is part of GRASS GIS. It is free software. You can     */
/* redistribute it and/or modify it under the terms of              */ 
/* the GNU General Public License as published by the Free Software */
/* Foundation; either version 2 of the License, or (at your option) */
/* any later version.                                               */
/*                                                                  */  
/********************************************************************/

#define MAIN
#include "global.h"
#include "Vect.h"

int 
main (int argc, char *argv[])
{
    int    i,ret;
    struct Map_info Map;
    struct Categories Labels;
	struct GModule *module;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Load values from vector to database.";

    parse_command_line (argc, argv);

    G_begin_distance_calculations();
    G_begin_polygon_area_calculations();

    vstat.maxerror = 10;

    /* open labels */
    if ( options.option == O_LABEL )
	if ( G_read_vector_cats (options.name, options.mapset, &Labels) < 0 )
	    exit (-1);

    /* open map */
    if ((ret = Vect_open_old(&Map,options.name,options.mapset)) < 0) {
	fprintf(stderr,"Error reading map file.") ;
	exit(-1);
    }  else  if (ret < 2) {
	fprintf(stderr,"v.support must be run on this map\n");
	exit(-1) ;
    } 

    /* allocate list */
    vstat.cat = vstat.alloc = Map.n_atts;
    vstat.rcat = 0;
    vstat.sort = 0;    
    alloc_list();

    /* read points */
    if ( options.type & DOT )
	if ( read_points(&Map, &Labels) < 0) { 
	    fprintf(stderr,"Error reading points.\n") ;
	    exit(-1);
	}
    /* read lines */
    if ( options.type & LINE )
	if ( read_lines(&Map, &Labels) < 0) { 
	    fprintf(stderr,"Error reading lines.\n") ;
	    exit(-1);
	}		
    /* read areas */
    if ( options.type & AREA )
	if ( read_areas(&Map, &Labels) < 0) { 
	    fprintf(stderr,"Error reading areas.\n") ;
	    exit(-1);
    }
         
    Vect_close (&Map);

    conv_units();
    
    if ( options.print ) report();
    else update();

    /* free list */
    free_list();

    print_stat();

    exit(0);
}
