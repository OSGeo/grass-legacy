/********************************************************************/
/*                                                                  */
/* v.to.db - load values from vector to database                    */
/*                                                                  */
/* Radim Blazek, 6/2000                                             */
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
    struct Map_info Map;
    struct GModule *module;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = "Load values from vector to database. In uploaded/printed category values "
			"'-1' is used for 'no category' and 'null'/'-' if category cannot be found or "
                        "multiple categories were found.";

    parse_command_line (argc, argv);

    G_begin_distance_calculations();
    G_begin_polygon_area_calculations();

    /* open map */
    Vect_set_open_level (2);
    Vect_open_old(&Map,options.name,options.mapset);

    /* allocate array for values (number of cats may not be greater than number of lines ) */
    /* (+ 1 is for cat -1 (no category) reported at the end ) */
    Values = (VALUE *) G_calloc ( Vect_get_num_lines( &Map ) + 1, sizeof ( VALUE ) );
    vstat.rcat = 0;

    /* Read values from map */
    if ( options.option == O_QUERY ){
	query(&Map);
    } else if ( options.option == O_AREA ){
	read_areas(&Map);
    } else { 
        read_lines(&Map); 
    }		

    conv_units();
    
    if ( options.print ) {
	report();
    } else {
	update( &Map );
	Vect_set_db_updated ( &Map );
    }
    
    Vect_close (&Map);

    /* free list */
    G_free ( Values );

    print_stat();

    exit(0);
}
