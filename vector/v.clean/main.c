/* ***************************************************************
 * *
 * * MODULE:       v.clean
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Clean lines
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <stdlib.h> 
#include <string.h> 
#include "gis.h"
#include "Vect.h"

#define TOOL_BREAK    1 /* break at intersection */
#define TOOL_RMDUPL   2 /* remove duplicates */
#define TOOL_RMDANGLE 3 /* remove dangles */ 
#define TOOL_RTDANGLE 4 /* retype 'boundary' dangles to 'line' */ 

int break_lines ( struct Map_info *Out, int otype, int x_flag );
int rmdupl ( struct Map_info *Out, int otype );

int 
main (int argc, char *argv[])
{
	struct Map_info In, Out;
        int    i, otype, with_z;
	char   *mapset, errmsg[200];
	struct GModule *module;
	struct Option *in_opt, *out_opt, *type_opt, *tool_opt;
	struct Flag   *x_flag; 
	int tools[100], ntools;

	module = G_define_module();
	module->description = "Break lines at intersections.";

	in_opt = G_define_standard_option(G_OPT_V_INPUT);
	out_opt = G_define_standard_option(G_OPT_V_OUTPUT);
	type_opt = G_define_standard_option(G_OPT_V_TYPE) ;
	
	tool_opt = G_define_option();
	tool_opt->key = "tool";
	tool_opt->type =  TYPE_STRING;
	tool_opt->required = YES;
	tool_opt->multiple = YES;
	tool_opt->options = "break,rmdupl";
        tool_opt->description = "Action to be done:\n"
	                        "\t\tbreak - break lines at each intersection\n"
			        "\t\trmdupl - remove duplicate lines (pay attention to categories!)";
	
	x_flag = G_define_flag ();
	x_flag->key             = 'x';
	x_flag->description     = "Write out intersection points instead of broken lines";
	
	G_gisinit(argv[0]);
        if (G_parser (argc, argv))
	    exit(-1); 
	
	i = 0;
	while (type_opt->answers[i]) {
	    switch ( type_opt->answers[i][0] ) {
	        case 'p':
	            otype |= GV_POINT;
		    break;
	        case 'l':
	            otype |= GV_LINE;
		    break;
	        case 'b':
	            otype |= GV_BOUNDARY;
		    break;
	        case 'c':
	            otype |= GV_CENTROID;
		    break;
	    }
	    i++;
	}
	
	ntools = 0; i = 0;
	while (tool_opt->answers[i]) {
	    G_debug ( 1, "tool : %s", tool_opt->answers[i] );
	    if ( ntools >= 100 ) {
		G_warning ("Too many tools (some ignored)");
		break;
	    }
	    if ( strcmp ( tool_opt->answers[i], "break" ) == 0 )
		tools[ntools] = TOOL_BREAK;
	    else if ( strcmp ( tool_opt->answers[i], "rmdupl" ) == 0 )
		tools[ntools] = TOOL_RMDUPL;
	    else 
		G_fatal_error ( "Tool doesn't exist" );

	    ntools++; i++;
	}
	
	G_debug ( 1, "ntools = %d", ntools );
		    
	/* open input vector */
        if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
	     sprintf (errmsg, "Could not find input %s\n", in_opt->answer);
	     G_fatal_error (errmsg);
	}
	
        Vect_set_open_level (2); 
	Vect_open_old (&In, in_opt->answer, mapset); 

	with_z = Vect_is_3d (&In);
    
	Vect_set_fatal_error (GV_FATAL_PRINT);
	if (0 > Vect_open_new (&Out, out_opt->answer, with_z)) {
	     Vect_close (&In);
	     exit (1);
	}

	/* Copy input to output */
	Vect_copy_head_data (&In, &Out);
	Vect_copy_map_lines ( &In, &Out );
	Vect_build ( &Out, NULL );
	Vect_close (&In);
	Vect_close (&Out);

	Vect_open_update (&Out, out_opt->answer, G_mapset()); 

	for ( i = 0; i < ntools ; i++ ) { 
	    switch ( tools[i] ) {
		case TOOL_BREAK:
		    fprintf (stderr, "Tool: Break lines at intersections\n" );
		    fflush ( stderr );
                    break_lines ( &Out, otype, (int) x_flag->answer );
		    break;
		case TOOL_RMDUPL:
		    fprintf (stderr, "Tool: Remove duplicates\n" );
		    fflush ( stderr );
                    rmdupl ( &Out, otype );
		    break;
	    }
	}

	Vect_build (&Out, stdout);
	Vect_close (&Out);

	exit(0) ;
}


