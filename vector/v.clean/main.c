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
#include "proto.h"

int break_lines ( struct Map_info *Out, int otype, int x_flag );
int rmdupl ( struct Map_info *Out, int otype );
int rmdac ( struct Map_info *Out );
int svtl ( struct Map_info *Out, int otype, int tool, double thresh, int x_flag );
int break_polygons ( struct Map_info *Out, int otype, int x_flag );

int 
main (int argc, char *argv[])
{
	struct Map_info In, Out;
        int    i, otype, with_z;
	char   *mapset;
	struct GModule *module;
	struct Option *in_opt, *out_opt, *type_opt, *tool_opt, *thresh_opt;
	struct Flag   *x_flag; 
	int    *tools, ntools, atools;
	double *threshs;

	G_gisinit(argv[0]);

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
	tool_opt->options = "break,rmdupl,svtlx,rmdac,bpol";
        tool_opt->description = "Action to be done:\n"
	                        "\t\tbreak - break lines at each intersection\n"
			        "\t\trmdupl - remove duplicate lines (pay attention to categories!)\n"
			        "\t\tsvtlx - snap vertex to a line and create new vertex at that line\n"
			        "\t\trmdac - remove duplicate area centroids ('type' option ignored)\n"
			        "\t\tbpol - break (topologicaly clean) polygons (imported from "
				"non topological format (like shapefile). Boundaries are broken on each "
				"point shared between 2 and more polygons where angles of segments "
			        "are different";
	
	thresh_opt = G_define_option();
	thresh_opt ->key = "thresh";
	thresh_opt ->type =  TYPE_DOUBLE;
	thresh_opt ->required = NO;
	thresh_opt ->multiple = YES;
        thresh_opt ->description = "Threshold in map units for each tool (default: 0.0).";

	x_flag = G_define_flag ();
	x_flag->key             = 'x';
	x_flag->description     = "Write out intersection points instead of broken lines";
	
        if (G_parser (argc, argv))
	    exit(-1); 
	
	otype = Vect_option_to_types ( type_opt );
	
	atools = 20;
	tools = (int *) G_malloc ( atools * sizeof(int) );

	/* Read tools */
	ntools = 0; i = 0; 
	while (tool_opt->answers[i]) {
            if ( i + 1 >= atools ) {
	        atools += 20;
		G_realloc ( tools,  atools * sizeof(int) );
	    }
	    
	    G_debug ( 1, "tool : %s", tool_opt->answers[i] );
	    if ( strcmp ( tool_opt->answers[i], "break" ) == 0 )
		tools[ntools] = TOOL_BREAK;
	    else if ( strcmp ( tool_opt->answers[i], "rmdupl" ) == 0 )
		tools[ntools] = TOOL_RMDUPL;
	    else if ( strcmp ( tool_opt->answers[i], "svtlx" ) == 0 )
		tools[ntools] = TOOL_SVTLX;
	    else if ( strcmp ( tool_opt->answers[i], "rmdac" ) == 0 )
		tools[ntools] = TOOL_RMDAC;
	    else if ( strcmp ( tool_opt->answers[i], "bpol" ) == 0 )
		tools[ntools] = TOOL_BPOL;
	    else 
		G_fatal_error ( "Tool doesn't exist" );

	    ntools++; i++;
	}
	
	G_debug ( 1, "ntools = %d", ntools );
	threshs = (double *) G_malloc ( ntools * sizeof(double) );
	
	/* Read thresholds */
	for ( i = 0; i < ntools; i++ ) threshs[i] = 0.0;
	i = 0;
	while ( thresh_opt->answers && thresh_opt->answers[i] ) {
	    threshs[i] = atof ( thresh_opt->answers[i] ) ;
	    G_debug ( 1, "thresh : %s -> %f ", tool_opt->answers[i], threshs[i] );
	    
	    if (  tools[i] != TOOL_SVTLX ) {
		G_warning ("Threshold for tool %d may not be > 0, set to 0", i + 1);
		threshs[i] = 0.0;
	    }
	    i++;
	}

        /* Print tool table */
	fprintf (stdout,             "+---------------------------------+--------------+\n" );
	fprintf (stdout,             "| Tool                            | Threshold    |\n" );
	fprintf (stdout,             "+---------------------------------+--------------+\n" );
	for ( i = 0; i < ntools; i++ ) {
	    switch ( tools[i] ) {
		case ( TOOL_BREAK ) :
	            fprintf (stdout, "| Break                           |" );	    
		    break;
		case ( TOOL_RMDUPL ) :
	            fprintf (stdout, "| Remove duplicates               |" );	    
		    break;
		case ( TOOL_SVTLX ) :
	            fprintf (stdout, "| Snap vertices                   |" );	    
		    break;
		case ( TOOL_RMDAC ) :
	            fprintf (stdout, "| Remove duplicate area centroids |" );	    
		    break;
		case ( TOOL_BPOL ) :
	            fprintf (stdout, "| Break polygons                  |" );	    
		    break;
	    }
	    fprintf (stdout, " %e |\n", threshs[i] );	    
	}
	fprintf (stdout,             "+---------------------------------+--------------+\n" );
		    
	/* open input vector */
        if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
	     G_fatal_error ("Could not find input map <%s>\n", in_opt->answer);
	}

        /* Input vector may be both on level 1 and 2. Level 2 is required for 
	 * virtual centroids (shapefile/OGR) and level 1 if input is too big 
	 * and build would take a long time */
	Vect_open_old (&In, in_opt->answer, mapset); 

	with_z = Vect_is_3d (&In);
    
	Vect_set_fatal_error (GV_FATAL_PRINT);
	if (0 > Vect_open_new (&Out, out_opt->answer, with_z)) {
	     Vect_close (&In);
	     exit (1);
	}

	/* Copy input to output */
	Vect_copy_head_data (&In, &Out);
	Vect_hist_copy (&In, &Out);
	Vect_hist_command ( &Out );
	Vect_copy_map_lines ( &In, &Out );
	Vect_copy_tables ( &In, &Out, 0 );
	
	fprintf (stdout, "Building topology for copy of input vector ...\n" );	    
	Vect_build ( &Out, stdout );
	Vect_close (&In);
	Vect_close (&Out);

	Vect_open_update (&Out, out_opt->answer, G_mapset()); 

	fprintf (stdout,         "--------------------------------------------------\n" );
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
		case TOOL_RMDAC:
		    fprintf (stderr, "Tool: Remove duplicate area centroids\n" );
		    fflush ( stderr );
                    rmdac ( &Out );
		    break;
		case TOOL_SVTLX:
		    fprintf (stderr, "Tool: Snap vertex to a line and create new vertex at that line\n" );
		    fflush ( stderr );
                    svtl ( &Out, otype, TOOL_SVTLX, threshs[i], (int) x_flag->answer );
		    break;
		case TOOL_BPOL:
		    fprintf (stderr, "Tool: Break polygons\n" );
		    fflush ( stderr );
                    break_polygons ( &Out, otype, (int) x_flag->answer );
		    break;
	    }
	    fprintf (stdout,         "--------------------------------------------------\n" );
	}

	Vect_build (&Out, stdout);
	Vect_close (&Out);

	exit(0) ;
}


