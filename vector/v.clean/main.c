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
#include <stdio.h> 
#include "gis.h"
#include "Vect.h"
#include "proto.h"

int rmdac ( struct Map_info *Out );
void remove_bridges ( struct Map_info *Map, struct Map_info *Err );
int prune ( struct Map_info *, int, double );

int 
main (int argc, char *argv[])
{
	struct Map_info In, Out, Err, *pErr;
        int    i, otype, with_z;
	char   *mapset;
	struct GModule *module;
	struct Option *in_opt, *out_opt, *type_opt, *tool_opt, *thresh_opt, *err_opt;
	struct Flag *no_build_flag;
	int    *tools, ntools, atools;
	double *threshs;
	int    level;
	int    count;
	double size;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description = "Break lines at intersections.";

	in_opt = G_define_standard_option(G_OPT_V_INPUT);
	out_opt = G_define_standard_option(G_OPT_V_OUTPUT);
	type_opt = G_define_standard_option(G_OPT_V_TYPE) ;
	
	err_opt = G_define_standard_option(G_OPT_V_OUTPUT);
	err_opt->key = "err";
	err_opt->description = "Name of output map where errors are written.";
        err_opt->required = NO;
	
	tool_opt = G_define_option();
	tool_opt->key = "tool";
	tool_opt->type =  TYPE_STRING;
	tool_opt->required = YES;
	tool_opt->multiple = YES;
	tool_opt->options = "break,rmdupl,rmdangle,chdangle,rmbridge,chbridge,snap,rmdac,bpol,prune,"
	                    "rmarea,rmsa";
        tool_opt->description = "Cleaning tool";
        tool_opt->descriptions = 
	    	"break;break lines at each intersection;"
	    	"rmdupl;remove duplicate lines (pay attention to categories!);"
	    	"rmdangle;remove dangles, threshold ignored if < 0;"
		"chdangle;change the type of boundary dangle to line, "
	              	"threshold ignored if < 0, input line type is ignored;"
        	"rmbridge;remove bridges connecting area and island or 2 islands;"
		"chbridge;change the type of bridges connecting area and island "
		    	"or 2 islands from boundary to line;"
		"snap;snap lines to vertex in threshold;"
		"rmdac;remove duplicate area centroids ('type' option ignored);"
		"bpol;break (topologicaly clean) polygons (imported from "
		      	"non topological format (like shapefile). Boundaries are broken on each "
		      	"point shared between 2 and more polygons where angles of segments are different;"
		"prune;remove vertices in threshold from lines and boundaries, "
			"boundary is pruned only if topology is not damaged (new intersection, "
       			"changed attachement of centroid), first and last segment of the boundary "
			"is never changed;"
		"rmarea;remove small areas, the longest boundary with adjacent area is removed;"
		"rmsa;remove small angles between lines at nodes";
	thresh_opt = G_define_option();
	thresh_opt ->key = "thresh";
	thresh_opt ->type =  TYPE_DOUBLE;
	thresh_opt ->required = NO;
	thresh_opt ->multiple = YES;
        thresh_opt ->label       = "Threshold";
        thresh_opt ->description = "Threshold in map units for each tool (default: 0.0).";

        no_build_flag = G_define_flag ();
        no_build_flag->key             = 'b';
        no_build_flag->description     = "Do not rebuild and store the topology at the end.";

        if (G_parser (argc, argv))
	    exit(-1); 
	
	otype = Vect_option_to_types ( type_opt );
	
	Vect_check_input_output_name ( in_opt->answer, out_opt->answer, GV_FATAL_EXIT );
	if ( err_opt->answer ) {
	    Vect_check_input_output_name ( in_opt->answer, err_opt->answer, GV_FATAL_EXIT );
	}
	
	atools = 20;
	tools = (int *) G_malloc ( atools * sizeof(int) );

	/* Read tools */
	ntools = 0; i = 0; 
	if ( strlen(tool_opt->answer) < 1 )
		G_fatal_error ("You must select at least one tool.");
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
	    else if ( strcmp ( tool_opt->answers[i], "rmdangle" ) == 0 )
		tools[ntools] = TOOL_RMDANGLE;
	    else if ( strcmp ( tool_opt->answers[i], "chdangle" ) == 0 )
		tools[ntools] = TOOL_CHDANGLE;
	    else if ( strcmp ( tool_opt->answers[i], "rmbridge" ) == 0 )
		tools[ntools] = TOOL_RMBRIDGE;
	    else if ( strcmp ( tool_opt->answers[i], "chbridge" ) == 0 )
		tools[ntools] = TOOL_CHBRIDGE;
	    else if ( strcmp ( tool_opt->answers[i], "snap" ) == 0 )
		tools[ntools] = TOOL_SNAP;
	    else if ( strcmp ( tool_opt->answers[i], "rmdac" ) == 0 )
		tools[ntools] = TOOL_RMDAC;
	    else if ( strcmp ( tool_opt->answers[i], "bpol" ) == 0 )
		tools[ntools] = TOOL_BPOL;
	    else if ( strcmp ( tool_opt->answers[i], "prune" ) == 0 )
		tools[ntools] = TOOL_PRUNE;
	    else if ( strcmp ( tool_opt->answers[i], "rmarea" ) == 0 )
		tools[ntools] = TOOL_RMAREA;
	    else if ( strcmp ( tool_opt->answers[i], "rmsa" ) == 0 )
		tools[ntools] = TOOL_RMSA;
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
	    
	    if (  tools[i] != TOOL_SNAP && tools[i] != TOOL_RMDANGLE && tools[i] != TOOL_CHDANGLE
	          && tools[i] != TOOL_PRUNE && tools[i] != TOOL_RMAREA ) {
		G_warning ("Threshold for tool %d may not be > 0, set to 0", i + 1);
		threshs[i] = 0.0;
	    }
	    i++;
	}

        /* Print tool table */
	fprintf (stderr,             "+---------------------------------+---------------+\n" );
	fprintf (stderr,             "| Tool                            | Threshold     |\n" );
	fprintf (stderr,             "+---------------------------------+---------------+\n" );
	for ( i = 0; i < ntools; i++ ) {
	    switch ( tools[i] ) {
		case ( TOOL_BREAK ) :
	            fprintf (stderr, "| Break                            |" );	    
		    break;
		case ( TOOL_RMDUPL ) :
	            fprintf (stderr, "| Remove duplicates                |" );	    
		    break;
		case ( TOOL_RMDANGLE ) :
	            fprintf (stderr, "| Remove dangles                   |" );	    
		    break;
		case ( TOOL_CHDANGLE ) :
	            fprintf (stderr, "| Change type of boundary dangles  |" );	    
		    break;
		case ( TOOL_RMBRIDGE ) :
	            fprintf (stderr, "| Remove bridges                   |" );	    
		    break;
		case ( TOOL_CHBRIDGE ) :
	            fprintf (stderr, "| Change type of boundary bridges  |" );	    
		    break;
		case ( TOOL_SNAP ) :
	            fprintf (stderr, "| Snap vertices                    |" );	    
		    break;
		case ( TOOL_RMDAC ) :
	            fprintf (stderr, "| Remove duplicate area centroids  |" );	    
		    break;
		case ( TOOL_BPOL ) :
	            fprintf (stderr, "| Break polygons                   |" );	    
		    break;
		case ( TOOL_PRUNE ) :
	            fprintf (stderr, "| Prune                            |" );	    
		    break;
		case ( TOOL_RMAREA ) :
	            fprintf (stderr, "| Remove small areas               |" );	    
		    break;
		case ( TOOL_RMSA ) :
	            fprintf (stderr, "| Remove small angles at nodes     |" );	    
		    break;
	    }
	    fprintf (stderr, " %e |\n", threshs[i] );	    
	}
	fprintf (stderr,             "+---------------------------------+---------------+\n" );
		    
	/* open input vector */
        if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
	     G_fatal_error ("Could not find input map <%s>\n", in_opt->answer);
	}

        /* Input vector may be both on level 1 and 2. Level 2 is necessary for 
	 * virtual centroids (shapefile/OGR) and level 1 is better if input is too big 
	 * and build in previous module (like v.in.ogr or other call to v.clean) would take 
	 * a long time */
	level = Vect_open_old (&In, in_opt->answer, mapset); 

	with_z = Vect_is_3d (&In);
    
	Vect_set_fatal_error (GV_FATAL_PRINT);
	if ( 0 > Vect_open_new (&Out, out_opt->answer, with_z)) {
	     Vect_close (&In);
	     exit (1);
	}

	if ( err_opt->answer ) {
	    Vect_set_fatal_error (GV_FATAL_PRINT);
	    if (0 > Vect_open_new (&Err, err_opt->answer, with_z)) {
		 Vect_close (&In);
		 Vect_close (&Out);
		 exit (1);
	    }
	    pErr = &Err;
	} else {
	    pErr = NULL;
	}

	/* Copy input to output */
	fprintf (stderr, "Copying vector lines ...\n" );
	Vect_copy_head_data (&In, &Out);
	Vect_hist_copy (&In, &Out);
	Vect_hist_command ( &Out );

	/* This works for both level 1 and 2 */
	Vect_copy_map_lines ( &In, &Out );
	Vect_copy_tables ( &In, &Out, 0 );

	Vect_set_release_support ( &In );
	Vect_close (&In);

	/* Start with GV_BUILD_NONE and for each tool use unly the necessary level! */
	fprintf (stderr,         "--------------------------------------------------\n" );
	for ( i = 0; i < ntools ; i++ ) { 
	    if (  tools[i] == TOOL_RMDAC || tools[i] == TOOL_PRUNE || tools[i] == TOOL_RMAREA ) {
	        if ( Vect_get_built ( &Out ) >= GV_BUILD_CENTROIDS ) {
		    Vect_build_partial ( &Out, GV_BUILD_CENTROIDS, NULL );
		} else {
	            fprintf (stderr,         "Rebuilding parts of topology ...\n" );
		    Vect_build_partial ( &Out, GV_BUILD_CENTROIDS, stderr );
	            fprintf (stderr,         "--------------------------------------------------\n" );
		}
	    } else {
	        if ( Vect_get_built ( &Out ) >= GV_BUILD_BASE )	{
		    Vect_build_partial ( &Out, GV_BUILD_BASE, NULL );
		} else {
	            fprintf (stderr,         "Rebuilding parts of topology ...\n" );
		    Vect_build_partial ( &Out, GV_BUILD_BASE, stderr );
	            fprintf (stderr,         "--------------------------------------------------\n" );
		}
	    }

	    switch ( tools[i] ) {
		case TOOL_BREAK:
		    fprintf (stderr, "Tool: Break lines at intersections\n" );
		    fflush ( stderr );
                    Vect_break_lines ( &Out, otype, pErr, stderr );
		    break;
		case TOOL_RMDUPL:
		    fprintf (stderr, "Tool: Remove duplicates\n" );
		    fflush ( stderr );
                    Vect_remove_duplicates ( &Out, otype, pErr, stderr );
		    break;
		case TOOL_RMDANGLE:
		    fprintf (stderr, "Tool: Remove dangles\n" );
		    fflush ( stderr );
                    Vect_remove_dangles ( &Out, otype, threshs[i], pErr, stderr );
		    break;
		case TOOL_CHDANGLE:
		    fprintf (stderr, "Tool: Change type of boundary dangles\n" );
		    fflush ( stderr );
                    Vect_chtype_dangles ( &Out, threshs[i], pErr, stderr );
		    break;
		case TOOL_RMBRIDGE:
		    fprintf (stderr, "Tool: Remove bridges\n" );
		    fflush ( stderr );
                    Vect_remove_bridges ( &Out, pErr, stderr );
		    break;
		case TOOL_CHBRIDGE:
		    fprintf (stderr, "Tool: Change type of boundary bridges\n" );
		    fflush ( stderr );
                    Vect_chtype_bridges ( &Out, pErr, stderr );
		    break;
		case TOOL_RMDAC:
		    fprintf (stderr, "Tool: Remove duplicate area centroids\n" );
		    fflush ( stderr );
                    rmdac ( &Out );
		    break;
		case TOOL_SNAP:
		    fprintf (stderr, "Tool: Snap line to vertex in threshold\n" );
		    fflush ( stderr );
                    Vect_snap_lines ( &Out, otype, threshs[i], pErr, stderr );
		    break;
		case TOOL_BPOL:
		    fprintf (stderr, "Tool: Break polygons\n" );
		    fflush ( stderr );
                    Vect_break_polygons ( &Out, otype, pErr, stderr );
		    break;
		case TOOL_PRUNE:
		    fprintf (stderr, "Tool: Prune lines/boundaries\n" );
		    fflush ( stderr );
                    prune ( &Out, otype, threshs[i] );
		    break;
		case TOOL_RMAREA:
		    fprintf (stderr, "Tool: Remove small areas\n" );
		    fflush ( stderr );
                    count = Vect_remove_small_areas ( &Out, threshs[i], pErr, stderr, &size );
		    fprintf (stderr, "%d areas of total size %g removed\n", count, size );
		    break;
		case TOOL_RMSA:
		    fprintf (stderr, "Tool: Remove small angles at nodes\n" );
		    fflush ( stderr );
                    count = Vect_clean_small_angles_at_nodes ( &Out, otype, pErr, stderr );
		    fprintf (stderr, "%d modifications done\n", count );
		    break;
	    }
	    fprintf (stderr,         "--------------------------------------------------\n" );
	}

	if ( !no_build_flag->answer ) {
	    fprintf (stderr, "Rebuilding topology for output vector ...\n" );	    
	    Vect_build_partial (&Out, GV_BUILD_NONE, NULL);
	    Vect_build (&Out, stderr);
	} else { 
	    Vect_build_partial ( &Out, GV_BUILD_NONE, NULL ); /* -> topo not saved */
	}
	Vect_close (&Out);

	if ( pErr ) {
	    fprintf (stderr,         "--------------------------------------------------\n" );
	    fprintf (stderr, "Building topology for error vector ...\n" );
	    Vect_build (pErr, stderr);
	    Vect_close (pErr);
	}

	exit(0) ;
}


