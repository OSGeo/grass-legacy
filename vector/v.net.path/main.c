/****************************************************************
 *
 * MODULE:       v.net.path
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Shortest path on vector network
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 ****************************************************************/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

int path ( struct Map_info *, struct Map_info *, int, double, int );

int main(int argc, char **argv)
{
    struct Option *input_opt, *output_opt, *afield_opt, *nfield_opt, *afcol, *abcol, *ncol, *type_opt;
    struct Option *max_dist;
    struct Flag   *geo_f, *segments_f;
    struct GModule *module;
    char   *mapset;
    struct Map_info In, Out;
    int    type, afield, nfield, geo;
    double maxdist;

    /* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

    module = G_define_module();
    module->description = "Find shortest path on vector network. Reads start/end points"
	    "from standard input in 2 possible formats:\n"
	    "id start_point_category end_point_category\n"
	    "id start_point_x start_point_y end_point_x end_point_y\n"
	    "Points specified by category must be exactly on network nodes.";

    input_opt = G_define_standard_option(G_OPT_V_INPUT);
    output_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    type_opt =  G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options    = "line,boundary";
    type_opt->answer     = "line,boundary";
    type_opt->description = "Arc type";

    afield_opt = G_define_standard_option(G_OPT_V_FIELD);
    afield_opt->key = "alayer";
    afield_opt->answer = "1";
    afield_opt->description = "Arc layer";
    
    nfield_opt = G_define_standard_option(G_OPT_V_FIELD);
    nfield_opt->key = "nlayer";
    nfield_opt->answer = "2";
    nfield_opt->description = "Node layer";

    afcol = G_define_option() ;
    afcol->key         = "afcol" ;
    afcol->type        = TYPE_STRING ;
    afcol->required    = NO ; 
    afcol->description = "Arc forward/both direction(s) cost column" ;
    
    abcol = G_define_option() ;
    abcol->key         = "abcol" ;
    abcol->type        = TYPE_STRING ;
    abcol->required    = NO ; 
    abcol->description = "Arc backward direction cost column" ;
    
    ncol = G_define_option() ;
    ncol->key         = "ncol" ;
    ncol->type        = TYPE_STRING ;
    ncol->required    = NO ; 
    ncol->description = "Node cost column" ;

    max_dist = G_define_option() ;
    max_dist->key = "maxdist";
    max_dist->type = TYPE_DOUBLE;
    max_dist->required = NO;
    max_dist->answer = "1000";
    max_dist->description = "Maximum distance to the network if start/end are given as coordinates. "
			    "If start/end point is outside this threshold, the path is not found "
			    "and error message is printed. To speed up the process, keep this "
			    "value as low as possible.";
    
    geo_f = G_define_flag ();
    geo_f->key             = 'g';
    geo_f->description     = "Use geodesic calculation for longitude-latitude locations";
    
    segments_f = G_define_flag ();
    segments_f->key             = 's';
    segments_f->description     = "Write output as original input segments, not each path as one line.";
    
    if(G_parser(argc,argv))
        exit(-1);

    type = Vect_option_to_types ( type_opt ); 
    afield = atoi (afield_opt->answer);
    nfield = atoi (nfield_opt->answer);
    maxdist = atof ( max_dist->answer );

    if ( geo_f->answer ) {
       geo = 1; 
       if (G_projection () != PROJECTION_LL)
          G_warning("The current projection is not longitude-latitude");
    }
    else geo = 0;

    Vect_check_input_output_name ( input_opt->answer, output_opt->answer, GV_FATAL_EXIT );
    
    mapset = G_find_vector2 (input_opt->answer, NULL); 
      
    if ( mapset == NULL) 
      G_fatal_error ("Could not find input vector '%s'\n", input_opt->answer);

    Vect_set_open_level(2);
    Vect_open_old (&In, input_opt->answer, mapset); 

    Vect_set_fatal_error (GV_FATAL_PRINT);
    if (1 > Vect_open_new (&Out, output_opt->answer, Vect_is_3d(&In) )){
        Vect_close (&In);
	G_fatal_error ("Failed opening output vector file");
    }

    Vect_net_build_graph ( &In, type, afield, nfield, afcol->answer, abcol->answer, 
	                   ncol->answer, geo, 0 );

    path ( &In, &Out, nfield, maxdist, segments_f->answer ); 

    Vect_close(&In);

    Vect_build (&Out, stdout);
    Vect_close(&Out);

    exit(0);
}



