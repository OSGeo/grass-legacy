/***************************************************************
 *
 * MODULE:       v.net
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Network maintenance
 *               
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "proto.h"

int main (int argc, char **argv)
{
    struct GModule *module;
    struct Option *input;
    struct Option *output;
    struct Option *action;
    struct Option *afield_opt, *nfield_opt;
    int    afield, nfield;

    /*  Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description = "Network maintenance.";

    /* Define the options */
    input = G_define_option ();
    input->key = "input";
    input->type = TYPE_STRING;
    input->required = YES;
    input->multiple = NO;
    input->gisprompt = "old,vector,vector";
    input->description = "Input vector map";

    output = G_define_option ();
    output->key = "output";
    output->type = TYPE_STRING;
    output->required = NO;
    output->multiple = NO;
    output->gisprompt = "new,vector,vector";
    output->description = "Output vector map";

    action = G_define_option ();
    action->key = "operation";
    action->type = TYPE_STRING;
    action->required = NO;
    action->multiple = NO;
    action->answer = "nodes";
    action->options = "nodes,report";
    action->description = "Operation to be performed\n"
	    "\t\tnodes - new point is placed on each node (line end) if doesn't exist\n"
	    "\t\treport - print to standard output: line_category start_point_category end_point_category";

    afield_opt = G_define_standard_option(G_OPT_V_FIELD);
    afield_opt->key = "alayer";
    afield_opt->answer = "1";
    afield_opt->description = "Arc layer";

    nfield_opt = G_define_standard_option(G_OPT_V_FIELD);
    nfield_opt->key = "nlayer";
    nfield_opt->answer = "2";
    nfield_opt->description = "Node layer";    
  
    if (G_parser(argc, argv))
        exit (-1);
  
    afield = atoi (afield_opt->answer);
    nfield = atoi (nfield_opt->answer);
    
    if ( action->answer[0] == 'n' ) { /* nodes */
	Vect_check_input_output_name ( input->answer, output->answer, GV_FATAL_EXIT );
	
        if ( output->answer == NULL ) 
	    G_fatal_error("Output vector map must be specified");

        nodes ( input->answer, output->answer);
    }

    if ( action->answer[0] == 'r' ) /* report */
      report ( input->answer, afield, nfield);

  return (0);
}
