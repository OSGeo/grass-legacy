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
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/Vect.h>
#include "proto.h"

int main (int argc, char **argv)
{
    struct GModule *module;
    struct Option *input;
    struct Option *output;
    struct Option *action;
    struct Option *afield_opt, *nfield_opt;
    struct Flag *cats_flag;
    int    afield, nfield;

    /*  Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->keywords = _("vector, networking");
    module->description = _("Network maintenance.");

    /* Define the options */
    input = G_define_standard_option (G_OPT_V_INPUT);

    output = G_define_standard_option (G_OPT_V_OUTPUT);
    output->required = NO;

    action = G_define_option ();
    action->key = "operation";
    action->type = TYPE_STRING;
    action->required = NO;
    action->multiple = NO;
    action->answer = "nodes";
    action->options = "nodes,report,nreport";
    action->description = _("Operation to be performed");
    action->descriptions = _("nodes;new point is placed on each node (line end) "
			     "if doesn't exist;"
			     "report;print to standard output "
			     "{line_category start_point_category end_point_category};"
			     "nreport;print to standard output "
			     "{point_category line_category[,line_category...]}");

    afield_opt = G_define_standard_option(G_OPT_V_FIELD);
    afield_opt->key = "alayer";
    afield_opt->description = _("Arc layer");

    nfield_opt = G_define_standard_option(G_OPT_V_FIELD);
    nfield_opt->key = "nlayer";
    nfield_opt->answer = "2";
    nfield_opt->description = _("Node layer");    

    cats_flag = G_define_flag();
    cats_flag->key = 'c';
    cats_flag->description =
      _("Assign unique categories to new points (operation=nodes)");
  
    if (G_parser(argc, argv))
        exit (EXIT_FAILURE);
  
    afield = atoi (afield_opt->answer);
    nfield = atoi (nfield_opt->answer);
    
    if ( strcmp ( action->answer, "nodes") == 0  ) { /* nodes */
	Vect_check_input_output_name ( input->answer, output->answer, GV_FATAL_EXIT );
	
        if ( output->answer == NULL ) 
	    G_fatal_error(_("Output vector map must be specified"));

        nodes ( input->answer, output->answer, cats_flag->answer, nfield);
    } 
    else  /* report */
    {
        int  act;

	if ( strcmp ( action->answer, "report") == 0  ) 
        {
            act = REPORT;
        } 
        else 
        {
            act = NREPORT;
        }
        
	report ( input->answer, afield, nfield, act);
    }

  return (EXIT_SUCCESS);
}
