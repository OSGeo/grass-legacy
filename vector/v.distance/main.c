/***************************************************************
 *
 * MODULE:       v.distance
 * 
 * AUTHOR(S):    - J.Soimasuo 15.9.1994, University of Joensuu,
 *                 Faculty of Forestry, Finland
 *               - some additions 2002 Markus Neteler
 *               - updated to 5.1 by Radim Blazek 2003
 *               
 * PURPOSE:      Calculates distance from a point to nearest line or point in vector layer. 
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
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
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    register int ret, error;
    char vectname[1024];
    char *mapset;
    struct GModule *module;
    struct Option *opt1, *opt2, *type_opt, *field_opt, *max_opt;
    struct Map_info Map;
    int type, field, level;
    double max;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
	"Calculates distance from a point to nearest line or point in vector layer.";

    opt1 = G_define_option() ;
    opt1->key        = "map" ;
    opt1->type       = TYPE_STRING ;
    opt1->gisprompt  = "old,dig,Vector";
    opt1->required   = YES ;
    opt1->description= "Name of existing vector file" ;

    opt2 = G_define_option();
    opt2->key = "east_north";
    opt2->type = TYPE_DOUBLE;
    opt2->key_desc = "east,north";
    opt2->multiple = YES;
    opt2->required = NO;
    opt2->description = "Coordinates for query";

    type_opt = G_define_option() ;
    type_opt->key        = "type" ;
    type_opt->type       = TYPE_STRING ;
    type_opt->required   = NO ;
    type_opt->multiple   = YES ;
    type_opt->answer     = "point,line" ;
    type_opt->options    = "point,line";
    type_opt->description= "Select type: point or/and line";

    field_opt = G_define_standard_option(G_OPT_V_FIELD) ;     
    
    max_opt = G_define_option();
    max_opt->key = "max";
    max_opt->type = TYPE_DOUBLE;
    max_opt->required = NO;
    max_opt->answer = "1000000";
    max_opt->description = "Maximum distance";

    setbuf (stdout, NULL);

    if (G_parser(argc, argv))
        exit(-1);
   
    error = 0; 

    type = Vect_option_to_types ( type_opt );
    field = atoi (field_opt->answer); 
    max = atof (max_opt->answer);

    /* Open mapput and output vector files */

    strcpy (vectname, opt1->answer);

    if (error)
	exit (-1);

    mapset = G_find_vector2 (vectname, "") ;

    level = Vect_open_old (&Map, vectname, mapset);
    if (0 > level)
	G_fatal_error ("Can't open vector file");
    if (2 > level)
	G_fatal_error ("Must first run v.support on vector file");

    ret = distance (opt2->answers, &Map, type, field, max);
    Vect_close (&Map);
    exit (0);
}

int oops (int line, char *buf, char *msg)
{
    static int first = 1;
    if (!isatty(0))
    {
	if (first)
	{
	    fprintf (stderr, "%s: ** input errors **\n",
		G_program_name());
	    first = 0;
	}
	fprintf (stderr, "line %d: %s\n", line, buf);
    }
    fprintf (stderr, "** %s **\n", msg);

    return 0;
}
