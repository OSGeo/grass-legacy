/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       m.clump
 * AUTHOR(S):    Michael Shapiro, CERL
 * PURPOSE:      Aggregate point data into clusters of like data using
 *               Voronoi tesselation. 
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#define global
#include "glob.h"

static struct parms parms;

main (argc, argv) char *argv[];
{

    G_gisinit (argv[0]);

    parse_command_line (argc, argv, &parms);

    if (parms.region)
	set_region();

    init_attributes (parms.fields);
    init_barriers (parms.barriers);

    read_point_list (parms.input, parms.fs);

    triangulate_point_list();

    break_connections();

    write_results (parms.output);

    exit(0);
}

be_quiet()
{
    return parms.quiet;
}
