/* ***************************************************************
 * *
 * * MODULE:       v.mapcalc
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Category manipulations
 * *               
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"

/* This is just example and starting point */

static const char help_text[] =
"v.mapcalc - Vector map layer data calculator\n"
"usage: v.mapcalc '<map>=<expression>'\n";

int 
main (int argc, char *argv[])
{
	struct Map_info AMap, BMap, OMap;
	char   amap[500], bmap[500], omap[500], oper[100];
        int    operator, with_z, atype, btype;
	char   *amapset, *bmapset;
	
	G_gisinit(argv[0]);

        if (argc > 2 || argc == 1 || (argc > 1 && strcmp(argv[1], "help") == 0)) {
            fputs(help_text, stderr);
            return 0;
        }

	/* TODO: real parser, real mapcalc, ... (everything) */
        fprintf ( stdout, "expression : %s\n", argv[1] );
	
	if (  sscanf( argv[1], "%s = %s %s %s", omap, amap, oper, bmap) < 4 )
	    G_fatal_error ("Cannot parse expression");
	
	operator = Vect_overlay_str_to_operator ( oper );
	if ( operator == -1 )
	    G_fatal_error ("Unknown operator: %s", oper);

        fprintf ( stdout, "amap: %s\nbmap: %s\nomap: %s\noperator: %s\n", amap, bmap, omap, oper );
	
	/* open input vector */
        if ( (amapset = G_find_vector2 (amap, "")) == NULL ) 
	     G_fatal_error ( "Could not find input %s\n", amap);
	
        if ( (bmapset = G_find_vector2 (bmap, "")) == NULL ) 
	     G_fatal_error ( "Could not find input %s\n", bmap);
	
        Vect_set_open_level (2); 
	Vect_open_old ( &AMap, amap, amapset ); 
	
	Vect_set_fatal_error (GV_FATAL_PRINT);
	if ( 0 > Vect_open_old (&BMap, bmap, bmapset) ) {
            Vect_close (&AMap);
	    exit (1);
        }

	/* open output vector */
	with_z = 0;
	if (  Vect_is_3d ( &AMap ) ||  Vect_is_3d ( &BMap ) ) 
	    with_z = 1;

	if (0 > Vect_open_new (&OMap, omap, with_z)) {
            Vect_close (&AMap);
            Vect_close (&BMap);
            exit (1);
        }

       	/* a/btype should be result of parser.
	*  Note that GV_AREA and GV_BOUNDARY/GV_CENTROID at the same time doesn't make much
	*  sense - should be allowed ? */
	atype = btype = GV_POINT | GV_LINE | GV_AREA;
	Vect_overlay ( &AMap, atype, NULL, NULL, &BMap, btype, NULL, NULL, operator, &OMap );

	Vect_close (&AMap);
	Vect_close (&BMap);
	
	Vect_build (&OMap, stdout);
        Vect_close (&OMap);

	exit(0) ;
}


