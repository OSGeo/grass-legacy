/***************************************************************************
 * $Id$
 *
 * MODULE: 	g.region (interactive)
 * AUTHOR(S):	Michael Shapiro, CERL
 * PURPOSE: 	Program to manage and print the boundary definitions for the
 *              geographic region.
 *             
 * COPYRIGHT:  	(C) 2000 by the GRASS Development Team
 *
 *   	    	This program is free software under the GPL (>=v2)
 *   	    	Read the file COPYING that comes with GRASS for details.
 ****************************************************************************
 * $Log$
 * Revision 1.4  2000-11-26 16:33:15  andreas
 * added module description, file header, output of ellipsoid with cmdline and inter
 *
 * Revision 1.3  2000/11/08 20:31:28  andreas
 * added output of map datum
 *
 */

#define MAIN
#include <string.h>
#include <unistd.h>
#include "glob.h"

#define selection(x) (strcmp(buf,x)==0)
static int max(int,int);

int 
main (int argc, char *argv[])
{
    char buf[200];
    int (*option)();
    int ok;
    int cur_from_def();
    int modify_cur();
    int cur_from_db();
    int cur_to_db();
    int new_db();
    int modify_db();
    int from_cellhd();
    int from_vect();
    int from_sites();
    int from_view();
    char *G_database_projection_name();
    char *G__get_window();
    char *prj, *datum, *ellps;
    char north[20], south[20], nsres[20];
    char east[20], west[20], ewres[20];
    int len1, len2, len3;

    G_gisinit (argv[0]) ;

/* get current window.  If invalid replace with default window */

    if (G__get_window (&cur_window, "", "WIND", G_mapset()))
    {
	G_get_default_window (&cur_window);
	G_put_window (&cur_window);
    }

/* if output is not the terminal, assume current window info is desired */
    if (!isatty(1))
    {
	G__write_Cell_head (stdout, &cur_window, 0);
	exit(0);
    }

    while (1)
    {
	G_clear_screen ();
	fprintf (stderr, "                         REGION FACILITY\n");
	fprintf (stderr, "LOCATION: %-40s MAPSET: %s\n\n", G_location(), G_mapset());
	G_format_northing (cur_window.north, north, cur_window.proj);
	G_format_northing (cur_window.south, south, cur_window.proj);
	G_format_resolution (cur_window.ns_res, nsres, cur_window.proj);

	G_format_easting (cur_window.east, east, cur_window.proj);
	G_format_easting (cur_window.west, west, cur_window.proj);
	G_format_resolution (cur_window.ew_res, ewres, cur_window.proj);

	len1 = max(strlen(north), strlen(east));
	len2 = max(strlen(south), strlen(west));
	len3 = max(strlen(ewres), strlen(nsres));

	fprintf (stderr, "CURRENT REGION: N=%*s  S=%*s  RES=%*s  ROWS=%d\n",
	    len1, north, len2, south, len3, nsres, cur_window.rows);
	fprintf (stderr, "                E=%*s  W=%*s  RES=%*s  COLS=%d\n",
	    len1, east, len2, west, len3, ewres, cur_window.cols);

	prj = G_database_projection_name();
	datum = G_database_datum_name();
	ellps = G_database_ellipse_name();
	if (!prj) prj = "** unknown **";
	if (!datum) datum = "** unknown (default WGS84) **";
	if (!ellps) ellps = "** unknown (default WGS84) **";
	fprintf (stderr, "PROJECTION: %d (%s)\n", cur_window.proj, prj);
	fprintf (stderr, "ZONE:       %d\n", cur_window.zone);
	fprintf (stderr, "DATUM:      %s\n", datum);
	fprintf (stderr, "ELLIPSOID:  %s\n", ellps);

	fprintf (stderr, "\nPlease select one of the following options:\n\n");
	fprintf (stderr, "   Current Region                           Region Database\n\n");
	fprintf (stderr, "1  Modify current region directly        8  Save current region in the database\n");
	fprintf (stderr, "2  Set from default region               9  Create a new region\n");
	fprintf (stderr, "3  Set from a database region            0  Modify an existing region\n");
	fprintf (stderr, "4  Set from a raster map\n");
	fprintf (stderr, "5  Set from a vector map\n");
	fprintf (stderr, "6  Set from a sites list map\n");
	fprintf (stderr, "7  Set from 3d.view file\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "RETURN to quit\n");
	fprintf (stderr, "> ");

	if (!G_gets(buf)) continue;
	G_strip (buf);

	if (selection(""))         exit(0);
	else if (selection("1"))   option = modify_cur;
	else if (selection("2"))   option = cur_from_def;
	else if (selection("3"))   option = cur_from_db;
	else if (selection("4"))   option = from_cellhd ;
	else if (selection("5"))   option = from_vect ;
	else if (selection("6"))   option = from_sites ;
	else if (selection("7"))   option = from_view;
	else if (selection("8"))   option = cur_to_db;
	else if (selection("9"))   option = new_db;
	else if (selection("0"))   option = modify_db;

	else
	{
	    fprintf (stderr, "<%s> unknown option. ", buf);
	    option = 0;
	}
	ok = 0;
	if (option)
	{
	    G_clear_screen();
	    ok = option();
	}
	fprintf (stderr, "\n\n");
	if (!ok) sleep(4);
    }
}

static int max(int a,int b)
{
    return a>b ? a:b;
}
