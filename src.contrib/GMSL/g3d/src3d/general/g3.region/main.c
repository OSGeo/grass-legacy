#define MAIN
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "glob.h"
#include "gis.h"
#include "G3d.h"
#include "local_proto.h"

#define selection(x) (strcmp(buf,x)==0)

/* the following is copied from wind_format.c */
void
format_double (value, buf)
    double value;
    char *buf;
{
    sprintf (buf, "%.8f", value);
    G_trim_decimal (buf);
}

int max(a,b)
{
    return a>b ? a:b;
}

int main(argc,argv) 
int  argc;
char *argv[];
{
    char buf[200];
    int (*option)();
    int ok;
    char *prj;
    char north[20], south[20], nsres[20];
    char east[20], west[20], ewres[20];
    char top[20], bottom[20], tbres[20];
    int len1, len2, len3;
    G3D_Region window3d;

    G_gisinit (argv[0]) ;
    G3d_initDefaults ();

    G3d_getWindow (&window3d);

    while (1)
    {
	G_clear_screen ();
	fprintf (stderr, "                         REGION FACILITY\n");
	fprintf (stderr, "LOCATION: %-40s MAPSET: %s\n\n", G_location(), G_mapset());
	G_format_northing (window3d.north, north, window3d.proj);
	G_format_northing (window3d.south, south, window3d.proj);
	G_format_resolution (window3d.ns_res, nsres, window3d.proj);

	G_format_easting (window3d.east, east, window3d.proj);
	G_format_easting (window3d.west, west, window3d.proj);
	G_format_resolution (window3d.ew_res, ewres, window3d.proj);

	format_double (window3d.top, top);
	format_double (window3d.bottom, bottom);
	format_double (window3d.tb_res, tbres);

	len1 = max(max(strlen(north), strlen(east)), strlen(top));
	len2 = max(max(strlen(south), strlen(west)), strlen(bottom));
	len3 = max(max(strlen(ewres), strlen(nsres)), strlen(tbres));

	fprintf (stderr, "CURRENT REGION: N=%*s  S=%*s  RES=%*s    ROWS=%d\n",
	    len1, north, len2, south, len3, nsres, window3d.rows);
	fprintf (stderr, "                E=%*s  W=%*s  RES=%*s    COLS=%d\n",
	    len1, east, len2, west, len3, ewres, window3d.cols);
	fprintf (stderr, "                T=%*s  B=%*s  RES=%*s  DEPTHS=%d\n",
	    len1, top, len2, bottom, len3, tbres, window3d.depths);

	prj = G_database_projection_name();
	if (!prj) prj = "** unknown **";
	fprintf (stderr, "PROJECTION: %d (%s)\n", window3d.proj, prj);
	fprintf (stderr, "ZONE:       %d\n", window3d.zone);

	fprintf (stderr, "\n\nPlease select one of the following options\n\n");
	fprintf (stderr, "   Current 3d Region                        3d Region Database\n\n");
	fprintf (stderr, "1  Modify current 3d region directly    10  Save current region in 3d database\n");
	fprintf (stderr, "2  Set from default region              11  Create a new 3d region\n");
	fprintf (stderr, "3  Set from a database region           12  Modify an existing 3d region\n");
	fprintf (stderr, "4  Set from a raster map\n");
	fprintf (stderr, "5  Set from a vector map\n");
	fprintf (stderr, "6  Set from 3d.view file\n");
	fprintf (stderr, "7  Set from a 3d default region\n");
	fprintf (stderr, "8  Set from a 3d database region\n");
	fprintf (stderr, "9  Set from a 3d raster map\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "RETURN to quit\n\n");
	fprintf (stderr, "> ");

	if (!G_gets(buf)) continue;
	G_strip (buf);

	if (selection(""))         exit(0);
	else if (selection("1"))   option = modify_cur;
	else if (selection("2"))   option = cur_from_def;
	else if (selection("3"))   option = cur_from_db;
	else if (selection("4"))   option = from_cellhd ;
	else if (selection("5"))   option = from_vect ;
	else if (selection("10"))  option = cur_to_db;
	else if (selection("11"))  option = new_db;
	else if (selection("12"))  option = modify_db;
	else if (selection("6"))   option = from_view;
	else if (selection("9"))   option = from_3dcellhd;
	else if (selection("8"))   option = cur_from_3ddb;
	else if (selection("7"))   option = cur_from_3ddef;

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

