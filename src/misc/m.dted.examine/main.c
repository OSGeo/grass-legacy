#include <unistd.h>
#include "gis.h"
#include "local_proto.h"
static int count = 0;

int 
main (int argc, char *argv[])
{
	struct GModule *module;
    struct Option *tapename;
    unsigned char buf[4096];
    char type[5];
    int lat_d, lat_m, lat_s;
    int lon_d, lon_m, lon_s;
    int lon_res, lat_res ;
    int lon_lines, lat_points ;
    char lat_c, lon_c;
    int eof ;
    int y;
    long x;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Provides a terse description of "
		"level 1 and 2 digital terrain elevation data (DTED) files "
		"produced and distributed by the Defense Mapping Agency "
		"(DMA) on 1/2-inch magnetic tapes.";

    tapename = G_define_option();
    tapename->key = "input";
    tapename->type = TYPE_STRING;
    tapename->required = YES;
    tapename->description = "Name of tape drive device";

    if (G_parser(argc,argv))
	exit(1);

    opentape(tapename->answer);

    count = 0;
    eof = 0;
    while(eof < 2)
    {
	if(readtape (buf, sizeof buf) == 0)
	{
	    dumpcount();
	    fprintf (stdout,"**EOF**\n");
	    eof++ ;
	    continue;
	}
	eof = 0;
	if ((*buf) == 0252)
	{
	    if (count == 0)
	    {
		fprintf (stdout,"DATA ");
		fflush (stdout);
	    }
	    count++;
	    continue;
	}
	dumpcount();
	sscanf (buf, "%3s", type);
	fprintf (stdout,"%s\n", type);
	if (strcmp (type, "UHL") != 0)
	    continue ;
	sscanf (buf,
	    "%*4c%3d%2d%2d%1c%3d%2d%2d%1c%4d%4d%*19c%4d%4d",
		&lon_d, &lon_m, &lon_s, &lon_c ,
		&lat_d, &lat_m, &lat_s, &lat_c,
		&lon_res, &lat_res,
		&lon_lines, &lat_points);
	fprintf (stdout," SW LON: %3d:%02d:%02d%c   RES: %.1f (%d lines)\n",
	     lon_d, lon_m, lon_s, lon_c, lon_res/10.0, lon_lines);
	fprintf (stdout,"    LAT: %3d:%02d:%02d%c   RES: %.1f (%d points)\n",
	     lat_d, lat_m, lat_s, lat_c, lat_res/10.0, lat_points);
	
/* since dma lat/long are for the center of the data cells
   calculate y as if there were 1 less longitude line */
	y = (lon_lines-1) * lon_res / 10.0 ;
	if (lon_c == 'W') y = -y;
	x = (long) lon_d * 3600 + lon_m * 60 + lon_s + y;

	fprintf (stdout," NE LON: %3ld:%02ld:%02ld%c\n",
		x/3600, (x%3600)/60, x%60, lon_c);

/* since dma lat/long are for the center of the data cells
   calculate y as if there were 1 less latitude point */
	y = (lat_points-1) * lat_res / 10.0 ;
	if (lat_c == 'S') y = -y;
	x = (long) lat_d * 3600 + lat_m * 60 + lat_s + y;

	fprintf (stdout,"    LAT: %3ld:%02ld:%02ld%c\n",
		x/3600, (x%3600)/60, x%60, lat_c);
    }
    closetape();

    return 0;
}

int dumpcount (void)
{
    if (count) fprintf (stdout,"%d records\n", count);
    count = 0;

    return 0;
}
