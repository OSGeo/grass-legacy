#include "dma.h"

getargs (argc, argv)
    char *argv[];
{
    struct {
	struct Option *input, *output, *header,
		      *north, *south, *east, *west;
    } parm;
    struct {
	struct Flag *f, *q;
    } flag;
    char n_info[100], s_info[100], e_info[100], w_info[100];
    double xeast, xwest, xnorth, xsouth;

    int i;
    int k;
    int d1,m1,s1;
    int d2,m2,s2;
    int lat, lon;
    char hemi1[3], hemi2[3];
    char temp[100];

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "Name of tape drive device";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;
    parm.output->description = "Name of output file";

    parm.header = G_define_option();
    parm.header->key = "header";
    parm.header->type = TYPE_STRING;
    parm.header->required = YES;
    parm.header->description = "Name of header output file";

    parm.north = G_define_option();
    parm.north->key = "north";
    parm.north->type = TYPE_STRING;
    parm.north->required = YES;
    parm.north->description = n_info;
    sprintf (n_info, "North latitude of region to extract (format: %s)",
	G_lat_format_string());

    parm.south = G_define_option();
    parm.south->key = "south";
    parm.south->type = TYPE_STRING;
    parm.south->required = YES;
    parm.south->description = s_info;
    sprintf (s_info, "South latitude of region to extract (format: %s)",
	G_lat_format_string());

    parm.east = G_define_option();
    parm.east->key = "east";
    parm.east->type = TYPE_STRING;
    parm.east->required = YES;
    parm.east->description = e_info;
    sprintf (e_info, "East longitude of region to extract (format: %s)",
	G_lon_format_string());

    parm.west = G_define_option();
    parm.west->key = "west";
    parm.west->type = TYPE_STRING;
    parm.west->required = YES;
    parm.west->description = w_info;
    sprintf (w_info, "West longitude of region to extract (format: %s)",
	G_lon_format_string());

    flag.f = G_define_flag();
    flag.f->key = 'f';
    flag.f->description = "Read the entire tape in all cases";

    flag.q = G_define_flag();
    flag.q->key = 'q';
    flag.q->description = "Run quietly";

    if (G_parser(argc,argv))
	exit(1);

    verbose = !flag.q->answer;
    stopok = !flag.f->answer;

    tapename = parm.input->answer;
    outname  = parm.output->answer;
    headname = parm.header->answer;

    if (!G_scan_northing (parm.north->answer, &xnorth, PROJECTION_LL))
    {
	fprintf (stderr, "%s=%s - illegal latitude\n",
		parm.north->key, parm.north->answer);
	return 0;
    }

    if (!G_scan_northing (parm.south->answer, &xsouth, PROJECTION_LL))
    {
	fprintf (stderr, "%s=%s - illegal latitude\n",
		parm.south->key, parm.south->answer);
	return 0;
    }

    if (!G_scan_easting (parm.east->answer, &xeast, PROJECTION_LL))
    {
	fprintf (stderr, "%s=%s - illegal longitude\n",
		parm.east->key, parm.east->answer);
	return 0;
    }

    if (!G_scan_easting (parm.west->answer, &xwest, PROJECTION_LL))
    {
	fprintf (stderr, "%s=%s - illegal longitude\n",
		parm.west->key, parm.west->answer);
	return 0;
    }
/* convert lat/lon to 10*arc seconds (as required by printgeo.c)*/
    north = xnorth * 36000;
    south = xsouth * 36000;
    east  = -(xeast * 36000); /* longitude sense must be inverted */
    west  = -(xwest * 36000);

    return 1;
}
