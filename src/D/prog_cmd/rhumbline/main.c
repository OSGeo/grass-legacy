#include "gis.h"

#define LINE_COLOR 1
#define TEXT_COLOR 2
#define LON1       3
#define LAT1       4
#define LON2       5
#define LAT2       6


struct Command_keys keys[] =
{
    {"lcolor", LINE_COLOR},
    {"tcolor", TEXT_COLOR},
    {"lon1", LON1},
    {"lat1", LAT1},
    {"lon2", LON2},
    {"lat2", LAT2},
    {NULL, 0}
};

char line_color[80];
char text_color[80];

static double lon1,lat1,lon2,lat2;
static char have_lon1 = 0;
static char have_lat1 = 0;
static char have_lon2 = 0;
static char have_lat2 = 0;

main(argc, argv) char *argv[];
{
    int stash();
    int color;
    int use_mouse;
    char msg[100];

    G_gisinit (argv[0]);
    if (G_projection() != PROJECTION_LL)
    {
	sprintf (msg, "%s: database is not a %s\n",
		argv[0], G_projection_name(PROJECTION_LL));
	G_fatal_error (msg);
	exit(1);
    }
    *line_color = 0;
    *text_color = 0;
    if (argc > 1)
    {
	switch(G_parse_command (argc, argv, keys, stash))
	{
	case 1:      /* help requested */
	    exit(0);
	    break;
	case -1:
	    G_parse_command_usage (argv[0], keys, USAGE_LONG);
	    exit(-1);
	}
    }
    use_mouse = 1;
    if (have_lat1 || have_lat2 || have_lon1 || have_lon2)
    {
	if (!have_lat1 || !have_lat2 || !have_lon1 || !have_lon2)
	{
	    G_parse_command_usage (argv[0], keys, USAGE_LONG);
	    exit(-1);
	}
	use_mouse = 0;
    }

    R_open_driver();

    if (*line_color == 0)
    {
	strcpy (line_color, "white");
    }
    else
    {
	if(!D_translate_color (line_color))
	{
	    fprintf (stderr, "%s: %s - color unknown, using white\n",argv[0],
		line_color);
	    strcpy (line_color, "white");
	}
    }
    if (*text_color == 0)
	strcpy (text_color, strcmp (line_color, "white") ? "white" : "red");
    else
    {
	if(!D_translate_color (text_color))
	{
	    fprintf (stderr, "%s: %s - color unknown, ",argv[0], text_color);
	    strcpy (text_color, strcmp (line_color, "white") ? "white" : "red");
	    fprintf (stderr, "using %s\n",text_color);
	}
    }

    setup_plot();
    if (use_mouse)
	mouse ();
    else
	plot (lon1, lat1, lon2, lat2);

    R_close_driver();
}

static
stash(position, value)
    char *value;
{
    switch (position)
    {
    case TEXT_COLOR:
	strcpy (text_color, value);
	return 0;
    case LINE_COLOR:
	strcpy (line_color, value);
	return 0;
    case LON1:
	if (have_lon1++) return 1;
	if (!G_scan_easting (value, &lon1, G_projection())) return 1;
	return 0;
    case LON2:
	if (have_lon2++) return 1;
	if (!G_scan_easting (value, &lon2, G_projection())) return 1;
	return 0;
    case LAT1:
	if (have_lat1++) return 1;
	if (!G_scan_northing (value, &lat1, G_projection())) return 1;
	return 0;
    case LAT2:
	if (have_lat2++) return 1;
	if (!G_scan_northing (value, &lat2, G_projection())) return 1;
	return 0;
    default:
	return 1;
    }
}
