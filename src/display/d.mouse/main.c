#include "gis.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    struct Option *anchor, *drag;
    double east0,north0;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    anchor = G_define_option();
    anchor->key = "anchor";
    anchor->key_desc = "east,north";
    anchor->type = TYPE_STRING;
    anchor->required = NO;
    anchor->description = "Anchor Point";

    drag = G_define_option();
    drag->key = "drag";
    drag->type = TYPE_STRING;
    drag->required = NO;
    drag->options = "box,line,none";
    drag->answer = "none";
    drag->description = "drag method";

    if (argc > 1)
    {
	if (G_parser(argc, argv))
		exit(1);
    }
    if (anchor->answer)
    {

	if (!G_scan_easting (anchor->answers[0], &east0, G_projection()))
	{
	    fprintf (stderr, "%s - illegal value for easting", anchor->answers[0]);
	    G_usage();
	    exit(1);
	}
	if (!G_scan_northing (anchor->answers[1], &north0, G_projection()))
	{
	    fprintf (stderr, "%s - illegal value for northing", anchor->answers[1]);
	    G_usage();
	    exit(1);
	}
    }
    else
    {
	east0 = north0 = 0.0;
    }

    if (R_open_driver() != 0)
	    G_fatal_error ("No graphics device selected");
    setup();
    where(east0, north0, drag->answer);
    R_close_driver();

    exit(0);
}
