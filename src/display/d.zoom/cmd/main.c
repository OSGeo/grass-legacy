/*
 *   d.zoom
 *
 *   Get region through graphics
 */

#include "gis.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    int stat;
    int rotate;
    struct Flag *quiet;
    struct Option *action;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    action = G_define_option();
    action->key = "action";
    action->type = TYPE_STRING;
    action->description = "Type of zoom (for lat/lon databases only)";
    action->options = "zoom,rotate";
    action->required = NO;
    action->answer = NULL; /* do NOT set a default, please */

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet";

    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    R_open_driver();

    D_setup(0);

/* find out for lat/lon if zoom or rotate option */
    rotate = 0;
    if (G_projection() == PROJECTION_LL)
    {
	if (action->answer)
	    rotate = strcmp (action->answer,"rotate") == 0;
	else
	    rotate = ask_rotate();
    }

/* Do the zoom */
    stat = zoom(quiet->answer, rotate) ;

    R_close_driver();

    exit(stat);
}
