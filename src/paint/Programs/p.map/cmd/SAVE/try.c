#include "gis.h"
main(argc,argv)
	char **argv ;
{
    struct Cell_head window;

    G_gisinit(argv[0]) ;

    G_get_window (&window);
    distance (window.east, window.west);
}
