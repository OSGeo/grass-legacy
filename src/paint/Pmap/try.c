#include "gis.h"
main()
{
    struct Cell_head window;

    G_gisinit("");

    G_get_window (&window);
    distance (window.east, window.west);
}
