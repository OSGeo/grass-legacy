#include "gis.h"
#include <signal.h>
init()
{
    char *getenv();

    signal (SIGINT, SIG_IGN);

    if (getenv("NO_GRAPHICS")) return;

    if(G__getenv ("MONITOR") == NULL)
	choose_mon();

/* make sure we can run driver */
    R_open_driver();
    R_close_driver();

    set_colormode ("fixed");
    system ("Dscreen");
    set_background_color ("black");
    erase();
    basemap();
}
