#include "gis.h"

adjust_window (window)
    struct Cell_head *window ;
{
    char *err, *G_adjust_Cell_head();

    if (err = G_adjust_Cell_head(window,0,0))
    {
	char msg[100];
	sprintf (msg, "Invalid window: %s", err);
        G_fatal_error (msg);
    }
}
