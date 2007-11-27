#include <grass/gis.h>
#include <grass/glocale.h>

int 
adjust_window (struct Cell_head *window)
{
    char *err;

    if ((err = G_adjust_Cell_head3(window,0,0,0)))
    {
        G_fatal_error (_("Invalid region: %s"), err);
    }

    return 0;
}
