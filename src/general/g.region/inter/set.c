#include "glob.h"
set_window (window, name)
    struct Cell_head *window;
    char * name;
{
    if(G_put_window (window) < 0)
	fprintf (stderr, "** unable to write current region. ");
    else
    {
	G_copy (&cur_window, window, sizeof cur_window);
	fprintf (stderr, "current region set from [%s]. ", name);
    }
}
