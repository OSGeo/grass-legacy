#include "gis.h"

make_window(window)
    struct Cell_head *window ;
{
    int box;
    char buf[128];

    box = 1;

    if (window->proj == PROJECTION_LL)
    {
	while (1)
	{
	    fprintf (stderr, "\n\nSelect the window method\n\n");
	    fprintf (stderr, "  1. Zoom in using a rubberband box\n");
	    fprintf (stderr, "  2. Rotate the world by selecting the center longitude\n");
	    fprintf (stderr, "\n> ");
	    if (!G_gets(buf)) continue;
	    G_strip (buf);
	    if (strcmp (buf, "1") == 0)
		box = 1;
	    else if (strcmp (buf, "2") == 0)
		box = 0;
	    else
		continue;
	    break;
	}
    }

    if (box)
	return make_window_box (window);
    else
	return make_window_center (window);
}
