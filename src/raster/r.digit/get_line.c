#include "gis.h"
get_line (fd, labels)
    FILE *fd;
    struct Categories *labels;
{
    int x, y;
    int px, py;
    int any;
    char east[256], north[256];

    instructions(0);
    x = y = -9999;
    any = 0;
    while (get_point (&x, &y, east, north))
    {
	if (!any)
	{
	    fprintf (fd, "LINE\n");
	    any = 1;
	}
	else
	{
	    black_and_white_line (px, py, x, y);
	}
	px = x;
	py = y;
	fprintf (fd, " %s %s\n", east, north);
    }
    get_category (fd, "line", labels);
    return any;
}
