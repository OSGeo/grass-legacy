#include "gis.h"
get_area (fd, labels)
    FILE *fd;
    struct Categories *labels;
{
    int x, y;
    int px, py;
    int x0,y0;
    int any;
    char east[256], north[256];

    instructions(0);
    x = y = -9999;
    any = 0;
    while (get_point (&x, &y, east, north))
    {
	if (!any)
	{
	    fprintf (fd, "AREA\n");
	    any = 1;
	    x0 = x;
	    y0 = y;
	}
	else
	{
	    black_and_white_line (px, py, x, y);
	}
	px = x;
	py = y;
	fprintf (fd, " %s %s\n", east, north);
    }
    black_and_white_line (x0, y0, x, y);
    get_category (fd, "area", labels);
    return any;
}

