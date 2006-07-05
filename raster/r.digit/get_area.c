#include <grass/gis.h>
#include <grass/raster.h>
#include "local_proto.h"

int get_area (FILE *fd, struct Categories *labels)
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
	    R_flush();
	}
	px = x;
	py = y;
	fprintf (fd, " %s %s\n", east, north);
    }
    black_and_white_line (x0, y0, x, y);
    R_flush();
    get_category (fd, "area", labels);

    return any;
}
