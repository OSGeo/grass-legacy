#include "gis.h"
#include "options.h"

char *fgets();

draw_points_diamond(size)
{
    int x, y;

    while(next_point(&x, &y))
    {
	D_move_abs(x     , y+size) ;
	D_cont_abs(x+size, y     ) ;
	D_cont_abs(x     , y-size) ;
	D_cont_abs(x-size, y     ) ;
	D_cont_abs(x     , y+size) ;
    }
}

draw_points_box(size)
{
    int x, y;

    while(next_point(&x, &y))
    {
	D_move_abs(x-size, y-size) ;
	D_cont_abs(x-size, y+size) ;
	D_cont_abs(x+size, y+size) ;
	D_cont_abs(x+size, y-size) ;
	D_cont_abs(x-size, y-size) ;
    }
}

draw_points_plus(size)
{
    int x, y;

    while(next_point(&x, &y))
    {
	D_move_abs(x-size, y     ) ;
	D_cont_abs(x+size, y     ) ;
	D_move_abs(x     , y-size) ;
	D_cont_abs(x     , y+size) ;
    }
}

draw_points_x(size)
{
    int x, y;

    while(next_point(&x, &y))
    {
	D_move_abs(x-size, y-size) ;
	D_cont_abs(x+size, y+size) ;
	D_move_abs(x+size, y-size) ;
	D_cont_abs(x-size, y+size) ;
    }
}

next_point (x,y)
    int *x, *y;
{
    char buffer[128];
    char east[100], north[100];
    double e,n;

    for(;;)
    {
	if(!fgets(buffer, sizeof buffer, infile)) return 0;
	if (sscanf (buffer, "%s %s", east,north) != 2) continue;
	if (!G_scan_easting (east, &e, G_projection())) continue;
	if (!G_scan_northing (north, &n, G_projection())) continue;

	G_plot_where_xy (e, n, x, y);
	break;
    }

    return 1;
}
