#include <unistd.h>
#include "globals.h"
#include "raster.h"
#include "local_proto.h"

int draw_points_diamond (void)
{
    int x, y;

    while(next_point(&x, &y))
    {
	R_move_abs(x     , y+size) ;
	R_cont_abs(x+size, y     ) ;
	R_cont_abs(x     , y-size) ;
	R_cont_abs(x-size, y     ) ;
	R_cont_abs(x     , y+size) ;
    }

    return 0;
}

int draw_points_box (void)
{
    int x, y ;

    while(next_point(&x, &y))
    {
	R_move_abs(x-size, y-size) ;
	R_cont_abs(x-size, y+size) ;
	R_cont_abs(x+size, y+size) ;
	R_cont_abs(x+size, y-size) ;
	R_cont_abs(x-size, y-size) ;
    }

    return 0;
}

int draw_points_plus (void)
{
    int x, y ;

    while(next_point(&x, &y))
    {
	R_move_abs(x-size, y     ) ;
	R_cont_abs(x+size, y     ) ;
	R_move_abs(x     , y-size) ;
	R_cont_abs(x     , y+size) ;
    }

    return 0;
}

int draw_points_x (void)
{
    int x, y ;

    while(next_point(&x, &y))
    {
	R_move_abs(x-size, y-size) ;
	R_cont_abs(x+size, y+size) ;
	R_move_abs(x+size, y-size) ;
	R_cont_abs(x-size, y+size) ;
    }

    return 0;
}

int next_point (int *x, int *y)
{
    char buf[1024];
    double east, north;
    char ebuf[128], nbuf[128];
    char label[512];

    while(1)
    {
	*label = 0;
	if (isatty(fileno(infile)))
	    fprintf (stderr, "east north [label] >  ");
	if(!fgets(buf, sizeof buf, infile)) return 0;

	if (sscanf (buf,"%s %s %[^\n]", ebuf, nbuf, label) < 2)
	    continue;
	if (!G_scan_easting (ebuf, &east, G_projection()) )
	    continue;
	if (!G_scan_northing (nbuf, &north, G_projection()) )
	    continue;

	if (east  >= region.west  && east  <= region.east
	&&  north >= region.south && north <= region.north)
	    break;
    }

    G_plot_where_xy (east, north, x, y);
    R_move_abs (*x+size+2 + size/3, *y);
    R_text (label);

    return 1;
}
