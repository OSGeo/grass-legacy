#include "what.h"
#include "display.h"
#include "raster.h"
#include "glocale.h"

void draw_point_plus(int, int, int);

int getArea(tp)
     struct Sql *tp;
{

    int screen_x, screen_y;
    double east, north;
    int button;
    double D_get_d_north(), D_get_d_south();
    double D_get_d_east(), D_get_d_west();
    double D_d_to_u_row(), D_d_to_u_col();
    int projection;
    int D_dist;

    projection = G_projection();

    screen_x = ((int) D_get_d_west() + (int) D_get_d_east()) / 2;
    screen_y = ((int) D_get_d_north() + (int) D_get_d_south()) / 2;



    fprintf(stderr, _("\n\nButtons:\n"));
    fprintf(stderr, _("Left:  Select site for DB query.\n"));
    fprintf(stderr, _("Right: Finish. \n\n\n"));
    R_get_location_with_pointer(&screen_x, &screen_y, &button);

    R_standard_color(D_translate_color("red"));
    draw_point_plus(screen_x, screen_y, 5);
    R_flush();

    east = D_d_to_u_col((double) screen_x);
    north = D_d_to_u_row((double) screen_y);
    tp->centX = east;
    tp->centY = north;
    tp->permX = east + tp->distance;
    tp->permY = north + tp->distance;
    tp->maxX = east + tp->distance;
    tp->maxY = north + tp->distance;
    tp->minX = east - tp->distance;
    tp->minY = north - tp->distance;

    tp->rad2 = ((tp->permX - tp->centX) * (tp->permX - tp->centX) +
		(tp->permY - tp->centY) * (tp->permY - tp->centY));

    D_dist =
	((int) D_u_to_d_col(east + tp->distance) -
	 (int) D_u_to_d_col(east - tp->distance)) / 2;
    R_move_abs(screen_x - D_dist, screen_y - D_dist);
    R_cont_abs(screen_x - D_dist, screen_y + D_dist);
    R_cont_abs(screen_x + D_dist, screen_y + D_dist);
    R_cont_abs(screen_x + D_dist, screen_y - D_dist);
    R_cont_abs(screen_x - D_dist, screen_y - D_dist);
    R_flush();

    return (button);


}

void draw_point_plus(int D_X, int D_Y, int size)
{

    R_move_abs(D_X - size, D_Y);
    R_cont_abs(D_X + size, D_Y);
    R_move_abs(D_X, D_Y - size);
    R_cont_abs(D_X, D_Y + size);
    return;
}
