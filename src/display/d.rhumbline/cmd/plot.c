#include <stdio.h>

extern double D_get_d_north(), D_get_d_south() ;
extern double D_get_d_east(), D_get_d_west() ;
extern double D_d_to_u_row(), D_d_to_u_col() ;
extern double D_u_to_d_row(), D_u_to_d_col() ;


extern double fabs();

extern double G_rhumbline_lat_from_lon();
/*
extern double G_rhumbline_distance();
*/

#define METERS_TO_MILES(x) ((x) * 6.213712e-04)

static int min_range[5], max_range[5];
static int which_range;
static int change_range;

int cont();
int move();

setup_plot()
{

/* establish the current graphics window */
    D_setup(0);

/* set D clip window */
    D_set_clip_window ( (int)D_get_d_north(),
			(int)D_get_d_south(),
			(int)D_get_d_west(),
			(int)D_get_d_east());

/* setup the G plot to use the D routines */
    G_setup_plot ( D_get_d_north(),
		   D_get_d_south(),
		   D_get_d_west(),
		   D_get_d_east(),
		   move, cont);

    /*
    G_get_ellipsoid_parameters (&a, &e2);
    G_begin_rhumbline_distance (a, e2);
    */
    R_text_size (10, 10);
}

plot (lon1,lat1,lon2,lat2,line_color,text_color)
    double lat1, lon1, lat2, lon2;
{
    double distance;
    char buf[100];
    int text_x, text_y;

    which_range = -1;
    change_range = 1;
    R_standard_color (line_color);
    if (lon1 != lon2)
    {
	G_shortest_way (&lon1, &lon2);
	G_begin_rhumbline_equation (lon1,lat1,lon2,lat2);
	G_plot_fx(G_rhumbline_lat_from_lon, lon1, lon2);
	/*
	text_x = get_text_x();
	if (text_x >= 0)
	    text_y = rhumbline(text_x);
	*/
    }
    else
    {
	G_plot_where_xy (lon1, (lat1+lat2)/2, &text_x, &text_y);
	G_plot_line (lon1, lat1, lon2, lat2);
    }
    R_flush();
/*
    distance = G_rhumbline_distance (lon1, lat1, lon2, lat2);
    sprintf (buf, "%.0lf miles\n", METERS_TO_MILES(distance));
    if (text_x >= 0)
    {
	if (text_y + 10 <= D_get_d_north())
	    text_y = D_get_d_north() - 10;
	if (text_x + 10*strlen(buf) >= D_get_d_east())
	    text_x = D_get_d_east() - 10*strlen(buf);
	R_move_abs (text_x, text_y);
	R_standard_color (text_color);
	R_text (buf);
	R_flush();
    }
*/
}

static
cont(x,y)
{
    if(D_cont_abs (x, y)) /* clipped */
    {
	change_range = 1;
    }
    else  /* keep track of left,right x for lines drawn in window */
    {
	if (change_range)
	{
	    which_range++;
	    min_range[which_range] = max_range[which_range] = x;
	    change_range = 0;
	}
	else
	{
	    if (x < min_range[which_range])
		min_range[which_range] = x;
	    else if (x > max_range[which_range])
		max_range[which_range] = x;
	}
    }
}

static
move(x,y)
{
    D_move_abs(x,y);
}

static
rhumbline(x)
{
    double lon, lat;
    lon = D_d_to_u_col((double)x) ;
    lat = G_rhumbline_lat_from_lon (lon);
    return (int) D_u_to_d_row (lat);
}

static
get_text_x()
{
    int n;
    int len;
    int max;
    int which;

    which = -1;
    max = 0;
    for (n = 0; n <= which_range; n++)
    {
	len = max_range[n] - min_range[n];
	if (len >= max)
	{
	    max = len;
	    which = n;
	}
    }
    if (which < 0) return -1;

    return (max_range[which] + min_range[which])/2;
}
