#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "global.h"
#include "proto.h"

#define WDTH 5

static struct line_pnts *Points;
static struct line_cats *Cats;
static int first = 1;

int do_move_line(struct Map_info *, plus_t, plus_t, double, double);
int find_closest_point(struct Map_info *, int, double, double, double);

/* Digitize new line */
int new_line(void)
{
    int sxo, syo, sxn, syn;
    int button;
    double x, y;

    G_debug(2, "new_line()");

    if (first) {
	Points = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
	first = 0;
    }

    i_prompt("Digitize new line:");
    i_prompt_buttons("New point", "Back one point", "Quit");
    Vect_reset_line(Points);

    open_driver();
    R_set_update_function(update);

    first = 1;
    while (1) {
	/* Get next coordinate */
	R_set_update_function(update);
	if (first)
	    R_get_location_with_pointer(&sxn, &syn, &button);
	else {
	    sxo = (int) D_u_to_d_col(Points->x[Points->n_points - 1]);
	    syo = (int) D_u_to_d_row(Points->y[Points->n_points - 1]);
	    R_get_location_with_line(sxo, syo, &sxn, &syn, &button);
	}

	x = D_d_to_u_col(sxn);
	y = D_d_to_u_row(syn);
	G_debug(3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn,
		y);
	if (button == 0 || button == 3)
	    break;
	if (button == 2) {
	    if (Points->n_points) {
		display_points(Points, BLACK, 0);
		Points->n_points--;
		if (!Points->n_points)
		    first = 1;
		else
		    display_points(Points, WHITE, 0);
		continue;
	    }
	    else {
		Vect_reset_line(Points);
		first = 1;
		continue;
	    }

	}
	else {
	    /* button == 1 */

	    /* Add to line */
	    Vect_append_point(Points, x, y, 0);

	    display_points(Points, WHITE, 0);
	    first = 0;
	}
    }

    /* Write line */
    if (button == 3 && !first) {
	display_points(Points, WHITE, GREEN);
	Vect_write_line(&Map, GV_LINE, Points, Cats);
    }
    else {
	display_points(Points, BLACK, 0);
    }

    close_driver();

    i_prompt("");
    i_prompt_buttons("", "", "");

    G_debug(3, "new_line(): End");

    return 1;
}


/* process line */
int process_line(int type_of_process)
{

    int row, col;
    int nrows, ncols;
    int screen_x, screen_y;
    double east, north, east_sel = 0.0, north_sel = 0.0;
    int button;
    double x1, y1, x2, y2;
    double maxdist;

    plus_t line, line_sel = 0, node = 0, node_sel = 0;


    switch (type_of_process) {
    case PROCESS_DELETE:
	G_debug(2, "delete_line()");

	i_prompt("Delete line:");
	i_prompt_buttons("Select line", "Delete", "Cancel");
	break;

    case PROCESS_MOVE:
	G_debug(2, "move_line()");

	i_prompt("Move line:");
	i_prompt_buttons("Select line", "Move", "Cancel");
	break;

    case PROCESS_NODE:
	G_debug(2, "move_node()");

	i_prompt("Move node/vertex:");
	i_prompt_buttons("Select", "Move", "Cancel");
	break;

    default:
	return -1;
	break;
    }

    open_driver();
    R_set_update_function(update);

    nrows = Region.rows;
    ncols = Region.cols;

    screen_x = ((int) D_get_d_west() + (int) D_get_d_east()) / 2;
    screen_y = ((int) D_get_d_north() + (int) D_get_d_south()) / 2;


    while (1) {

	R_set_update_function(update);
	R_get_location_with_pointer(&screen_x, &screen_y, &button);

	if (button == 0 || button == 3) {
	    if (line_sel)
		display_line(line_sel, WHITE, GREEN);
	    break;
	}

	east = D_d_to_u_col((double) screen_x);
	north = D_d_to_u_row((double) screen_y);

	if (button == 2 && (line_sel || node_sel)) {

	    switch (type_of_process) {
	    case PROCESS_DELETE:
		if (line_sel) {
		    display_line(line_sel, BLACK, BLACK);
		    Vect_delete_line(&Map, line_sel);
		}
		break;

	    case PROCESS_MOVE:
		if (line_sel)
		    do_move_line(&Map, 0, line_sel, east - east_sel,
				 north - north_sel);
		break;

	    case PROCESS_NODE:
		if (node_sel && line_sel)
		    do_move_line(&Map, node_sel, line_sel, east - east_sel,
				 north - north_sel);
		break;
	    
	    default:
		break;
	    }

	    line_sel = 0;
	    node_sel = 0;
	    continue;
	}

	row = (Region.north - north) / Region.ns_res;
	col = (east - Region.west) / Region.ew_res;

	if (row < 0 || row >= nrows)
	    continue;
	if (col < 0 || col >= ncols)
	    continue;

	x1 = D_d_to_u_col((double) (screen_x - WDTH));
	y1 = D_d_to_u_row((double) (screen_y - WDTH));
	x2 = D_d_to_u_col((double) (screen_x + WDTH));
	y2 = D_d_to_u_row((double) (screen_y + WDTH));

	x1 = fabs(x2 - x1);
	y1 = fabs(y2 - y1);

	if (x1 > y1)
	    maxdist = x1;
	else
	    maxdist = y1;

	line = Vect_find_line(&Map, east, north, 0,
			      GV_POINT | GV_CENTROID, maxdist, 0, 0);
	if (line == 0)
	    line = Vect_find_line(&Map, east, north, 0,
				  GV_LINE | GV_BOUNDARY, maxdist, 0, 0);
	if ((type_of_process == PROCESS_NODE) && line)
	    node = find_closest_point(&Map, line, east, north, maxdist);


	if (line_sel)
	    display_line(line_sel, WHITE, GREEN);

	if (line || node) {
	    if (line)
		display_line(line, YELLOW, RED);
	    east_sel = east;
	    north_sel = north;
	}

	line_sel = line;
	node_sel = node;
    }


    close_driver();

    switch (type_of_process) {
    case PROCESS_DELETE:

	G_debug(3, "delete_line(): End");
	break;

    case PROCESS_MOVE:

	G_debug(3, "move_line(): End");
	break;

    case PROCESS_NODE:

	G_debug(3, "move_node(): End");
	break;

    default:
	break;
    }

    i_prompt("");
    i_prompt_buttons("", "", "");

    return 1;
}


/* workhorse called by process_line*/
int do_move_line(struct Map_info *Map, plus_t node, plus_t line,
		 double xoffset, double yoffset)
{

    int i, rcode = 0;
    int type;

    if (first) {
	Points = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
	first = 0;
    }
    if (!Vect_line_alive(Map, line))
	return 0;

    G_debug(3, "do_move_line()");

    Vect_reset_line(Points);
    type = Vect_read_line(Map, Points, Cats, line);

    display_points(Points, BLACK, BLACK);

    if (node) {
	Points->x[node - 1] += xoffset;
	Points->y[node - 1] += yoffset;
    }
    else {
	for (i = 0; i < Points->n_points; i++) {
	    Points->x[i] += xoffset;
	    Points->y[i] += yoffset;
	}
    }

    if ((rcode = Vect_rewrite_line(Map, line, type, Points, Cats)) != -1) {

	display_points(Points, WHITE, GREEN);
	return 1;
    }
    else
	return 0;

}

/* finds a point in line closest in (x,y)-coords to given (east,west) - when we need not z */
int find_closest_point(struct Map_info *Map, int line, double east,
		       double north, double maxdist)
{
    int i, rcode = 0;
    int type;
    double current_dist, closest_dist, dx, dy;

    closest_dist = maxdist;

    if (first) {
	Points = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
	first = 0;
    }
    if (!Vect_line_alive(Map, line))
	return 0;

    G_debug(3, "do_move_line()");

    Vect_reset_line(Points);
    type = Vect_read_line(Map, Points, Cats, line);

    for (i = 0; i < Points->n_points; i++) {
	if ((Points->x[i] < east - maxdist)
	    || (Points->x[i] > east + maxdist))
	    continue;
	if ((Points->y[i] < north - maxdist)
	    || (Points->y[i] > north + maxdist))
	    continue;

	dx = Points->x[i] - east;
	dy = Points->y[i] - north;
	current_dist = hypot(dx, dy);
	if (current_dist <= closest_dist) {
	    closest_dist = current_dist;
	    rcode = i + 1;
	}
    }

    /* if (rcode) display_point(Points->x[rcode-1], Points->y[rcode-1], RED, 2); */

    return rcode;
}
