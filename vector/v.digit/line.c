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

/* Delete line */
int delete_line(void)
{

    int row, col;
    int nrows, ncols;
    int screen_x, screen_y;
    double east, north;
    int button;
    double x1, y1, x2, y2;
    double maxdist;

    plus_t line, line_sel = 0;

    G_debug(2, "delete_line()");

    i_prompt("Delete line:");
    i_prompt_buttons("Select line", "Delete", "Cancel");

    open_driver();
    R_set_update_function(update);

    nrows = Region.rows;
    ncols = Region.cols;

    screen_x = ((int) D_get_d_west() + (int) D_get_d_east()) / 2;
    screen_y = ((int) D_get_d_north() + (int) D_get_d_south()) / 2;


    while (1) {

	R_set_update_function(update);
	R_get_location_with_pointer(&screen_x, &screen_y, &button);

	if (button == 2 && line_sel) {
	    display_line(line_sel, BLACK, BLACK);
	    Vect_delete_line(&Map, line_sel);
	    line_sel = 0;
	    continue;
	}

	if (button == 0 || button == 3) {
	    if (line_sel)
		display_line(line_sel, WHITE, GREEN);
	    break;
	}

	east = D_d_to_u_col((double) screen_x);
	north = D_d_to_u_row((double) screen_y);

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

	if (line_sel)
	    display_line(line_sel, WHITE, GREEN);
	if (line)
	    display_line(line, YELLOW, RED);
	line_sel = line;
    }


    close_driver();

    i_prompt("");
    i_prompt_buttons("", "", "");

    G_debug(3, "delete_line(): End");

    return 1;
}
