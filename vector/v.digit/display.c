#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "global.h"
#include "proto.h"


/* Display points */
int display_points(struct line_pnts *Points, int color, int ends_color)
{
    int i;

    G_debug(2, "display_points");

    R_color(color);

    for (i = 1; i < Points->n_points; i++) {
	G_plot_line(Points->x[i - 1], Points->y[i - 1], Points->x[i],
		    Points->y[i]);
    }

    if (ends_color != 0) {
	R_color(ends_color);
	blot_point(Points->x, Points->y, 0);
	blot_point(Points->x + Points->n_points - 1,
		   Points->y + Points->n_points - 1, 0);
    }

    R_flush();

    return 1;
}

/* Display point */
int display_point(double xcoor, double ycoor, int node_color, int size)
{
    G_debug(3, "display_point()");

    R_color(node_color);
    blot_point(&xcoor, &ycoor, size);

    return 1;
}

/* Display vector line */
int display_line(int line, int line_color, int ends_color)
{
    int type;
    static struct line_pnts *Points;
    static struct line_cats *Cats;
    static int first = 1;

    G_debug(2, "display_line()");

    if (first) {
	Points = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
	first = 0;
    }

    if (!Vect_line_alive(&Map, line))
	return 0;

    type = Vect_read_line(&Map, Points, Cats, line);

    display_points(Points, line_color, ends_color);

    return 1;
}

/* Display vector map */
int display_map(void)
{
    int i, n;

    G_debug(2, "display_map()");

    n = Vect_get_num_lines(&Map);
    for (i = 1; i <= n; i++) {
	display_line(i, WHITE, GREEN);
    }

    return 1;
}

/* Draw a (size+1)-pixel box at point*/
int blot_point(double *x, double *y, int size)
{
    int xpos, ypos;
    int top, bot, left, right;
    double Xs, Ys;

    size = abs(size);
    ++size;

    G_debug(3, "blot_point(): at %f, %f", *x, *y);

    D_get_screen_window(&top, &bot, &left, &right);

    if (*x < D_d_to_u_col(left))
	return 0;
    else if (*x > D_d_to_u_col(right))
	return 0;
    else if (*y < D_d_to_u_row(bot))
	return 0;
    else if (*y > D_d_to_u_row(top))
	return 0;
    else {

	Xs = (D_d_to_u_col(right) - D_d_to_u_col(left)) / (right - left);
	Ys = (D_d_to_u_row(top) - D_d_to_u_row(bot)) / (top - bot);

	xpos = (int) ((*x - D_d_to_u_col(left)) / Xs + left);
	ypos = (int) ((*y - D_d_to_u_row(bot)) / Ys + bot);

	D_move_abs(xpos, ypos);
	D_cont_abs(xpos, ypos - size);
	D_cont_abs(xpos - size, ypos);
	D_cont_abs(xpos, ypos + size);
	D_cont_abs(xpos + size, ypos);

    }
    return 0;
}
