/*	flash.c
	 - to draw areas and lines in toggled color just to be redrawn 
	in the calling routine back to the previous color 
	making quick 'flash' --alex, nov/02
*/
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "what.h"

void
flash_area(struct Map_info *Map, plus_t area, struct line_pnts *Points,
	   int flash_colr)
{
    struct line_pnts *Points_i;

    P_AREA *pa;
    double **xs, **ys;
    int rings;
    int *rpnts;
    int j, jk;


    Points_i = Vect_new_line_struct();

    V2_get_area(Map, area, &pa);
    rings = 1 + pa->n_isles;
    xs = (double **) G_malloc(sizeof(double *) * rings);
    ys = (double **) G_malloc(sizeof(double *) * rings);
    rpnts = (int *) G_malloc(sizeof(int) * rings);

    rpnts[0] = Points->n_points;
    xs[0] = (double *) G_malloc(sizeof(double) * rpnts[0]);
    ys[0] = (double *) G_malloc(sizeof(double) * rpnts[0]);
    Vect_copy_pnts_to_xy(Points, xs[0], ys[0], &rpnts[0]);


    R_standard_color(flash_colr);

    for (j = 0; j < Points->n_points - 1; j++)
	G_plot_line(Points->x[j], Points->y[j],
		    Points->x[j + 1], Points->y[j + 1]);

    for (j = 0; j < pa->n_isles; j++) {

	Vect_get_isle_points(Map, pa->isles[j], Points_i);
	rpnts[j + 1] = Points_i->n_points;
	xs[j + 1] = (double *) G_malloc(sizeof(double) * rpnts[j + 1]);
	ys[j + 1] = (double *) G_malloc(sizeof(double) * rpnts[j + 1]);
	Vect_copy_pnts_to_xy(Points_i, xs[j + 1], ys[j + 1], &rpnts[j + 1]);

	for (jk = 0; jk < Points_i->n_points - 1; jk++)
	    G_plot_line(Points_i->x[jk], Points_i->y[jk],
			Points_i->x[jk + 1], Points_i->y[jk + 1]);

    }

    G_plot_area(xs, ys, rpnts, rings);

    for (j = 0; j < rings; j++) {
	free(xs[j]);
	free(ys[j]);
    }

    free(xs);
    free(ys);
    free(rpnts);

    Vect_destroy_line_struct(Points_i);
    R_flush();


}

void
flash_line(struct Map_info *Map, plus_t line, struct line_pnts *Points,
	   int flash_colr)
{
    double *x, *y;
    int j, ret, np;


    if (0 > (ret = V2_read_line(Map, Points, line))) {
	if (ret == -2)
	    G_warning("Read error\n");
	return;
    }
    np = Points->n_points;
    x = Points->x;
    y = Points->y;

    R_standard_color(flash_colr);

    for (j = 1; j < np; j++) {

	G_plot_line(x[0], y[0], x[1], y[1]);
	x++;
	y++;
    }

    R_flush();
}
