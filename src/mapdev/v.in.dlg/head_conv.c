#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "dlg.h"
#include "Vect.h"

#define METERS_PER_INCH	0.0254

static double greater (double ,double);
static double lesser (double,double);

int hd_dlg_to_dig (
    struct dlg *dlg,
    struct dig_head *dig)
{
	G_strncpy (dig->organization, dlg->head.banner, 29);
	G_strncpy (dig->date, dlg->head.source_date, 19);
	G_strncpy (dig->your_name, "Import from DLG", 19);
	G_strncpy (dig->map_name, dlg->head.cart_unit, 40);
	G_strncpy (dig->source_date, dlg->head.source_date, 10);
	dig->orig_scale = atol (dlg->head.orig_scale);
	G_strncpy (dig->line_3, dlg->head.line_3, 72);
	dig->plani_zone = dlg->head.plani_zone;
	dig->N = greater (dlg->coors.utm_n[NW], dlg->coors.utm_n[NE]);
	dig->E = greater (dlg->coors.utm_e[SE], dlg->coors.utm_e[NE]);
	dig->W = lesser  (dlg->coors.utm_e[NW], dlg->coors.utm_e[SW]);
	dig->S = lesser  (dlg->coors.utm_n[SE], dlg->coors.utm_n[SW]);
	dig->digit_thresh = 0.001;	/* hard coding... */
	dig->map_thresh = 0.001 * atol (dlg->head.orig_scale);
		/* * METERS_PER_INCH; */

	return 0;
}

static double greater (
    double a,
    double b)
{
    return (a > b ? a : b);
}

static double lesser (
    double a,
    double b)
{
    return (a < b ? a : b);
}
