#include <stdlib.h>
#include "gis.h"
#include "dlg.h"
#include "Vect.h"

#define METERS_PER_INCH	0.0254

double greater ();
double lesser ();

hd_dlg_to_dig (dlg, dig)
    struct dlg *dlg;
    struct dig_head *dig;
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

}

char *
G_strncpy (T, F, n)
    register char *T, *F;
    register int n;
{
    register char *d = T;

    while (n-- && *F)
        *d++ = *F++;
    *d = '\0';
    return (T);
}

double
greater (a, b)
    double a;
    double b;
{
    return (a > b ? a : b);
}

double
lesser (a, b)
    double a;
    double b;
{
    return (a < b ? a : b);
}
