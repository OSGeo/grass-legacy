#include <stdlib.h>
#include <string.h>
#include <grass/glocale.h>
#include "global.h"

int get_psuedo_control_pt(int tie_row, int tie_col)
{
    char msg[200];
    struct Ortho_Photo_Points ps_cp;
    int i, j, k;

    G_debug(1, "In ps_cp");

    /*  allocate psuedo struct, max points are four */
    ps_cp.count = 4;
    ps_cp.e1 = (double *)G_malloc(4 * sizeof(double));
    ps_cp.n1 = (double *)G_malloc(4 * sizeof(double));
    ps_cp.e2 = (double *)G_malloc(4 * sizeof(double));
    ps_cp.n2 = (double *)G_malloc(4 * sizeof(double));
    ps_cp.status = (int *)G_malloc(4 * sizeof(int));
    G_debug(1, "ps_cp allocated");

    /*  pseudo points are four corners taken from T_Points */
    k = 0;
    for (i = 0; i < 2; i++) {
	for (j = 0; j < 2; j++) {
	    ps_cp.e1[k] = T_Point[tie_row + i][tie_col + j].xt;
	    ps_cp.n1[k] = T_Point[tie_row + i][tie_col + j].yt;
	    ps_cp.e2[k] = (j * ((T_Point[tie_row][tie_col + 1].XT -
				 T_Point[tie_row][tie_col].XT) /
				target_window.ew_res));
	    ps_cp.n2[k] =
		(i *
		 ((T_Point[tie_row][tie_col].YT -
		   T_Point[tie_row + 1][tie_col].YT) / target_window.ns_res));
	    ps_cp.status[k] = 1;

	    G_debug(2, "\t k = %d\t i = %d\t j = %d", k, i, j);
	    G_debug(2, "\t\t e1[k] = %f", ps_cp.e1[k]);
	    G_debug(2, "\t\t n1[k] = %f", ps_cp.n1[k]);
	    G_debug(2, "\t\t e2[k] = %f", ps_cp.e2[k]);
	    G_debug(2, "\t\t n2[k] = %f", ps_cp.n2[k]);

	    k++;
	}
    }

    G_debug(1, "ps_cp initialized");

    switch (I_compute_ref_equations(&ps_cp, E12, N12, E21, N21)) {
    case -1:
	G_debug(1, "\tref_equ: case -1");
	strcat(msg, _("Poorly placed psuedo control points. "));
	strcat(msg, _("Cannot generate the transformation equation."));
	break;
    case 0:
	G_debug(1, "\tref_equ: case 0");
	strcat(msg, _("No active psuedo control points"));
	break;
    default:
	G_debug(1, "\tref equ: case good");

	E12a = E12[0];
	E12b = E12[1];
	E12c = E12[2];
	N12a = N12[0];
	N12b = N12[1];
	N12c = N12[2];
	E21a = E21[0];
	E21b = E21[1];
	E21c = E21[2];
	N21a = N21[0];
	N21b = N21[1];
	N21c = N21[2];

	G_debug(1, "\t\tE21 = %f\t %f\t %f", E21a, E21b, E21c);
	G_debug(1, "\t\tN21 = %f\t %f\t %f", N21a, N21b, N21c);

	return 1;
    }
    G_fatal_error(msg);
}
