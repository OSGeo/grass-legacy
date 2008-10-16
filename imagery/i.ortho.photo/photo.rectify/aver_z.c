#include "global.h"

int get_aver_elev(struct Ortho_Control_Points *cpz, double *aver_z)
{
    double meanz;
    int i, n;

    /*  Need 1 control points */
    if (cpz->count <= 0) {
	return (-1);
    }

    /* set average elevation from mean values of CONZ points */
    meanz = 0;
    n = 0;
    for (i = 0; i < cpz->count; i++) {
	if (cpz->status[i] <= 0)
	    continue;

	n++;
	meanz += *((cpz->z2)++);
	G_debug(3, "In ortho meanz = %f", meanz);
    }

    *aver_z = meanz / n;

    G_debug(1, "In ortho aver_z = %f", *aver_z);

    /* reset pointers */
    for (i = 0; i < cpz->count; i++) {
	if (cpz->status[i] <= 0)
	    continue;
	(cpz->z2)--;
    }

    return 0;
}
