#include "global.h"
get_control_points (group)
    char *group;
{
    char msg[200];
    struct Control_Points cp;

    if(!I_get_control_points (group, &cp))
	exit(0);

    sprintf (msg, "Control Point file for group [%s] in [%s] - ",
	group,G_mapset());

    switch (I_compute_georef_equations (&cp,E12,N12,E21,N21))
    {
    case -1:
	strcat (msg,"Poorly placed control points.");
	strcat (msg, " Can not generate the transformation equation.");
	break;
    case 0:
	strcat (msg, "No active control points");
	break;
    default:
	E12a = E12[0]; E12b = E12[1]; E12c = E12[2];
	N12a = N12[0]; N12b = N12[1]; N12c = N12[2];
	E21a = E21[0]; E21b = E21[1]; E21c = E21[2];
	N21a = N21[0]; N21b = N21[1]; N21c = N21[2];
	return 1;
    }
    G_fatal_error (msg);
    exit(1);
}
