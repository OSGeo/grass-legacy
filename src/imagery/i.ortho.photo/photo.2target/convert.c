#include "globals.h"

Compute_ortho_equation()
{
    group.con_equation_stat = I_compute_ortho_equations(&group.control_points,
	&group.cam_info,
        &group.init_info,
        &group.XC, &group.YC, &group.ZC,
        &group.omega, &group.phi, &group.kappa);
}

Compute_ref_equation()
{
    group.ref_equation_stat = I_compute_ref_equations(&group.photo_points,
        group.E12, group.N12, 
        group.E21, group.N21);
}

