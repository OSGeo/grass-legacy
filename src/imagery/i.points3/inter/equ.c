#include "globals.h"

Compute_equation()
{
/**
    group.equation_stat = I_compute_georef_equations(&group.points,
	group.E12, group.N12, group.E21, group.N21);
**/
}

Compute_fiducial_equation()
{
/**
    group.equation_stat_fid = I_compute_fiducial_equations(&group.points_fid,
	group.EF12, group.NF12, group.EF21, group.NF21);
**/
}

CRS_Compute_equation(order)
int order;
{
/**
    group.equation_stat_3d = CRS_compute_georef_equations(&group.points_3d,
	group.E12, group.N12, group.E21, group.N21, order);
**/
}

Compute_ortho_equation()
{
/**    double e0,e1,e2,n0,n1,n2,z0,z1,z2;
/**    int    status, i;
/**
/**    /* alloc and fill temp control points */
/**    group.temp_points.count = 0;
/**    group.temp_points.status = NULL;
/**    group.temp_points.e1 = NULL;
/**    group.temp_points.n1 = NULL;
/**    group.temp_points.z1 = NULL;
/**    group.temp_points.e2 = NULL;
/**    group.temp_points.n2 = NULL;
/**    group.temp_points.z2 = NULL;
/**
/**    /* e0, n0, equal photo coordinates not image coords */
/**    for (i = 0; i < group.points_3d.count; i++)  {
/**        status = group.points_3d.status[i];
/**        e1 = group.points_3d.e1[i];
/**        n1 = group.points_3d.n1[i];
/**        z1 = group.points_3d.z1[i];
/**        e2 = group.points_3d.e2[i];
/**        n2 = group.points_3d.n2[i];
/**        z2 = group.points_3d.z2[i];
/** 
/**        I_georef  (e1,n1,&e0, &n0, group.EF12, group.NF12);
/**        I_new_con_point (&group.temp_points, e0,n0,z1,e2,n2,z2,status);
/**    }
/**
/**
/**    group.equation_stat_3d = I_compute_ortho_equations(&group.temp_points,
/**	&group.camera,
/**        &group.camera_expose,
/**        &group.XC, &group.YC, &group.ZC,
/**        &group.omega, &group.phi, &group.kappa);

**************************************************************************/
}

