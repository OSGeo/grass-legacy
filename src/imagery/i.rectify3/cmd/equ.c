#include "global.h"

int Compute_equation (void)
{
/*****************
    group.equation_stat = I_compute_georef_equations(&group.points,
	group.E12, group.N12, group.E21, group.N21);
*****************/
    group.stat = I_compute_georef_equations(&group.points,
	group.coefs);
}

int Compute_fiducial_equation (void)
{
/********
    group.equation_stat_fid = I_compute_fiducial_equations(&group.points_fid,
	group.EF12, group.NF12, group.EF21, group.NF21);
********/
    group.stat = I_compute_fiducial_equations(&group.auxil,
	group.coefs);
}


/*************
CRS_Compute_equation(order)
int order;
{
    group.equation_stat_3d = CRS_compute_CP_georef_equations(&group.points_3d,
	group.E12, group.N12, group.E21, group.N21, order);
}
*************/


int Compute_ortho_equation (void)
{
    double e0,e1,e2,n0,n1,n2,z0,z1,z2;
    int    status, i;
    Control_Points   *points;
    Auxillary_Photo  *auxil;
    Coeffs_Photo     *coefs;

    /* make auxilary visiable */
    points = (Control_Points *) group.points;

    /* make auxilary visiable */
    auxil = (Auxillary_Photo *) group.auxil;

    /* make auxilary visiable */
    coefs = (Coeffs_Photo *) group.coefs;


    /* alloc and fill temp control points */

    auxil->points_photo.count = 0;
    auxil->points_photo.status = NULL;
    auxil->points_photo.e1 = NULL;
    auxil->points_photo.n1 = NULL;
    auxil->points_photo.z1 = NULL;
    auxil->points_photo.e2 = NULL;
    auxil->points_photo.n2 = NULL;
    auxil->points_photo.z2 = NULL;



    /* e0, n0, equal photo coordinates not image coords */
    for (i = 0; i < auxil->points_fid.count; i++)  {
        status = auxil->points_fid.status[i];
        e1 = auxil->points_fid.e1[i];
        n1 = auxil->points_fid.n1[i];
        e2 = auxil->points_fid.e2[i];
        n2 = auxil->points_fid.n2[i];
 
        I_fiducial_ref  (e1, n1, &e0, &n0, coefs);

	/** TODO -- get elevation for e0,n0 target location */

        I_new_con_point (&auxil->points_photo, e0,n0,z1,e2,n2,z2,status);
    }


    group.stat = I_compute_ortho_equations(&auxil->points_photo, 
					   auxil, coefs);

}

