/*======================================================================
                             poly_trans.c


poly_trans_calculate(group)

poly_trans_forward (group, e1, n1, z1, e2, n2, z2)
  Rectify_Group    *group;
  double            e1,  n1,  z1;
  double           *e2, *n2, *z2;

poly_trans_inverse (group, e1, n1, z1, e2, n2, z2)
  Rectify_Group    *group;
  double           *e1, *n1, *z1;
  double            e2,  n2,  z2;


======================================================================*/

#include "ortho_image.h"


/*-------------------------------------------------------------------*/
poly_trans_calculate(group)
Rectify_Group *group;
{
    int    i;
    int    status;

    Control_Points     *points;           
    Coeffs_Poly     *coefs;


    /* check the transformation type */
    if ((group->trans_type < POLY1) || (group->trans_type > POLY3)) {
      /* TODO warning mes */
      return (-1);
    }

    /* check the initial status */
    if (group->stat < 0) {
      /* TODO warning mes */
      return (-1);
    }


    /* make control points visiable */
    points = (Control_Points *) group->points;

    /* make coeffients visiable */
    coefs = (Coeffs_Poly *) group->coefs;

    /* calculate the polynomial transform coeffients */
    group->stat = 
      CRS_compute_georef_equations (&points->points_temp, 
				    &coefs->E12[0], &coefs->N12[0],
				    &coefs->E21[0], &coefs->N21[0],
				    (int) group->trans_type);

    return (group->stat);
}


/*-------------------------------------------------------------------*/
poly_trans_forward (group, e1, n1, z1, e2, n2, z2)
Rectify_Group    *group;
double            e1,  n1,  z1;
double           *e2, *n2, *z2;
{
char           msg[100];

    Control_Points     *points;           
    Coeffs_Poly     *coefs;


    /* check the transformation type */
    if ((group->trans_type < POLY1) || (group->trans_type > POLY3)) {
      /* TODO warning mes */
      return (-1);
    }

    /* make control points visiable */
    points = (Control_Points *) group->points;

    /* make coeffients visiable */
    coefs = (Coeffs_Poly *) group->coefs;

    /* foward transformation  (source) --> (target)  */
    CRS_georef (e1, n1, e2, n2,
		coefs->E12, coefs->N12,
		(int) group->trans_type);

}

/*-------------------------------------------------------------------*/
poly_trans_inverse (group, e1, n1, z1, e2, n2, z2)
Rectify_Group    *group;
double           *e1, *n1, *z1;
double            e2,  n2,  z2;
{
char           msg[100];

    Control_Points     *points;           
    Coeffs_Poly     *coefs;


    /* check the transformation type */
    if ((group->trans_type < POLY1) || (group->trans_type > POLY3)) {
      /* TODO warning mes */
      return (-1);
    }

    /* make control points visiable */
    points = (Control_Points *) group->points;

    /* make coeffients visiable */
    coefs = (Coeffs_Poly *) group->coefs;

    /* foward transformation  (source) --> (target)  */
    CRS_georef (e2, n2, e1, n1,
		coefs->E21, coefs->N21,
		(int) group->trans_type);

}











