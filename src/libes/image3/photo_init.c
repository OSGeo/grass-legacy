/*======================================================================
                             photo_init.c

======================================================================*/

#include "ortho_image.h"

/*--------------------------------------------------------------------
  Set up the group transformation functions based upon the group
  transformation type.  If type == ZERO or is not valid, this 
  routine had better return (-1) or else calls to functions like
  group.get_points will point to NULL.


  Returns:
     0 : o.k.
    -1 : not o.k     
----------------------------------------------------------------------*/
int 
I_init_group_trans_funcs (Rectify_Group *group)
{


   /* set up transformation routines */

  switch (group->trans_type) {
  case POLY1:  
      init_poly_trans_funcs (group);
      return (0);

  case POLY2:  
      init_poly_trans_funcs (group);
      return (0);

  case POLY3:  
      init_poly_trans_funcs (group);
      return (0);

  case PHOTO:  
      init_photo_trans_funcs (group);
      return (0);

  case LAND_TM:  
      init_ltm_trans_funcs (group);
      return (0);

  /** TODO PROJECTIVE TRANS **/
 
  default: 
    return(-1);
   }

}


int 
init_photo_trans_funcs (Rectify_Group *group)
{
    /* routine to get control points */
    group->get_points = I_get_photo_points_data;

    /* routine to get auxillary data */
    group->get_auxil  = I_get_photo_auxil_data;

    /* routine to get coeffs data*/
    group->get_coefs  = I_get_photo_coefs_data;


    /* routine to mark control points */
    group->mark_points = mark_control;

    /* routine to analyze control points */
    group->anal_points = photo_anal_points;

    /* calculate ortho photo parameters */
    group->calculate_trans = photo_trans_calculate;

    /* ortho-photo forward transformation  */
    group->forward_trans  = photo_trans_forward;

    /* ortho-photo inverse transformation */
    group->inverse_trans  = photo_trans_inverse;

    return 0;
}


int 
init_poly_trans_funcs (Rectify_Group *group)
{
    /* routine to get control points */
    group->get_points = I_get_poly_points_data;

    /* routine to get auxillary data */
    group->get_auxil  = NULL;

    /* routine to get coeffs data*/
    group->get_coefs  = I_get_poly_coefs_data;


    /* routine to mark control points */
    group->mark_points = mark_control;

    /* routine to analyze control points */
    group->anal_points = poly_anal_points;


    /* calculate ortho photo parameters */
    group->calculate_trans = poly_trans_calculate;

    /* ortho-photo forward transformation  */
    group->forward_trans  = poly_trans_forward;

    /* ortho-photo inverse transformation */
    group->inverse_trans  = poly_trans_inverse;

    return 0;
}


int 
init_ltm_trans_funcs (Rectify_Group *group)
{
    /* routine to get control points */
    group->get_points = I_get_ltm_points_data;

    /* routine to get auxillary data */
    group->get_auxil  = I_get_ltm_auxil_data;

    /* routine to get coeffs data*/
    group->get_coefs  = I_get_ltm_coefs_data;


    /* routine to mark control points */
    group->mark_points = mark_control;

    /* routine to analyze control points */
    group->anal_points = ltm_anal_points;

    /* calculate ortho photo parameters */
    group->calculate_trans = ltm_trans_calculate;

    /* ortho-photo forward transformation  */
    group->forward_trans  = ltm_trans_forward;

    /* ortho-photo inverse transformation */
    group->inverse_trans  = ltm_trans_inverse;

    return 0;
}
