#define GLOBAL
#include "global.h"
#include "crs.h" 



I_get_group_coeffs_data (group) 
Rectify_Group *group;
{

   group->coefs = (Coeffs_Photo *) G_malloc (sizeof (Coeffs_Photo));
   get_photo_coeffs_data(group);
}


get_photo_coeffs_data(group)
Rectify_Group    *group;
{
Coeffs_Photo  *coeffs;
char           msg[100];


    /* make auxilary visiable */
    coeffs = (Coeffs_Photo *) group->coefs;

    Compute_fiducial_equation();
    Compute_ortho_equation();

}

