/*======================================================================
                             photo_coeffs.c

======================================================================*/

#include "ortho_image.h"

I_get_photo_coefs_data (group) 
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

}

