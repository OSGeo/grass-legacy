/*======================================================================
                             photo_coeffs.c

======================================================================*/

#include "ortho_image.h"

int I_get_photo_coefs_data (Rectify_Group *group)
{

   group->coefs = (Coeffs_Photo *) G_malloc (sizeof (Coeffs_Photo));
   get_photo_coeffs_data(group);

   return 0;
}

int get_photo_coeffs_data (Rectify_Group *group)
{
Coeffs_Photo  *coeffs;

    /* make auxilary visiable */
    coeffs = (Coeffs_Photo *) group->coefs;

    return 0;
}
