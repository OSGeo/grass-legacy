/*======================================================================
                             ltm_coeffs.c

======================================================================*/

#include "ortho_image.h"

int 
I_get_ltm_coefs_data (Rectify_Group *group)
{
   group->coefs = (Coeffs_Ltm *) G_malloc (sizeof (Coeffs_Ltm));
   get_ltm_coeffs_data(group);

   return 0;
}


int 
get_ltm_coeffs_data (Rectify_Group *group)
{
Coeffs_Ltm  *coeffs;

    /* make auxilary visiable */
    coeffs = (Coeffs_Ltm *) group->coefs;

   return 0;
}
