
#define EXTERN extern

#include "gis.h"
#include "edit.h"

new_firing_point()
{

    int i;

    if (i=num_fp++)
        firing = (FIRPOINT *)G_realloc(firing,(i+1)*sizeof(FIRPOINT));
    else
        firing = (FIRPOINT *)G_malloc(sizeof(FIRPOINT));

    firing[i].id[0] = 0;
    firing[i].easting = 0;
    firing[i].northing = 0.;
    firing[i].gc_fact = 1.5;
    firing[i].num_weap = 0;

}


new_fp_info(n)
{

    int i;

    if (i=firing[n].num_weap++)
        firing[n].ptr = (FP_INFO *)G_realloc(firing[n].ptr, 
          (i+1)*sizeof(FP_INFO));
    else
        firing[n].ptr = (FP_INFO *)G_malloc(sizeof(FP_INFO));

    firing[n].ptr[i].weap_code = 0;
    firing[n].ptr[i].num_day = 0;
    firing[n].ptr[i].num_night = 0;
    firing[n].ptr[i].min_charge = 0;
    firing[n].ptr[i].max_charge = 0;
    firing[n].ptr[i].targ_id[0] = 0;
    firing[n].ptr[i].noise[0] = 0;
    firing[n].ptr[i].height = 0.;

}
