#include "V_.h"
#include "Vect.h"
#include <string.h>

/*
**
**  Initialize Head structure.  To make sure that we are not writing
**    out garbage to a file.
**
*/

int Vect__init_head ( struct dig_head *head)
{
    head->organization[0] = 0;
    head->date[0] = 0;
    head->your_name[0] = 0;
    head->map_name[0] = 0;
    head->source_date[0] = 0;

    head->orig_scale  = 0;;
    head->line_3[0] = 0;
    head->plani_zone = 0;
    head->W = head->E = head->S = head->N = 0;
    head->digit_thresh = 0;
    head->map_thresh = 0;

	    /**********New 4.0************/
    head->Version_Major = 4;
    head->Version_Minor = 0;
    head->Back_Major = 4;
    head->Back_Minor = 0;

    /* portability stuff */
    head->portable = 0;
    head->dbl_quick = 0;
    head->flt_quick = 0;
    head->lng_quick = 0;
    head->shrt_quick = 0;

    return 0;
}

/*
**  Copy portable data in dig_head struct from one to another
**   without damaging other user data
*/
int Vect__copy_portable_info (
    struct dig_head *from,
    struct dig_head *to)
{

    to->Version_Major = from->Version_Major;
    to->Version_Minor = from->Version_Minor;
    to->Back_Major    = from->Back_Major;
    to->Back_Minor    = from->Back_Minor;

    to->portable = from->portable;
    to->dbl_quick = from->dbl_quick;
    to->flt_quick = from->flt_quick;
    to->lng_quick = from->lng_quick;
    to->shrt_quick = from->shrt_quick;

    dig_struct_copy (from->dbl_cnvrt,  to->dbl_cnvrt,  DBL_SIZ);
    dig_struct_copy (from->flt_cnvrt,  to->flt_cnvrt,  FLT_SIZ);
    dig_struct_copy (from->lng_cnvrt,  to->lng_cnvrt,  LNG_SIZ);
    dig_struct_copy (from->shrt_cnvrt, to->shrt_cnvrt, SHRT_SIZ);

    return 0;
}
#ifdef FOO 
#define DIG_ORGAN_LEN       30
#define DIG_DATE_LEN        20
#define DIG_YOUR_NAME_LEN   20
#define DIG_MAP_NAME_LEN    41
#define DIG_SOURCE_DATE_LEN 11
#define DIG_LINE_3_LEN      53  /* see below */

#define OLD_LINE_3_SIZE 73
#define NEW_LINE_3_SIZE 53
#define VERS_4_DATA_SIZE 20
#endif


int Vect_copy_head_data ( struct dig_head *from, struct dig_head *to)
{
    strncpy (to->organization, from->organization, DIG_ORGAN_LEN);
    strncpy (to->date, from->date, DIG_DATE_LEN);
    strncpy (to->your_name, from->your_name, DIG_YOUR_NAME_LEN);
    strncpy (to->map_name, from->map_name, DIG_MAP_NAME_LEN);
    strncpy (to->source_date, from->source_date, DIG_SOURCE_DATE_LEN);

    strncpy (to->line_3, from->line_3, DIG_LINE_3_LEN);

    to->orig_scale = from->orig_scale;
    to->plani_zone = from->plani_zone;
    
    to->W = from->W;
    to->E = from->E;
    to->S = from->S;
    to->N = from->N;

    to->digit_thresh = from->digit_thresh;
    to->map_thresh = from->map_thresh;

    return 0;
}
