#include <string.h>
#include "Vect.h"

/*
   **
   **  Initialize Head structure.  To make sure that we are not writing
   **    out garbage to a file.
   **
 */

int 
Vect__init_head (struct dig_head *head)
{
  head->organization[0] = 0;
  head->date[0] = 0;
  head->your_name[0] = 0;
  head->map_name[0] = 0;
  head->source_date[0] = 0;

  head->orig_scale = 0;;
  head->line_3[0] = 0;
  head->plani_zone = 0;
  /* head->W = head->E = head->S = head->N = 0; */
  head->digit_thresh = 0;
  head->map_thresh = 0;

  /* portability stuff */
  /* now set only by V_open_new/old() */
  /*
  head->Version_Major = 4;
  head->Version_Minor = 0;
  head->Back_Major = 4;
  head->Back_Minor = 0;
  */
  /*
  head->byte_order= 0;
  head->dbl_quick = 0;
  head->flt_quick = 0;
  head->lng_quick = 0;
  head->shrt_quick = 0;
  */
  
  return 0;
}

int 
Vect_copy_head_data (struct dig_head *from, struct dig_head *to)
{
  strncpy (to->organization, from->organization, DIG_ORGAN_LEN);
  strncpy (to->date, from->date, DIG_DATE_LEN);
  strncpy (to->your_name, from->your_name, DIG_YOUR_NAME_LEN);
  strncpy (to->map_name, from->map_name, DIG_MAP_NAME_LEN);
  strncpy (to->source_date, from->source_date, DIG_SOURCE_DATE_LEN);

  strncpy (to->line_3, from->line_3, DIG_LINE_3_LEN);

  to->orig_scale = from->orig_scale;
  to->plani_zone = from->plani_zone;

  /*
  to->W = from->W;
  to->E = from->E;
  to->S = from->S;
  to->N = from->N;
  */

  to->digit_thresh = from->digit_thresh;
  to->map_thresh = from->map_thresh;

  return 0;
}
