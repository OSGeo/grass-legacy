#define RIM_TEXT_LEN

#include <stdio.h>
#include "globals.h"
#include "make.h"


/* This routine returns the length of a text field in RIM from the info
   in the Field_info array.  Split text fields are combined into one
   field for this purposes and this routine assumes that the 'next_field'
   field of the field_info structure has been set correctly by res_split_f().
   If a non-text field is asked about a 0 will be returned.
   */
int rim_text_len(field_num)
   int field_num;
{
   int start, length;

   if (Field_info[field_num].column_type != T_FIELD_CHAR &&
       Field_info[field_num].column_type != V_FIELD_CHAR)
     return(0);

   /* move to first field if in a split field */
   start = Field_info[field_num].next_field[0];
   if (start==MAX_FIELDS)   /*not a split field */
      return(Field_info[field_num].length);
   else {
      length = 0;
      field_num = start;
   }

   /* add up all the lengths in either a normal of split text field */
   do {
      length += Field_info[field_num].length;
      field_num = Field_info[field_num].next_field[1];
   } while (start!=field_num);

   return(length);
}


