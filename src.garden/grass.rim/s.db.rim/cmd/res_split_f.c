#define RES_SPLIT_F

#include "gis.h"
#include "globals.h"
#include "make.h"


/* This routine converts the 'next_field' array in Field_info from
   just the (negative) split field numbers (example field names
   descript.1 and descript.2 have next_field arrays that look like {0,-1}
   and {0,-2} respectively after the make_add command) to indices
   into the Field_info array that show where the first portion
   of a split field is and where the next portion of a split field is.
   Continuing the example above, say that descript.1 was the
   10th field defined and descript.2 was the 21st field defined.  Then after
   this routine their respective next_field arrays would be {10,21} and
   {10,10}.

   In addition, for split fields it sets the rec_offset value for all but
   the first field to be the offset into the string, not the RIM buffer.*/

res_split_f()
{
   int num, i, last, next, flag, offset;
   char c;

   /* first pass: fill next_field[1] with the first field in split field */
   for (num=0; num<Field_num; num++) {
      c = Field_info[num].column_type;
      if (c==F_FIELD_CHAR || c==X_FIELD_CHAR || c==Y_FIELD_CHAR) {
        Field_info[num].next_field[0] = MAX_FIELDS;
        if (Field_info[num].next_field[1] < 0)
          Field_info[num].next_field[1]= - Field_info[num].next_field[1];
        else {
          if ((c==X_FIELD_CHAR || c==Y_FIELD_CHAR) && Projection==PROJECTION_LL)
            Field_info[num].next_field[1] = 6;
          else
            Field_info[num].next_field[1] = 2;
        }
      }
      else {
        if (Field_info[num].next_field[1]==0) {
          Field_info[num].next_field[0] = MAX_FIELDS;
          Field_info[num].next_field[1] = MAX_FIELDS;
        }
        else if (Field_info[num].next_field[1]==-1) {
          for (i=0; i<Field_num; i++)
            if (0==strcmp(Field_info[num].column_name,
                           Field_info[i].column_name))
               Field_info[i].next_field[0] = num;
        }
      }
   }

   /* second pass: find the sequence within split fields */
   for (num=0; num<Field_num; num++) {
      /* if this is the beginning of a chain of split fields */
      if (Field_info[num].next_field[1]==-1) {
         next = -2;
         last = num;
         offset = 0;
         /* keep looking for more split fields until we get back to the first*/
         while (next!=-1) {
            flag = FALSE;
            offset += Field_info[last].length;
            /* find the next split field and point the last one to it */
            for (i=0; i<Field_num && flag==FALSE; i++)
               if (Field_info[i].next_field[0]==num &&
                     Field_info[i].next_field[1]==next) {
                  Field_info[last].next_field[1] = i;
                  Field_info[i].rec_offset = offset;
                  flag = TRUE;
               }
            /* if didn't find a next one the last one is the last split field*/
            if (flag==FALSE) {
               next = -1;
               Field_info[last].next_field[1]=num;
            }
            /* otherwise continue loop looking for the next field */
            else {
               next--;
               last = Field_info[last].next_field[1];
            }
         }
      }
   }

   /* check for any remaining (negative) split field numbers that would
      indicate an error condition */
   for (num=0; num<Field_num; num++)
      if (Field_info[num].next_field[1]<0) {
         fprintf(Outfile, "\nForm Definition Error: Split field not number continuously starting from 1.\n");
         return(FALSE);
      }

   return(TRUE);
}



