#define FILL_FIELD
#include "gis.h"
#include "globals.h"
#include "make.h"
#include "rim.h"


/* This routine is called to fill the RIM buffer with a field value.
   It takes the value out of the Field_info.value pointer.  */
fill_field(field_num)
   int field_num;
{
   int first, i, offset;
   char tempstr[4000];

   switch (Field_info[field_num].column_type) {
      case S_FIELD_CHAR:
      case I_FIELD_CHAR:
         fill_buf_i(&Rim_buffer[Field_info[field_num].rec_offset],
                    (int *) Field_info[field_num].value);
         break;
      case X_FIELD_CHAR:
      case Y_FIELD_CHAR:
      case F_FIELD_CHAR:
         fill_buf_d(&Rim_buffer[Field_info[field_num].rec_offset],
                    (double *) Field_info[field_num].value);
         break;
      case T_FIELD_CHAR:
         /* If it is not a split text field send it out normally */
         if ((first=Field_info[field_num].next_field[0])==MAX_FIELDS)
            fill_buf_t(&Rim_buffer[Field_info[field_num].rec_offset],
                       Field_info[field_num].value,
                       Field_info[field_num].length);
         /* otherwise fill in this portion of the split text field */
         else {
            retr_buf_t(&Rim_buffer[Field_info[first].rec_offset],
                       tempstr, rim_text_len(first));
            i = 0;
            offset=Field_info[field_num].value-Field_info[first].value;
            while (i<Field_info[field_num].length
                   && Field_info[field_num].value[i]!='\0') {
               tempstr[i+offset] = Field_info[field_num].value[i];
               i++;
            }
            while (i<Field_info[field_num].length) { /*pad with spaces*/
               tempstr[i+offset] = ' ';
               i++;
            }
            if (Field_info[field_num].next_field[0]
                ==Field_info[field_num].next_field[1])
               tempstr[i+offset] = '\0';

            fill_buf_t(&Rim_buffer[Field_info[first].rec_offset],
                       tempstr, rim_text_len(first));
         }
         break;
      default:
         G_fatal_error("Unknown field type in fill_field().");
   }
}


fill_fields()
{
   int count;

   for (count=0; count<Field_num; count++)
      fill_field(count);
}

