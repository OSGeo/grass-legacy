#define VAL_TO_STR
#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"


/* This routine writes the value of the given field into a string using
   sprintf.  Note all the fields are left justified and padded with spaces
   out to the field's length. */
int val_to_str(string, field)
   int field;
   char *string;
{
   int count;
   char format[10];

   switch (Field_info[field].column_type) {
      case S_FIELD_CHAR:
      case I_FIELD_CHAR:
         sprintf(format, "%%-%dd", Field_info[field].length);
         sprintf(string, format, *((int *) Field_info[field].value));
         break;
      case X_FIELD_CHAR:
      case Y_FIELD_CHAR:
      case F_FIELD_CHAR:
         sprintf(format, "%%-%d.%dlf",
               Field_info[field].length,Field_info[field].next_field[1]);
         sprintf(string, format, *((double *) Field_info[field].value));
         break;
      case T_FIELD_CHAR:
         count=0;
         while (count<Field_info[field].length &&
                Field_info[field].value[count]!='\0') {
            string[count] = Field_info[field].value[count];
            count++;
         }
         string[count]='\0';
         break;
      default:
         G_fatal_error("Unknown field type in val_to_str().");
   }
   return(count);
}

