#define CLEAR_VALUE
#include "gis.h"
#include "globals.h"
#include "make.h"
#include "rim.h"


/* This routine is called to clear a field value.
   It writes an empty value to the Field_info.value pointer. */
clear_value(num)
   int num;
{
   int count;

   switch (Field_info[num].column_type) {
      case S_FIELD_CHAR:
      case I_FIELD_CHAR:
         *((int *) Field_info[num].value) = 0;
         break;
      case X_FIELD_CHAR:
      case Y_FIELD_CHAR:
      case F_FIELD_CHAR:
         *((double *) Field_info[num].value) = 0.0;
         break;
      case T_FIELD_CHAR:
         *(Field_info[num].value) = '\0';
         break;
      default:
         G_fatal_error("Unknown field type in clear_value().");
   }
}


clear_values()
{
   int count;

   for (count=0; count<Field_num; count++)
      clear_value(count);
}
