#define VAL_FROM_STR
#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"


/* This routine is given either a field number (for non-split fields) or the
   first field number and the split number (for split fields) and a string.
   The string is scanned for a value of type matching the field and the value is
   put into the Field_info structure.  In the case of a split field the field
   number passed in will be changed to the subfield that the split number
   indicated. */
int val_from_str(field, split, string)
   int *field, split;
   char *string;
{
  int count;

  /* if this is a split field work down to the subfield number */
  if (split!=0) {
    *field = Field_info[*field].next_field[0];
    count = split-1;
    while (count-- > 0) {
      if (Field_info[*field].next_field[0]!=Field_info[*field].next_field[1])
        *field = Field_info[*field].next_field[1];
      else return(-1);          /* if we try to go past the end of the last split */
    }
  }

  switch (Field_info[*field].column_type) {
  case S_FIELD_CHAR:
  case M_FIELD_CHAR:
  case I_FIELD_CHAR:
    count = sscanf(string, "%d", (int *) Field_info[*field].value);
    break;
  case X_FIELD_CHAR:
  case Y_FIELD_CHAR:
  case F_FIELD_CHAR:
    count = sscanf(string, "%lf", (double *) Field_info[*field].value);
    break;
  case V_FIELD_CHAR:
  case T_FIELD_CHAR:
    count=0;
    while (count<Field_info[*field].length && string[count]!='\0') {
      Field_info[*field].value[count] = string[count];
      count++;
    }
    if (string[count]=='\0') {
      Field_info[*field].value[count] = '\0';
      count = 1;
    }
    else count = -1;
    break;
  default:
    G_fatal_error("Unknown field type in val_from_str().");
  }
  return(count);
}

