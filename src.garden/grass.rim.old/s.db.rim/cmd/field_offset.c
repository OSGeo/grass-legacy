#define FIELD_OFFSET

#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"


/* This function returns the offset into an integer array (for RIM) that
   is the size of the field 'count' in the Field_info array.  For split
   text fields the first field will report the full offset for the split
   text and any of the other fields will return the same offset. */
int field_offset(count)
   int count;
{
   switch (Field_info[count].column_type) {
      case S_FIELD_CHAR:
      case I_FIELD_CHAR: return(INT_OFFSET);

      case X_FIELD_CHAR:
      case Y_FIELD_CHAR:
      case F_FIELD_CHAR: return(DOUBLE_OFFSET);

      case T_FIELD_CHAR: return((rim_text_len(count) + sizeof(int) - 1)
                                   / sizeof(int) );

      default: return(0);
   }
}

