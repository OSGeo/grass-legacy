#define RIM_TYPE

#include <stdio.h>
#include "make.h"

#define RIM_F_STR "double"
#define RIM_I_STR "integer"
#define RIM_T_STR "text"
/* This routine is passed a character representing a Gdbsites type and
   a string pointer to a string to be filled with the RIM type and it
   returns the string pointer which represents the RIM data type
   that corresponds to the input character in the form definition code. */
char *rim_type(type_c, type_str)
   char type_c, *type_str;
{
  switch (type_c) {
  case I_FIELD_CHAR:
  case S_FIELD_CHAR:
  case M_FIELD_CHAR:
    strcpy(type_str, RIM_I_STR);
    break;
  case V_FIELD_CHAR:
  case T_FIELD_CHAR:
    strcpy(type_str, RIM_T_STR);
    break;
  case X_FIELD_CHAR:
  case Y_FIELD_CHAR:
  case F_FIELD_CHAR:
    strcpy(type_str, RIM_F_STR);
    break;
  default:
    strcpy(type_str, "");
  }

  return(type_str);
}


