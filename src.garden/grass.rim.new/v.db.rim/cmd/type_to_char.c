#define TYPE_TO_CHAR

#include "vect.h"

char type_to_char(string)
     char *string;
{

  if (index(string, AREA_CHAR) != 0) return(AREA_CHAR);
  if (index(string, LINE_CHAR) != 0) return(LINE_CHAR);
  if (index(string, SITE_CHAR) != 0) return(SITE_CHAR);

  return('\0');
}
