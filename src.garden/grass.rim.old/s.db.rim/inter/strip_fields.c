#define STRIP_FIELDS

#include <stdio.h>
#include "make.h"


strip_fields(str)
   char *str;
{
   int i, len, j, count;

   len = strlen(str);
   i = 0;

   while (i<len) {
      i += strcspn(&str[i], TILDE);
      if (i>=len) break;
      j = i + 1 + strcspn(&str[i+1], SPECIAL_CHARS);
      if (str[j]==TILDE_CHAR) j += strspn(&str[j], TILDE);
      else if (str[j]==DOT_CHAR) {
         j++;
         j += strspn(&str[j], DIGITS);
      }
      for (count=i; count<j; count++) str[count]=' ';
      i = j;
   }
}

