#include "gis.h"
/**********************************************************************
 *
 *   char *
 *   G_ascii_check (buff)
 *
 *   returns buff with non_ascii characters removed, except for tabs
 *   which are turned into spaces.
 *
 *   parms:
 *      char *buff   buffer to have non-ascii characters removed
 *
 *   returns:
 *      nothing
 *
 **********************************************************************/

#define TAB		011
#define SPACE	040

int G_ascii_check(
    char *string )
{
    char *ptr1, *ptr2 ;

    ptr1 = string ;
    ptr2 = string ;

    while(*ptr1)
    {
	if ((*ptr1 >= 040) && (*ptr1 <= 0176))
	    *ptr2++ = *ptr1 ;
	else if (*ptr1 == TAB)
	    *ptr2++ = SPACE ;
	ptr1++ ;
    }
    *ptr2 = 0 ;

    return 0;
}
