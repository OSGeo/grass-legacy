#include "gis.h"

#define TAB		011
#define SPACE	040

/*!
 * \brief remove non-ascii characters from buffer
 *
 *   returns <b>string</b> with non_ascii characters removed, except for tabs
 *   which are turned into spaces.
 *
 *  \param *string buffer to have non-ascii characters removed
 *  \return int
 */

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
