static char rcsid[] = "@(#)XGRASS $Id$";
/*
 * File:
 *
 * Desc:
 *
 * Auth:
 *
 * Date:
 *
 * Modification History:
 *
 *
 */
#include "xgrass.h"

char *StrDup(s)
char *s;
{
    char *t;

    if ( s ) {
	t = XtMalloc(strlen(s) + 1);
	strcpy(t,s);
	return t;
    }
    return NULL;
}
