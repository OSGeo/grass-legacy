static char rcsid[] = "@(#)XGRASS $Id: strdup.c,v 0.0.0.1 1992/05/05 14:58:52 kurt Exp kurt $";
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
