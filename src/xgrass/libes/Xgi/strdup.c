static char rcsid[] = "@(#)XGRASS $Id$";
/*
 * File: strdup.c
 *
 * Desc: duplicate the string passed in
 *
 * Auth: Kurt Buehler
 *
 * Date: Tue Nov  5 16:21:47 CST 1991
 *
 * Modification History:
 *
 *
 */
#include "xgrass_lib.h"

#ifdef _NO_PROTO
char *_XgStrDup(s)
char *s;
#else
char *_XgStrDup(char *s)
#endif
{
    char *t;

    if ( s ) {
	t = _XgMalloc(strlen(s) + 1);
	strcpy(t,s);
	return t;
    }
    return NULL;
}
