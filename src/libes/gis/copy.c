#include "gis.h"
/*
 *********************************************************
 *
 * G_copy (a, b, n)
 *
 * copies n bytes starting at address b into address a
 ********************************************************/

int G_copy ( void *a,void *b,int n)
{
	char *ap,*bp;
	ap = a;
	bp = b;

    while (n-- > 0)
	*ap++ = *bp++;

    return 0;
}
