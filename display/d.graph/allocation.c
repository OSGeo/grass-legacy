#include "gis.h"
#include <stdlib.h>
#include <stdio.h>

char *falloc(int nelem,int elsize)
{
	char *ptr ;

	ptr = calloc(nelem, elsize) ;

	if (!ptr)
	   G_fatal_error ("ERROR: no more memory available") ;

	return(ptr) ;
}

char *frealloc(char *oldptr, int nelem,int elsize, int oldnelem)
{
	char *ptr ;

	ptr = calloc(nelem, elsize) ;
	if (!ptr)
	    G_fatal_error ("ERROR: no more memory available") ;

	{
		register char *a ;
		register char *b ;
		register int n ;
		n = oldnelem * elsize ;
		a = ptr ;
		b = oldptr ;
		while(n--)
			*a++ = *b++ ;
	}

	free(oldptr) ;
	return(ptr) ;
}
