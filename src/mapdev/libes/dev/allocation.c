/*  @(#)allocation.c	1.1  5/4/87  */

#include	<stdio.h>
char *
falloc(nelem, elsize)
	int nelem, elsize ;
{
	char *calloc() ;
	char *ptr ;


	ptr = calloc( (unsigned)nelem, (unsigned)elsize) ;

	if (!ptr)
	{
		printf("ERROR: no more memory available\n") ;
		exit(-1) ;
	}

	return(ptr) ;
}

char *
frealloc(oldptr, nelem, elsize)
	char *oldptr ;
	int nelem, elsize ;
{
	char *realloc() ;
	char *ptr ;

	ptr = realloc(oldptr, (unsigned)(nelem * elsize)) ;

	if (!ptr)
	{
		printf("ERROR: no more memory available\n") ;
		exit(-1) ;
	}

	return(ptr) ;
}

/***
char *
frealloc(oldptr, nelem, elsize, oldnelem)
	char *oldptr ;
	int nelem, elsize ;
	int oldnelem ;
{
	char *calloc() ;
	char *ptr ;

fprintf(stderr, " frealloc:  nelem: %d,  elsize: %d, oldnelem: %d\n",
	nelem, elsize, oldnelem) ;
sleep(3) ;
	ptr = calloc((unsigned)nelem, (unsigned)elsize) ;
	if (!ptr)
	{
		printf("ERROR: no more memory available\n") ;
		exit(-1) ;
	}

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
***/
