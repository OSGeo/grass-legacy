/*  @(#)fill_out.c	2.1  6/26/87  */
/* This moves a NULL in the middle of a buffer to the end of the buffer */

#include <stdio.h>
fill_out(buf, n)
	char buf[] ;
	int n ;
{
	char *b ;
	int i ;

	b = buf ;
	for (i=0; i<n; i++, b++)
	{
		if (*b == '\n')
			*b = 0 ;
		if (*b == 0)
			break ;
	}
	
	for ( ; i<n; i++, b++)
		*b = ' ' ;

	*--b = 0 ;
}
