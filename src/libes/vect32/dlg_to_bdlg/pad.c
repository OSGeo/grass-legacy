#include	<stdio.h>

pad(s,n)
	char *s ;
	int n ;
{
	int fill ;

	fill = 0 ;

	while(n--)
	{
		if(*s == NULL  ||  *s == '\n')
			fill = 1 ;
		if(fill)
			*s = ' ' ;
		s++ ;
	}
	*s = NULL ;
}
