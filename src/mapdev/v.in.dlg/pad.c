#include	<stdio.h>

int 
pad (char *s, int n)
{
	int fill ;

	fill = 0 ;

	while(n--)
	{
		if(*s == '\0'  ||  *s == '\n')
			fill = 1 ;
		if(fill)
			*s = ' ' ;
		s++ ;
	}
	*s = '\0' ;

    return 0;
}
