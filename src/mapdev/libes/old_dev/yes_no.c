/*  @(#)yes_no.c	2.1  6/26/87  */
#include <stdio.h>

yes_no(s)
	char *s ;
{
	char buff[64] ;
	while (1)
	{
		printf("%s", s) ;
		gets(buff) ;
		switch (*buff)
		{
			case 'Y': case 'y':
				return(1) ;
			case 'N': case 'n':
				return(0) ;
			default:
				printf("\nPlease answer yes or no\n") ;
		}
	}
}

