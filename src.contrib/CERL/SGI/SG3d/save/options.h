/*  %W%  %G%  */

#include <stdio.h>

#define NAME	1
#define OVER	2

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
	    "name", NAME,
	    "n", NAME,
	    "overlay", OVER,
	    "over", OVER,
	    "o", OVER
	} ;
	static int n_variables = 5 ;

#define EXTERN
#else
#define EXTERN extern
#endif

	EXTERN char name[128] ;
	EXTERN char *mapset ;
	EXTERN int overlay ;
