/*  @(#)options.h	2.1    6/26/87  */

#define MODE	1

#define UNSET	0
#define FLOAT	1
#define FIXED	2

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
	    "mode", MODE,
	    "m", MODE
	} ;
	static int n_variables = 2 ;

	int mode ;
#else
	extern int mode ;
#endif
