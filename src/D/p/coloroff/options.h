/*  @(#)options.h	2.1    6/26/87  */

#define OFF	1

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
	    "offset", OFF,
	    "off", OFF,
	    "o", OFF,
	} ;
	static int n_variables = 3 ;

	int off ;
#else
	extern int off ;
#endif
