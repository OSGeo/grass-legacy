/*  @(#)options.h	2.1    6/26/87  */

#define COLOR	1
#define SIZE	2
#define LINE	3

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
	    "size", SIZE,
	    "s", SIZE,
	    "color", COLOR,
	    "c", COLOR,
	    "line", LINE,
	    "l", LINE
	} ;
	static int n_variables = 6 ;

	int color ;
	float size ;
	int start_line ;
#else
	extern int color ;
	extern float size ;
	extern int start_line ;
#endif
