/*  @(#)options.h	2.1    6/26/87  */

/* Command argument positions */
#define NAME	1
#define COLOR	2

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"name", NAME,
		"n", NAME,
		"color", COLOR,
		"c", COLOR
	} ;
	static int n_variables = 4 ;

	char map_name[64] ;
	char color[64] ;
#else
	extern char map_name[] ;
	extern char color[64] ;
#endif

#define NORMAL	1
#define FANCY	2
