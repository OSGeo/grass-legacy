/*  %W%  %G%  */

/* Command argument positions */
#define NAME	1
#define COLOR	2
#define TYPE	3
#define LINES	4

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"name", NAME,
		"n", NAME,
		"color", COLOR,
		"c", COLOR,
		"type", TYPE,
		"t", TYPE,
		"lines", LINES,
		"l", LINES
	} ;
	static int n_variables = 8 ;

	char map_name[64] ;
	int color ;
	float size ;
	int type ;
	int lines ;
#else
	extern char map_name[] ;
	extern int color ;
	extern float size ;
	extern int type ;
	extern int lines ;
#endif

#define NORMAL	1
#define FANCY	2
