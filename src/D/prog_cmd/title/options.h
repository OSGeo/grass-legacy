/*  %W%  %G%  */

/* Command argument positions */
#define NAME	1
#define COLOR	2
#define SIZE	3
#define TYPE	4

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
		"size", SIZE,
		"s", SIZE,
		"type", TYPE,
		"t", TYPE
	} ;
	static int n_variables = 8 ;

	char map_name[64] ;
	char color[32] ;
	float size ;
	int type ;
#else
	extern char map_name[] ;
	extern char color[] ;
	extern float size ;
	extern int type ;
#endif

#define NORMAL	1
#define FANCY	2
