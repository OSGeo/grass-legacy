/*  %W%  %G%  */

/* Command argument positions */
#define NAME	1
#define COLOR	2
#define LINES	3
#define COLS	4

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
		"lines", LINES,
		"cols", COLS
	} ;
	static int n_variables = 6 ;

	char map_name[128] ;
	int color ;
	int lines ;
	int cols ;
#else
	extern char map_name[] ;
	extern int color ;
	extern int lines ;
	extern int cols ;
#endif
