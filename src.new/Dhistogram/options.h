/*  %W%  %G%  */

/* Command argument positions */
#define NAME	1
#define COLOR	2
#define TYPE	3
#define STYLE	4
#define NODATA  5

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
                "style",STYLE,
                "s",STYLE,
		"nodata",NODATA
	} ;
	static int n_variables = 9 ;

	char map_name[64] ;
	int color ;
	float size ;
	int style ;
	int type ;
        int nodata;
#else
	extern char map_name[] ;
	extern int color ;
	extern float size ;
	extern int style ;
	extern int type ;
	extern int nodata; 
#endif

#define PIE	1
#define BAR	2
#define COUNT   3
#define AREA	4
#define YES     1
#define NO      0
