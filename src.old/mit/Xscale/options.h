/*  %W%  %G%  */

#define COLOR1	1
#define COLOR2	2

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"color1", COLOR1,
		"c1", COLOR1,
		"color2", COLOR2,
		"c2", COLOR2
	} ;
	static int n_variables = 4 ;

	char color1[64] ;
	char color2[64] ;
#else
	extern char color1[64] ;
	extern char color2[64] ;
#endif
