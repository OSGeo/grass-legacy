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

	int color1 ;
	int color2 ;
#else
	extern int color1 ;
	extern int color2 ;
#endif
