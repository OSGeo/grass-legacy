/* %W%  %G%  */

/* Command argument positions */
#define NAME	1
#define COLOR1	2
#define COLOR2	3

#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"name", NAME,
		"n", NAME,
		"color1",	COLOR1,
		"c1",	COLOR1,
		"color2",	COLOR2,
		"c2",	COLOR2
	} ;
	static int n_variables = 8 ;
#endif

	EXTERN char name[64] ;
	EXTERN int color1 ;
	EXTERN int color2 ;
