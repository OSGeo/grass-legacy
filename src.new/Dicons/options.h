
#define COLOR	1
#define SIZE	2
#define ICON	3
#define INFILE	4

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
		"icon", ICON,
		"i", ICON, 
		"file", INFILE,
		"f", INFILE
	} ;
	static int n_variables = 8 ;

	int color ;
	int size ;
        char icon[512];
	FILE *infile ;
#else
	extern int color ;
	extern int size ;
	extern char icon[512]; 
	extern FILE *infile ;
#endif
