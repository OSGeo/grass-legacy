/* Command argument positions */
#define NAME	1

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"name", NAME,
		"n", NAME
	} ;
	static int n_variables = 2 ;

	char map_name[128] ;
	char *mapset ;
#else
	extern char map_name[] ;
	extern char *mapset ;
#endif
