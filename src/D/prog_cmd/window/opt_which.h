/*  @(#)opt_which.h	2.1    6/26/87  */

#define HORI	1
#define VERT	2

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"horizontal", HORI,
		"hori", HORI,
		"h", HORI,
		"vertical", VERT,
		"vert", VERT,
		"v", VERT
	} ;
	static int n_variables = 6 ;
	float X ;
	float Y ;
#else
	extern float X ;
	extern float Y ;
#endif
