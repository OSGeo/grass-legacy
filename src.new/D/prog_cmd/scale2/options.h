/*  %W%  %G%  */

#define COLOR1	1
#define COLOR2	2
#define EASTING 3
#define NORTHING 4
#define MOUSE 5

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"color1", COLOR1,
		"c1", COLOR1,
		"color2", COLOR2,
		"c2", COLOR2,
		"east", EASTING,
		"easting", EASTING,
		"e", EASTING,
		"north", NORTHING,
		"northing", NORTHING,
		"n", NORTHING,
		"mouse", MOUSE,
		"m", MOUSE
	} ;
	static int n_variables = 11 ;

	int color1 ;
	int color2 ;
        double easting;
        double northing;
        int coord_inp;
        int mouse;
#else
	extern int color1 ;
	extern int color2 ;
        extern double easting;
        extern double northing;
        extern int coord_inp;
        extern int mouse;
#endif
