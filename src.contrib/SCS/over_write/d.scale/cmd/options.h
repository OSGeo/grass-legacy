
#define COLOR1	1
#define COLOR2	2
#define EASTING 3
#define NORTHING 4
#define MOUSE 5 
#define UNIT 1 

#ifdef MAIN

	int color1 ;
	int color2 ;
        double easting;
		double northing;
        int coord_inp;
        int mouse;
        int units;
#else
	extern int color1 ;
	extern int color2 ;
        extern double easting;
		extern double northing;
        extern int coord_inp;
        extern int mouse;
        extern int units;
#endif
