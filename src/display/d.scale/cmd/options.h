
#define COLOR1	1
#define COLOR2	2
#define EASTING 3
#define NORTHING 4
#define MOUSE 5 

#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN int color1 ;
EXTERN int color2 ;
EXTERN double east;
EXTERN double north;
EXTERN int coord_inp;
EXTERN int mouse;
