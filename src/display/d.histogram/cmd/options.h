#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL char *map_name ;
GLOBAL int color ;
GLOBAL float size ;
GLOBAL int style ;
GLOBAL int type ;
GLOBAL int nodata;

#define PIE	1
#define BAR	2
#define COUNT   3
#define AREA	4
#define YES     1
#define NO      0
