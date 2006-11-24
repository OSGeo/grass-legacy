#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL int nrows, ncols;
GLOBAL int buflen;
GLOBAL int direction;
/* please, remove before GRASS 7 released */
GLOBAL int silent;
GLOBAL int zero_only;
GLOBAL int preserve_edges;
