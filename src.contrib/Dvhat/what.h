/*  %W%  %G%  */

#define MAX_LAYERS 1

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL struct Categories cats[MAX_LAYERS];
GLOBAL struct Map_info Map;
GLOBAL int nlayers;
/*
GLOBAL char name[MAX_LAYERS][40];
GLOBAL char mapset[MAX_LAYERS][40];
*/
GLOBAL char name[40];
GLOBAL char mapset[40];
