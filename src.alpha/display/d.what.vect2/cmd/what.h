#define MAX_LAYERS 15

#ifdef GLOBAL
#define GLOBAL extern
#endif

GLOBAL struct Categories Cats[MAX_LAYERS];
GLOBAL int nlayers;
GLOBAL int fd[MAX_LAYERS];
GLOBAL char name[MAX_LAYERS][80];
GLOBAL char mapset[MAX_LAYERS][80];
