/* what.c */
int what(double, double, double, int, int, int, int);

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL	extern
#endif

GLOBAL	char **vect;
GLOBAL	int nvects;
GLOBAL	struct Map_info *Map;
