#ifdef MAIN
	char *mapset_name[256];
	int nmapsets ;
	int choice[256];
	int nchoices;
	int curr_mapset[256];
	int ncurr_mapsets;
#else
	extern char *mapset_name[];
	extern int nmapsets;
	extern int choice[];
	extern int nchoices;
	extern int curr_mapset[];
	extern int ncurr_mapsets;
#endif

#define	REPLACE	0
#define ADD	1
#define DELETE	2
