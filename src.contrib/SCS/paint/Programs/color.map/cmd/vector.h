#define MAXVECTORS 20

struct vector
{
    int cur;		/* current vector file */
    int count;		/* number of vector files */
    char *linestyle[MAXVECTORS];
    int colors[MAXVECTORS][9];
    int width[MAXVECTORS];
    int hcolor[MAXVECTORS];
    int hwidth[MAXVECTORS];
    char masked[MAXVECTORS];
    char *name[MAXVECTORS];
    char *mapset[MAXVECTORS];
} ;

#ifdef MAIN
    struct vector vector;
#else
    extern struct vector vector;
#endif
