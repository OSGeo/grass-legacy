#define MAXLINES 20

struct line
{
    int cur;		/* current vector file */
    int count;		/* number of vector files */
    char *linestyle[MAXLINES];
    int colors[MAXLINES][9];
    int width[MAXLINES];
    int hcolor[MAXLINES];
    int hwidth[MAXLINES];
    char masked[MAXLINES];
    char *name[MAXLINES];
    char *mapset[MAXLINES];
} ;

#ifdef MAIN
    struct line line;
#else
    extern struct line line;
#endif
