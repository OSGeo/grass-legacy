#define MAXVECTORS 20

struct vector
{
    int cur;
    int count;
    double x, y;
    int fontsize;
    char *font;
    char *linestyle[MAXVECTORS];
    char *setdash[MAXVECTORS];
    int colors[MAXVECTORS][9];
    double width[MAXVECTORS];
    int  hcolor[MAXVECTORS];
    double hwidth[MAXVECTORS];
    char masked[MAXVECTORS];
    char *name[MAXVECTORS];
    char *mapset[MAXVECTORS];
} ;

#ifdef MAIN
    struct vector vector;
#else
    extern struct vector vector;
#endif
