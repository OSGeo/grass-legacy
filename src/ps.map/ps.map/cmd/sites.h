#define MAXSITES 100

struct site
{
    int cur;		/* current site file */
    int count;		/* number of site files */
    int color[MAXSITES];
    double size[MAXSITES];
    int size_att[MAXSITES];    /* number of attribute used for size */      
    double rotate[MAXSITES];   /* supported only by eps */    
    char with_text[MAXSITES];
    char *name[MAXSITES];
    char *mapset[MAXSITES];
    char *icon[MAXSITES];
    char *epspre[MAXSITES];   /* first part of EPS file name */
    char *epssuf[MAXSITES];   /* second part of EPS file name */ 
    int  epstype[MAXSITES];  /* 0 - no eps, 1 - common eps, 2 - eps for each category */
    char *font[MAXSITES];
} ;

#ifdef MAIN
    struct site site;
#else
    extern struct site site;
#endif
