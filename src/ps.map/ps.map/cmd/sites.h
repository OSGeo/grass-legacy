#define MAXSITES 20

struct site
{
    int cur;		/* current site file */
    int count;		/* number of site files */
    int color[MAXSITES];
    double size[MAXSITES];
    char with_text[MAXSITES];
    char *name[MAXSITES];
    char *mapset[MAXSITES];
    char *icon[MAXSITES];
    char *font[MAXSITES];
} ;

#ifdef MAIN
    struct site site;
#else
    extern struct site site;
#endif
