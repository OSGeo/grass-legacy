#define MAXSITES 20
#include "icon.h"

struct site
{
    int cur;		/* current site file */
    int count;		/* number of site files */
    int color[MAXSITES];
    char with_text[MAXSITES];
    char *name[MAXSITES];
    char *mapset[MAXSITES];
    ICON icon[MAXSITES];
} ;

#ifdef MAIN
    struct site site;
#else
    extern struct site site;
#endif
