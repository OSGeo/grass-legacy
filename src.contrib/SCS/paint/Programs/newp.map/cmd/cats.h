#define MAXLABELS 10

struct cats  
{
    int count;		/* number of labels files */
    char *name[MAXLABELS];
    char *mapset[MAXLABELS];
    char *other;
} ;

#ifdef MAIN
    struct cats cats;
#else
    extern struct cats cats;
#endif

#ifdef MAIN
    char* catsfile ;
#else
    extern char* catsfile;
#endif


