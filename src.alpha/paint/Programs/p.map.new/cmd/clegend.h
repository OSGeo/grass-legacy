struct prelegend 
{
    int count;
    char *other;
} ;

#ifdef MAIN
    struct prelegend prelegend ;
#else
    extern struct prelegend prelegend;
#endif

struct legend 
{
    int count;
    char *other;
} ;

#ifdef MAIN
    struct legend legend ;
#else
    extern struct legend legend;
#endif

