struct barscale 
{
    int count;
    char *other;
} ;

#ifdef MAIN
    struct barscale barscale;
#else
    extern struct barscale barscale;
#endif


