
struct vectorlegend 
{
    int count;
    char *other;
} ;

#ifdef MAIN
    struct vectorlegend vectorlegend ;
#else
    extern struct vectorlegend vectorlegend;
#endif


