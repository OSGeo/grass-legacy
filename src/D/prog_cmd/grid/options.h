#define SIZE	1
#define COLOR   2

#ifdef MAIN
    struct variables
    {
        char *alias ;
        int position ;
    } variables[] = {
        "size", SIZE,
        "s", SIZE,
        "color", COLOR,
        "c", COLOR
    } ;
    static int n_variables = 4 ;

    int color ;
    int size ;
#else
    extern int color ;
    extern int size ;
#endif
