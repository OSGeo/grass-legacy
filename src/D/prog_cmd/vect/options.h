/* Command argument positions */
#define NAME    1
#define COLOR   2

#ifdef MAIN
    struct variables
    {
        char *alias ;
        int position ;
    } variables[] = {
        "name", NAME,
        "n", NAME,
        "color", COLOR,
        "c", COLOR
    } ;
    static int n_variables = 4 ;

    char map_name[64] ;
    int color ;
#else
    extern char map_name[] ;
    extern int color ;
#endif

#define NORMAL  1
#define FANCY   2
