struct regline
{
    int count;
    char *other;
} ;

#ifdef MAIN
    struct regline regline;
#else
    extern struct regline regline;
#endif




struct dline
{
    char *linestyle;
    int colors[9];
    int width;
    int hcolor;
    int hwidth;
} ;

#ifdef MAIN
    struct dline dline;
#else
    extern struct dline dline;
#endif


