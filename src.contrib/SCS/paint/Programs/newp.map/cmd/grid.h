struct grid 
{
    char *linestyle;
    int colors[9];
    int width;
    int hcolor;
    int hwidth;
    char gridon[30];
    char pattern[30];
	int  patternw;
	int patternh;
} ;

#ifdef MAIN
    struct grid grid;
#else
    extern struct grid grid;
#endif


