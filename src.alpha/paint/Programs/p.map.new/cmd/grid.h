struct grid 
{
    char *linestyle;
    int colors[9];
    int width;
    int hcolor;
    int hwidth;
	int  textbg;
    char gridon[30];
    char pattern[30];
	int  patternw;
	int patternh;
	double textsize;
} ;

#ifdef MAIN
    struct grid grid;
#else
    extern struct grid grid;
#endif


