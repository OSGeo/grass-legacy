#ifndef PATTERN
#define PATTERN struct _pattern_
PATTERN
{
    char **pat;
    short nrows;
    short ncols;
    short xcenter;
    short ycenter;
    short colors[10];
} ;
#define GLOBAL_PATTERNS 1
#define USER_PATTERNS 2
#define TEMP_PATTERNS 3
#endif
