#include "gis.h"
#define NUL_STR "\0"
#define NUL_EXP  0

#define LIST_EXPR      1
#define PRNT_EXPR      2
#define ASSGN_EXPR     3
#define CHOOS_EXPR     4
#define UNCHOOS_EXPR   5
#define SAVE_EXPR      6
#define RECOV_EXPR     7
#define EXECUT_EXPR    8
#define QUIT_EXPR      9
#define MISC_EXPR     10
#define HELP_EXPR     11
#define NULL_EXPR     12
#define ERR_EXPR      13
#define COLR_EXPR     14

#define ADD			0
#define MULT		1

#define MAX_MAPS 6
struct mapdef
{
    int used ;
    char name[20] ;
    char mapset[20] ;
    struct Histogram histo ;
} ;

#ifdef MAIN

struct mapdef mapdef[MAX_MAPS] ;
int analysis_type = ADD ;
long normalize = 0 ;
char map_name[20] ;
char input_buf[256] ;

#else

extern struct mapdef mapdef[] ;
extern analysis_type ;
extern long normalize ;
extern char map_name[] ;
extern char input_buf[] ;

#endif MAIN

#define GETS mygets(input_buf)
char *mygets() ;
