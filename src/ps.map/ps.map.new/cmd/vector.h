/** Modified by: Janne Soimasuo August 1994 line_cat added **/
/** Modified by: Radim Blazek Jan 2000 acolor, label added **/
#include "color.h"
#define PI 3.14159265   
#define MAXVECTORS 20

/* line justification */
#define LINE_REF_CENTER 0
#define LINE_REF_LEFT 1
#define LINE_REF_RIGHT 2

/* draw line */
#define LINE_DRAW_LINE 1
#define LINE_DRAW_HIGHLITE 2

/* construct_path() */
#define START_PATH  0 
#define ADD_TO_PATH 1
#define CLOSE_PATH  2
#define WHOLE_PATH  3

struct vector
{
    int cur;
    int count;
    double x, y;
    int fontsize;
    char *font;
    char *linestyle[MAXVECTORS];
    char *setdash[MAXVECTORS];
    int colors[MAXVECTORS][9];
    double width[MAXVECTORS];
    double cwidth[MAXVECTORS];
    double offset[MAXVECTORS];
    double coffset[MAXVECTORS];
    int ref[MAXVECTORS];
    int  hcolor[MAXVECTORS];
    double hwidth[MAXVECTORS];
    char masked[MAXVECTORS];
    char *name[MAXVECTORS];
    char *mapset[MAXVECTORS];
    int line_cat[MAXVECTORS];
    char area[MAXVECTORS];
    RGB acolor[MAXVECTORS];
    char *label[MAXVECTORS];
    int lpos[MAXVECTORS];
} ;

#ifdef MAIN
    struct vector vector;
#else
    extern struct vector vector;
#endif
