/** Modified by: Janne Soimasuo August 1994 line_cat added **/
/** Modified by: Radim Blazek Jan 2000 acolor, label added **/
#include "color.h"
#define MAXVECTORS 20


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
