#include	"gis.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

struct stats_table
{
    long count;
    double area;
} ;

GLOBAL struct Cell_head window;

GLOBAL char *title1, *title2;

GLOBAL double window_cells;
GLOBAL double window_area;

GLOBAL struct stats_table *table;
GLOBAL long     *catlist1, *catlist2;
GLOBAL int      no_data1, no_data2; 
GLOBAL int      Rndex,Cndex;
GLOBAL char	*dumpname;
GLOBAL char     *statname;
GLOBAL FILE	*dumpfile;

GLOBAL char	map1name[30], map2name[30];
GLOBAL char    *mapset1,     *mapset2;
GLOBAL int      ncat1, ncat2;

GLOBAL char *fill, *midline;
