

#include <stdio.h>
#include "gis.h"
#include "Vect.h"


#define MAX_PTS 10000

/* 2 x 2 determinate  */
#define DET2_2(a,b,c,d)  ( (a*d) - (b*c) )     

#define MIN(a,b) ((a)<(b)?(a):(b))
#define MAX(a,b) ((a)>(b)?(a):(b))
#define LINTERP(a,b,r) ((a)+(r)*((b)-(a)))

typedef int FILEDESC;

typedef double Point2[2];

typedef struct{
    Point2 pnts[MAX_PTS];
    int npts;
    double sum_x, sum_y, sum_xy, sum_x_sq, slope, yinter;
} PointGrp;


extern Point2 *pg_getpoints();
extern Point2 *pg_getpoints_reversed();
extern double pg_y_from_x();
extern double distance2();
extern double distance2_point_to_seg();
extern double xy_distance2_point_to_seg();
extern double xy_distance3_point_to_seg();
extern double lowest_cell_near_point();

