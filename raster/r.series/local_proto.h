
#include "gis.h"

typedef DCELL cfunc(DCELL *, int);

extern cfunc c_ave;
extern cfunc c_divr;
extern cfunc c_max;
extern cfunc c_median;
extern cfunc c_min;
extern cfunc c_mode;
extern cfunc c_stddev;
extern cfunc c_sum;
extern cfunc c_var;

extern int sort_cell(DCELL *array,int n);
extern double d_var(DCELL *values, int n);
