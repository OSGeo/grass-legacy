
#include "gis.h"

typedef void cfunc(DCELL *, DCELL *, int);

extern cfunc c_ave;
extern cfunc c_count;
extern cfunc c_divr;
extern cfunc c_max;
extern cfunc c_median;
extern cfunc c_min;
extern cfunc c_mode;
extern cfunc c_stddev;
extern cfunc c_sum;
extern cfunc c_var;
extern cfunc c_reg_m;
extern cfunc c_reg_c;

extern int sort_cell(DCELL *, int);
