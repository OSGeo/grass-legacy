#include "gis.h"
extern double d_var();
extern double sqrt();

CELL
c_stddev (values, n)
    register CELL *values;
{
    return (CELL) (sqrt(d_var(values,n)) + .5);
}
