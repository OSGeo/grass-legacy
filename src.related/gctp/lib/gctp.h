/* GCTP header file
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

/* common block structure declarations */

struct _ellpz0_1 {
    double az, ez, esz, e0z, e1z, e2z, e3z, e4z;
};

struct _sphrz0_1 {
    double azz;
};

extern struct _ellpz0_1  ellpz0;
extern struct _sphrz0_1  sphrz0;

#include "proto.h"

/* end gctp.h */
