
#ifndef __GLOBALS_H_
#define __GLOBALS_H_

#include "gis.h"

extern volatile int floating_point_exception;
extern volatile int floating_point_exception_occurred;

extern int overflow_occurred;

extern int current_row;
extern struct Cell_head current_region;

extern int rows, columns;

#endif /* __GLOBALS_H_ */

