#include <stdio.h>

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int top, bottom, left, right;
GLOBAL int window_nrows, window_ncols;
GLOBAL int *data;
GLOBAL int current_row, top_edge, left_edge;
