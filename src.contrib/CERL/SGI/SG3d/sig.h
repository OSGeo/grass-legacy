#ifndef _GISH_
#define _GISH_

#include <stdio.h>
#define CELL int

typedef int FILEDESC;

char *G_store ();
char *G_malloc ();

FILEDESC G_open_cell_old ();

struct Cell_head {
    double north, south, east, west;
    double ns_res, ew_res;
    int rows, cols;
} ;

#endif

#define WIDTH 2089
#define HEIGHT 855

#define W_ROWS 504
#define W_COLS 468
#define X_OFFSET 275
#define Y_OFFSET 350
