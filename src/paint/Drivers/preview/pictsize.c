#include "P.h"
Ppictsize (rows, cols)
{
    char *malloc();
    if (data)
	error ("Multiple pictures not allowed");
    if (cols > window_ncols || rows > window_nrows)
	error ("Picture size is too large for the graphics frame");
    data = (int *) malloc (cols * sizeof(int));
    picture_ncols = cols;
    picture_nrows = rows;

    current_row = top + (window_nrows - rows)/2 + 1;
    left_edge = left + (window_ncols - cols)/2 + 1;
}
