/* this code is obsolete */
#include "graphics.h"

window_row (north)
    double north;
{
    double t;
    int row;
/*
 * convert the window north value to window row
 * note: negative values are rounded down
 */
    row = t = (graphics.window.north - north) / graphics.window.ns_res;
    if (t < row)
	row--;
    
    return row;
}

window_col (east)
    double east;
{
    double t;
    int col;
/*
 * convert the window east value to window col
 * note: negative values are rounded down
 */
    col = t = (east - graphics.window.west) / graphics.window.ew_res;
    if (t < col)
	col--;

    return col;
}
