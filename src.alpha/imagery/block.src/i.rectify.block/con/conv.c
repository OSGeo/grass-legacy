#include "globals.h"
/* conversion routines to convert from view x,y to cell col,row
 * as well as cell col,row to cell east,north
 */
view_to_col(view,x)
    View *view;
{
    return x - view->cell.left;
}

view_to_row(view,y)
    View *view;
{
    return y - view->cell.top;
}

col_to_view(view,col)
    View *view;
{
    return view->cell.left + col;
}

row_to_view(view,row)
    View *view;
{
    return view->cell.top + row;
}

/* in these next 2 routines, location determines if we are
 * converting from center of the cell (location == .5)
 * top or left edge (location == 0.0)
 * bottom or right edge (location == 1.0)
 */

double
row_to_northing(cellhd,row,location)
    struct Cell_head *cellhd;
    double location;
{
    return cellhd->north - (row + location) * cellhd->ns_res;
}

double
col_to_easting(cellhd,col,location)
    struct Cell_head *cellhd;
    double location;
{
    return cellhd->west + (col + location) * cellhd->ew_res;
}

double
northing_to_row(cellhd,north)
    struct Cell_head *cellhd;
    double north;
{
    return  (cellhd->north - north) / cellhd->ns_res;
}

double
easting_to_col(cellhd,east)
    struct Cell_head *cellhd;
    double east;
{
    return  (east - cellhd->west) / cellhd->ew_res;
}

