#include "globals.h"
/* conversion routines to convert from view x,y to cell col,row
 * as well as cell col,row to cell east,north
 */
int view_to_col(View *view,int x)
{
  if (view->cell.configured)
    return x - view->cell.left;
  else if (view->vect.configured)
    return x - view->vect.left;

    return 0;
}

int view_to_row(View *view,int y)
{
  if (view->cell.configured)
    return y - view->cell.top;
  else if (view->vect.configured)
    return y - view->vect.top;

    return 0;
}

int col_to_view(View *view,int col)
{
  if (view->cell.configured)
    return view->cell.left + col;
  else if (view->vect.configured)
    return view->vect.left + col;

    return 0;
}

int row_to_view(View *view,int row)
{
  if (view->cell.configured)
    return view->cell.top + row;
  else if (view->vect.configured)
    return view->vect.top + row;

    return 0;
}

/* in these next 2 routines, location determines if we are
 * converting from center of the cell (location == .5)
 * top or left edge (location == 0.0)
 * bottom or right edge (location == 1.0)
 */

double row_to_northing(struct Cell_head *cellhd,int row,double location)
{
    return cellhd->north - (row + location) * cellhd->ns_res;
}

double col_to_easting(struct Cell_head *cellhd,int col, double location)
{
    return cellhd->west + (col + location) * cellhd->ew_res;
}

double northing_to_row(struct Cell_head *cellhd, double north)
{
    return  (cellhd->north - north) / cellhd->ns_res;
}

double easting_to_col(struct Cell_head *cellhd, double east)
{
    return  (east - cellhd->west) / cellhd->ew_res;
}

