/* %W% %G% */
#include "gis.h"
#include "utm.h"

row_col (utm, north, east, row, col)
    struct UTM *utm;
    double north, east;
    int *row, *col;
{
/*
fprintf(stderr, "UE = %f, East = %f, UN = %f, North = %f\n", utm->west, east, utm->north, north);
    fprintf(stderr, "RES = %f,  %f\n", utm->ns_res, (utm->north - north) / utm->ns_res);
*/
    *row = (utm->north - north) / utm->ns_res;
    *col = (utm->west - east) / utm->ew_res;
/*
fprintf(stderr, " 1  ROW = %d,  COL = %d\n", *row, *col);
*/

}
