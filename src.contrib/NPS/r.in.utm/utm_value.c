
/* %W% %G% */

#include <stdio.h>
#include "utm.h"
#include "rowio.h"
utm_value (north, east, utm, utm_data)
    double north, east;
    struct UTM *utm;
    ROWIO *utm_data;
{
    int row, col;
    char *buf;
    char *rowio_get();

    row_col (utm, north, east, &row, &col);

/* 
fprintf(stderr, "NORTH = %f,  EAST = %f\n", north, east);
fprintf(stderr, "COL = %d,  ROW = %d\n", col, row);
 */
    if (row < 0 || row >= utm->nrows) return 0;
    if (col < 0 || col >= utm->ncols) return 0;

    buf = rowio_get (utm_data, row);
    if (buf != NULL)
   {
      row = value (buf + col*utm->bpc, utm->bpc, utm->sflag);
      return row;
   }
    else
	return 0;
}
