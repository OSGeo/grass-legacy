#include "gis.h"
#include "site.h"

int write_cell (int fd, struct Cell_head *window,
    double north, double east, int quad_size, CELL cat)
{
    CELL cell;
    int row;
    int col;
    int startrow, endrow;
    int startcol, endcol;

    row = (int) northing_to_row (north, window);
    col = (int) easting_to_col (east, window);

#ifdef DEBUG
fprintf (stdout,"point N %lf E %lf at row %d col %d (cat %ld) - ", north, east, row, col, 
 (long) cat);
hitreturn();
#endif DEBUG

    startcol = col - quad_size ;
    endcol   = col + quad_size ;
    if (startcol < 0)
        startcol = 0;
    if (endcol >= window->cols)
        endcol = window->cols - 1;

    if (endcol < startcol)
        return 0;

    startrow = row - quad_size ;
    endrow   = row + quad_size ;
    if (startrow < 0)
        startrow = 0;
    if (endrow >= window->rows)
        endrow = window->rows - 1;

    if (endrow < startrow)
        return 0;

#ifdef DEBUG
fprintf (stdout,"rows %d-%d, cols %d-%d\n", startrow, endrow, startcol, endcol);
hitreturn();
#endif DEBUG

    cell = cat;
    for (row = startrow; row <= endrow; row++)
        for (col = startcol; col <= endcol; col++)
            G_put_map_row_random (fd, &cell, row, col, 1);

    return 0;
}
