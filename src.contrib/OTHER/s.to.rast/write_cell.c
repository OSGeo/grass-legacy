#include "gis.h"
write_cell (fd, window, north, east, quad_size, cat)
    struct Cell_head *window;
    CELL cat;
{
    CELL cell;
    int count;
    int row;
    int col;
    int startrow, endrow;
    int startcol, endcol;

    float northing_to_row ();
    float easting_to_col ();

    row = (int) northing_to_row (north, window);
    col = (int) easting_to_col (east, window);

#ifdef DEBUG
printf("point N %d E %d at row %d col %d (cat %ld) - ", north, east, row, col, 
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
printf ("rows %d-%d, cols %d-%d\n", startrow, endrow, startcol, endcol);
hitreturn();
#endif DEBUG

    cell = cat;
    for (row = startrow; row <= endrow; row++)
        for (col = startcol; col <= endcol; col++)
            G_put_map_row_random (fd, &cell, row, col, 1);
}
