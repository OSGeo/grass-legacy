#include "gis.h"
process_line (fd,cell,e1,n1,e2,n2,window)
    CELL *cell;
    double e1,n1,e2,n2;
    struct Cell_head *window;
{
    int col1,row1,col2,row2;
    double G_easting_to_col(), G_northing_to_row();

    col1 = (int)G_easting_to_col (e1, window);
    col2 = (int)G_easting_to_col (e2, window);
    row1 = (int)G_northing_to_row (n1, window);
    row2 = (int)G_northing_to_row (n2, window);
    profile(fd,cell,col1,row1,col2,row2);
}
