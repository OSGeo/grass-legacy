#include "gis.h"
#include "local_proto.h"

int process_line (int fd, CELL *cell,
    double e1, double n1, double e2, double n2, struct Cell_head *window)
{
    int col1,row1,col2,row2;

    col1 = (int)G_easting_to_col (e1, window);
    col2 = (int)G_easting_to_col (e2, window);
    row1 = (int)G_northing_to_row (n1, window);
    row2 = (int)G_northing_to_row (n2, window);
    profile(fd,cell,col1,row1,col2,row2);

    return 0;
}
