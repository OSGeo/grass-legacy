#include "glob.h"

readrow(fd, buf, row, col, depth, nrows, ncols, ndepths)
    void *fd;
    double *buf;
{
    int i;
    float x;

    if (map_is_fp(fd)) {
        for (i=0;i<ncols;i++) {
            if (row+current_row >= nrows || row+current_row < 0)
                SETNULL_D(buf++);
            else if (col+i >= ncols || col+i < 0)
                SETNULL_D(buf++);
            else if (depth+current_depth >= ndepths || depth+current_depth < 0)
                SETNULL_D(buf++);
            else {
                G3d_getValueRegion (fd, i+col, row+current_row, depth+current_depth, &x, G3D_FLOAT);
                *buf++=(double) x;
            }
        }
    }
    else {
        for (i=0;i<ncols;i++) {
            if (((row+current_row) >= nrows) || ((row+current_row) < 0))
                SETNULL_D(buf++);
            else if (((col+i) >= ncols) || ((col+i) < 0))
                SETNULL_D(buf++);
            else if (((depth+current_depth) >= ndepths) || ((depth+current_depth) < 0))
                SETNULL_D(buf++);
else
                G3d_getValueRegion (fd, i+col, row+current_row, depth+current_depth, buf++, G3D_DOUBLE);
        }
    }
    return 1;
}
