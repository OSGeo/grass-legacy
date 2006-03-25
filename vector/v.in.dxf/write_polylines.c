#include "global.h"

/* PRINTS OUT THE POLYLINE VERTEX DATA TO FILE DESIGNATED AS layer_fd */
int write_polylines(DXF_DIG * layer_fd, int arr_size)
{
    struct line_cats *cats;

    Vect_copy_xyz_to_pnts(Points, xinfo, yinfo, zinfo, arr_size);
    /* TODO */
    cats = Vect_new_cats_struct();
    Vect_write_line(layer_fd->Map, GV_LINE, Points, cats);
    Vect_destroy_cats_struct(cats);

    return 0;
}
