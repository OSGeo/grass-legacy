#include "global.h"
write_matrix (outfd, row, col)
{
    int n;

    select_target_env();
    for (n=0; n < matrix_rows; n++)
	G_put_map_row_random (outfd, cell_buf[n], row++, col, matrix_cols);
    select_current_env();
}
