/* These next defines determine the size of the sub-window that will
 * be held in memory.  Larger values will require
 * more memory (but less i/o) If you increase these values, keep  in
 * mind that although you think the i/o will decrease, system paging
 * (which goes on behind the scenes) may actual increase the i/o.
 */

#define NROWS 64
#define NCOLS 256

/* do not modify past this point */

#ifndef GLOBAL
#define GLOBAL extern
#endif

#include "imagery.h"
#include "rowcol.h"

#define IDX int

GLOBAL ROWCOL row_map[NROWS][NCOLS] ;
GLOBAL ROWCOL col_map[NROWS][NCOLS] ;
GLOBAL ROWCOL row_min[NROWS];
GLOBAL ROWCOL row_max[NROWS];
GLOBAL ROWCOL row_left[NROWS];
GLOBAL ROWCOL row_right[NROWS];
GLOBAL IDX row_idx[NROWS];
GLOBAL int matrix_rows, matrix_cols;

GLOBAL CELL **cell_buf;

GLOBAL int *ref_list;
GLOBAL char **new_name;
GLOBAL struct Ref ref;

/* georef coefficients */

GLOBAL double E12[10], N12[10];
GLOBAL double E21[10], N21[10];

/* DELETED WITH CRS MODIFICATIONS
GLOBAL double E12a, E12b, E12c, N12a, N12b, N12c;
GLOBAL double E21a, E21b, E21c, N21a, N21b, N21c;
*/

GLOBAL struct Cell_head target_window;
