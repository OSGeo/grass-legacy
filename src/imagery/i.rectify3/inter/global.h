#include "defs.h"

/** #include "imagery.h"  **/
#include "rowcol.h"

/* These next defines determine the size of the sub-window that will
 * be held in memory.  Larger values will require
 * more memory (but less i/o) If you increase these values, keep  in
 * mind that although you think the i/o will decrease, system paging
 * (which goes on behind the scenes) may actual increase the i/o.
 */

#define NROWS 128
#define NCOLS 128

/* do not modify past this point */

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define IDX int

GLOBAL ROWCOL row_map[NROWS][NCOLS] ;
GLOBAL ROWCOL col_map[NROWS][NCOLS] ;
GLOBAL ROWCOL row_min[NROWS];
GLOBAL ROWCOL row_max[NROWS];
GLOBAL ROWCOL row_left[NROWS];
GLOBAL ROWCOL row_right[NROWS];
GLOBAL IDX row_idx[NROWS];
GLOBAL int matrix_rows, matrix_cols;


/* cell_buf will contain source image data */
GLOBAL CELL **cell_buf;
GLOBAL int temp_fd;
GLOBAL RASTER_MAP_TYPE map_type;
GLOBAL char *temp_name;

/* new_name will contain the names of the rectified images */
GLOBAL int *ref_list;
GLOBAL char **new_name;


/* georef coefficients */
GLOBAL double E12[10], N12[10];
GLOBAL double E21[10], N21[10];

/* DELETED WITH CRS MODIFICATIONS
GLOBAL double E12a, E12b, E12c, N12a, N12b, N12c;
GLOBAL double E21a, E21b, E21c, N21a, N21b, N21c;
*/

GLOBAL struct Cell_head target_window;

/* These things added for ortho photo and TM */
GLOBAL Rectify_Group group;




