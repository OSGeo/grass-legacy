/**********************************************************************
 *
 *   CELL *G_allocate_cell_buf ()
 *
 *   returns buffer to accomodate the reading in of one line of
 *   data from a cell file.
 *
 *   parms:
 *      none
 *
 *   returns:
 *      pointer to a buffer.
 *
 *   notes:
 *      generally used with each open cell file
 **********************************************************************/
#include "gis.h"

CELL *
G_allocate_cell_buf ()
{
    return (CELL *) G_calloc (G_window_cols() + 1, sizeof(CELL));
}
