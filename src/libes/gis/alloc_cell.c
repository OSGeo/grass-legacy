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
#include <math.h>
#include "gis.h"

/* convert type "RASTER_MAP_TYPE" into index */
#define F2I(map_type) \
	(map_type == CELL_TYPE ? 0 : (map_type == FCELL_TYPE ? 1 : 2))

static int type_size[3] = {sizeof(CELL), sizeof(FCELL), sizeof(DCELL)};

int G_raster_size (data_type)
    RASTER_MAP_TYPE data_type;
{
    return (type_size[F2I(data_type)]);
}

CELL *
G_allocate_cell_buf ()
{
    return (CELL *) G_calloc (G_window_cols() + 1, sizeof(CELL));
}
void *
G_allocate_raster_buf (data_type)
RASTER_MAP_TYPE data_type;
{
    return (void *) G_calloc (G_window_cols() + 1, G_raster_size(data_type));
}
CELL *
G_allocate_c_raster_buf ()
{
    return (CELL *) G_calloc (G_window_cols() + 1, sizeof(CELL));
}
FCELL *
G_allocate_f_raster_buf ()
{
    return (FCELL *) G_calloc (G_window_cols() + 1, sizeof(FCELL));
}
DCELL *
G_allocate_d_raster_buf ()
{
    return (DCELL *) G_calloc (G_window_cols() + 1, sizeof(DCELL));
}
char *
G_allocate_null_buf ()
{
    return (char *) G_calloc (G_window_cols() + 1, sizeof(char));
}

unsigned char *
G__allocate_null_bits (cols)
   int cols;
{
    return (unsigned char *) G_calloc (G__null_bitstream_size(cols)+1, 
                               sizeof(unsigned char));
}
int
G__null_bitstream_size(cols)
  int cols;
{
  if (cols <= 0 ) return -1;
  return (cols/8 + (cols % 8 != 0));
}
