#include <math.h>
#include "gis.h"

/* convert type "RASTER_MAP_TYPE" into index */
#define F2I(map_type) \
	(map_type == CELL_TYPE ? 0 : (map_type == FCELL_TYPE ? 1 : 2))

static int type_size[3] = {sizeof(CELL), sizeof(FCELL), sizeof(DCELL)};


/*!
 * \brief return size of raster
 *
 * If <em>data_type</em> is CELL_TYPE, returns sizeof(CELL)
 * If <em>data_type</em> is FCELL_TYPE, returns sizeof(FCELL)
 * If <em>data_type</em> is DCELL_TYPE, returns sizeof(DCELL)
 *
 *  \param data_type
 *  \return int
 */

int G_raster_size (data_type)
    RASTER_MAP_TYPE data_type;
{
    return (type_size[F2I(data_type)]);
}


/*!
 * \brief allocate a raster buffer
 *
 * This
 * routine allocates a buffer of type CELL just large enough to hold one row of
 * raster data (based on the number of columns in the active region).
 * \code
CELL *cell;
 * cell = G_allocate_cell_buf(void);
\endcode
 * If larger buffers are required, the routine <i>G_malloc</i> can be used.
 * If sufficient memory is not available, an error message is printed and exit() is called.
 * Returns pointer to a buffer. The routine is generally used with each open cell file.
 *
 *  \param void
 *  \return CELL * 
 */

CELL *
G_allocate_cell_buf ()
{
    return (CELL *) G_calloc (G_window_cols() + 1, sizeof(CELL));
}

/*!
 * \brief Allocate raster array 
 *
 * Allocate an array of CELL, FCELL, or DCELL (depending on <em>data_type</em>) based on the number of columns in the current region.
 *
 *  \param data_type
 *  \return void * 
 */

void *
G_allocate_raster_buf (data_type)
RASTER_MAP_TYPE data_type;
{
    return (void *) G_calloc (G_window_cols() + 1, G_raster_size(data_type));
}

/*!
 * \brief Allocate an array of CELL
 *
 * Allocate an array of CELL
 * based on the number of columns in the current region.
 *
 *  \return CELL * 
 */

CELL *
G_allocate_c_raster_buf ()
{
    return (CELL *) G_calloc (G_window_cols() + 1, sizeof(CELL));
}

/*!
 * \brief Allocate an array of FCELL
 *
 * Allocate an array of FCELL
 * based on the number of columns in the current region.
 *
 *  \return FCELL * 
 */

FCELL *
G_allocate_f_raster_buf ()
{
    return (FCELL *) G_calloc (G_window_cols() + 1, sizeof(FCELL));
}

/*!
 * \brief Allocate an array of DCELL
 *
 * Allocate an array of DCELL
 * based on the number of columns in the current region.
 *
 *  \return DCELL * 
 */

DCELL *
G_allocate_d_raster_buf ()
{
    return (DCELL *) G_calloc (G_window_cols() + 1, sizeof(DCELL));
}

/*!
 * \brief Allocate an array of char
 *
 * Allocate an array of char based on
 * the number of columns in the current region.
 *
 *  \return char * 
 */

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
