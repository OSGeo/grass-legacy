#include "raster.h"
#include "graph.h"


/*!
 * \brief select floating color table
 *
 * Selects a
 * float color table to be used for subsequent color calls. It is expected that
 * the user will follow this call with a call to erase and reinitialize the
 * entire graphics screen.
 * Returns 0 if successful, non-zero if unsuccessful.
 *
 *  \param void
 *  \return int
 */

int R_color_table_float(void)
{
	int i ;
	_send_ident(COLOR_TABLE_FLOAT) ;
	_get_int(&i) ;
	return(i) ;
}


/*!
 * \brief select fixed color table
 *
 * Selects a fixed
 * color table to be used for subsequent color calls. It is expected that the
 * user will follow this call with a call to erase and reinitialize the entire
 * graphics screen.
 * Returns 0 if successful, non-zero if unsuccessful.
 *
 *  \param void
 *  \return int
 */

int R_color_table_fixed(void)
{
	int i ;
	_send_ident(COLOR_TABLE_FIXED) ;
	_get_int(&i) ;
	return(i) ;
}

int R_color_offset(int n)
{
	int i ;
	i = n ;
	_send_ident(COLOR_OFFSET) ;
	_send_int(&i) ;

	return 0;
}
