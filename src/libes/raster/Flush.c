#include "raster.h"


/*!
 * \brief flush graphics
 *
 * Send all pending graphics commands to
 * the graphics driver. This is done automatically when graphics input requests
 * are made.
 *
 *  \param void
 *  \return int
 */

int R_flush(void)
{
	R_stabilize() ;
	return 0;
}
