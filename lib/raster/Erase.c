
#include "raster.h"
#include "graph.h"


/*!
 * \brief erase screen
 *
 * Erases the entire screen to black.
 *
 *  \param void
 *  \return int
 */

int R_erase(void)
{
	_send_ident(ERASE) ;

	return 0;
}
