#include "gis.h"
/*
 **********************************************************************
 *
 *   G_maskfd()
 *
 *   Returns the file descriptor opened for the current mask
 *
 *   returns:    fd number if mask is in use
 *               -1        if no mask is in use
 **********************************************************************/

#include "G.h"
/*
 * open mask file and return file descriptor
 */

/*!
 * \brief test for current mask
 *
 * returns file descriptor
 * number if MASK is in use and -1 if no MASK is in use.
 *
 *  \param void
 *  \return int
 */

int G_maskfd ()
{
	G__check_for_auto_masking () ;

	return G__.auto_mask > 0 ? G__.mask_fd : -1;
}
