
#ifndef lint
static char *SCCSid=	"SCCS version: %Z%   %M%   %I%   %G%";
#endif

/*
** NAME
** 	ipw2grass -- copy/convert given IPW image file to GRASS cell file
** 
** SYNOPSIS
**	#include "gis.h"
**
**	ipw2grass (ipw_fd, cf_fd, m_fd, cellhd, mult, offset)
** 	int ipw_fd, cf_fd, m_fd;
**	struct Cell_head cellhd;
**	double mult, offset;
** 
** DESCRIPTION
** 	ipw2grass reads the given IPW image file and converts to given
**	GRASS cell file, multiplying all data values by given multiplier
**	and adding the given offset.
** 
** RESTRICTIONS
** 
** RETURN VALUE
** 
** GLOBALS ACCESSED
** 
** ERRORS
** 
** WARNINGS
** 
** APPLICATION USAGE
** 
** FUTURE DIRECTIONS
** 
** BUGS
*/

#include <math.h>
#include "gis.h"
#include "ipw.h"

void
ipw2grass (ipw_fd, cf_fd, m_fd, cellhd, mult, offset)
	int		ipw_fd;		/* IPW file descriptor		 */
	int		cf_fd;		/* GRASS cell file desc		 */
	int		m_fd;		/* mask file descriptor		 */
	struct Cell_head cellhd;	/* Header of Active window	 */
	double		mult;		/* IPW data multiplier		 */
	double		offset;		/* data offset			 */
{
	int		nrows;		/* # rows in images		 */
	int		ncols;		/* # columns in images		 */
	int		row;		/* row loop counter		 */
	int		col;		/* column loop counter		 */
	fpixel_t	*ipw_buf;	/* IPW row buffer		 */
	pixel_t		*mask_buf;	/* mask row buffer		 */
	CELL		*grass_buf;	/* GRASS cell file row buffer	 */



	nrows = cellhd.rows;
	ncols = cellhd.cols;

   /* allocate cell buffer for GRASS cell row */

	grass_buf = G_allocate_cell_buf();

   /* allocate buffer for IPW row */

	ipw_buf = (fpixel_t *) ecalloc (ncols, sizeof(fpixel_t));
	if (ipw_buf == NULL) {
		error ("Unable to allocate IPW row buffer");
	}

   /* allocate buffer for mask row */

	mask_buf = (pixel_t *) ecalloc (ncols, sizeof(pixel_t));
	if (mask_buf == NULL) {
		error ("Unable to allocate mask row buffer");
	}

   /* convert IPW file to GRASS cell file, row by row */

	for (row = 0; row < nrows; row++) {

		/* read IPW row */

		if (fpvread (ipw_fd, ipw_buf, ncols) != ncols) {
			error ("Error reading IPW file, line %d", row+1);
		}

		/* read mask row */

		if (m_fd != ERROR) {
			if (pvread (m_fd, mask_buf, ncols) != ncols) {
				error ("Error reading mask file, line %d", row+1);
			}
		}

		/* copy IPW row, multiplying by multiplier and adding offset */

		for (col = 0; col < ncols; col++) {
			if (m_fd != ERROR && mask_buf[col] == 0) {
				grass_buf[col] = 0;
			} else {
	    			grass_buf[col] =
				   (CELL) anint ((double)
					(ipw_buf[col] * mult + offset));
			}
		}

		/* write row to GRASS cell file */

		G_put_map_row (cf_fd, grass_buf);
    	}

}
