
#ifndef lint
static char *SCCSid=	"SCCS version: @(#)   grass2ipw.c   1.5   5/9/91";
#endif

/*
** NAME
** 	grass2ipw -- copy/convert given GRASS cell file to an IPW image file
** 
** SYNOPSIS
**	grass2ipw (ipw_fd, cf_fd, nrows, ncols, divisor, offset, raw)
** 	int ipw_fd, cf_fd;
**	int nrows, ncols;
**	double divisor, offset;
**	int raw;
** 
** DESCRIPTION
** 	grass2ipw reads the given GRASS cell file and converts to given
**	IPW image file.
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

#include "gis.h"
#include "ipw.h"

void
grass2ipw (ipw_fd, cf_fd, nrows, ncols, divisor, offset, raw)

	int		ipw_fd;		/* IPW image file desc		 */
	int		cf_fd;		/* open file descriptors	 */
	int		nrows;		/* # rows in Active window	 */
	int		ncols;		/* # cols in Active window	 */
	double		divisor;	/* data divisor			 */
	double		offset;		/* data offset			 */
	int		raw;		/* flag for raw dat-no F.P. conv */
{
	int		row;		/* row loop counter		 */
	int		col;		/* column loop counter		 */
	fpixel_t	*ipw_buf;	/* IPW row buffer		 */
	CELL		*grass_buf;	/* GRASS cell file row buffer	 */


   /* allocate cell buffer for GRASS cell row */

	grass_buf = G_allocate_cell_buf();

   /* allocate buffer for IPW row */

	if (!raw) {
		ipw_buf = (fpixel_t *) ecalloc (ncols, sizeof(fpixel_t));
		if (ipw_buf == NULL) {
			error ("Unable to allocate IPW row buffer");
		}
	}

   /* convert GRASS cell file to IPW file, row by row */

	for (row = 0; row < nrows; row++) {

		/* read cell file row */

		if (G_get_map_row (cf_fd, grass_buf, row) < 0) {
			error ("Error reading cell file, row %d", row+1);
		}

		/* copy to IPW row */
		/* write row to IPW file */

		if (!raw) {
			for (col = 0; col < ncols; col++) {
	    			ipw_buf[col] =
				(fpixel_t) (grass_buf[col] + offset) / divisor;
			}
			if (fpvwrite (ipw_fd, ipw_buf, ncols) != ncols) {
				error ("Error writing IPW file, line %d", row+1);
			}
		} else {
			for (col = 0; col < ncols; col++) {
	    			grass_buf[col] =
					(grass_buf[col] + offset) / divisor;
			}
			if (pvwrite (ipw_fd, grass_buf, ncols) != ncols) {
				error ("Error writing IPW file, line %d", row+1);
			}
		}


    	}

}
