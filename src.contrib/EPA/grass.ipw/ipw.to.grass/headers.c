
#ifndef lint
static char *SCCSid=	"SCCS version: %Z%   %M%   %I%   %G%";
#endif

/*
** NAME
** 	headers -- read IPW headers and initialize GRASS cell file header
** 
** SYNOPSIS
**	#include "gis.h"
**
**	headers (fdi, fdm, cellhd, min, max, override)
** 	int fdi, fdm;
**	struct Cell_head *cellhd;
**	fpixel_t *min, *max;
**	int override;
** 
** DESCRIPTION
** 	headers reads the headers of the given IPW image file, ingests
**	the LQ headers for floating point conversion and constructs
**	the cell file header for the GRASS cell file.
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
#include "bih.h"
#include "geoh.h"
#include "lqh.h"
#include "orh.h"
#include "gethdrs.h"
#include "fpio.h"

int
headers (fdi, fdm, cellhd, min, max, override)
	int			 fdi;		/* IPW image file desc   */
	int			 fdm;		/* mask image file desc  */
	struct Cell_head	*cellhd;	/* -> GRASS cell header  */
	fpixel_t	        *min;		/* F.P. min in image     */
	fpixel_t	        *max;		/* F.P. max in image     */
	int			override;	/* flag to override cell */
						/* half-pixel offset	 */
{
	int		class;			/* class loop counter    */
	int		cat;			/* GRASS category        */
	BIH_T		**bihpp;		/* -> Basic Image Header */
	GEOH_T		**geohpp;		/* -> geo header         */
	LQH_T		**lqhpp;		/* -> LQH header	 */
	char		label[256];		/* category label        */
	char		units[256];		/* data units string	 */
	fpixel_t       *fpmin;			/* -> min F.P. array	 */
	fpixel_t       *fpmax;			/* -> max F.P. array	 */

	static GETHDR_T h_lqh = {LQH_HNAME, (ingest_t) lqhread};
	static GETHDR_T h_geo = {GEOH_HNAME, (ingest_t) geohread};
	static GETHDR_T h_orh = {ORH_HNAME, (ingest_t) orhread};
	static GETHDR_T *request[] = {&h_lqh, &h_geo, &h_orh, 0};


   /* read BIH of IPW file */

	bihpp = bihread (fdi);
	if (bihpp == NULL) {
		error ("Can't read Basic Image header of IPW file");
	}

   /* read BIH of mask file */

	if (fdm != ERROR) {
		if (bihread (fdm) == NULL) {
			error ("Can't read Basic Image header of mask file");
		}
	}

  /* single-band IPW files only */

	if (hnbands(fdi) > 1) {
		error ("IPW file has multiple bands");
	}

	if (fdm != ERROR) {

		if (hnbands(fdm) != hnbands(fdi) ||
		    hnlines(fdm) != hnlines(fdi) ||
		    hnsamps(fdm) != hnsamps(fdi)) {
			error ("IPW image and mask image are incompatible");
		}
	}

  /* ingest LQH, GEOH, and CRH, check for ORH; skip other headers */

	gethdrs (fdi, request, NO_COPY, ERROR);
	if (fdm != ERROR) {
		skiphdrs (fdm);
	}

	units[0] = '\0';
	if ((lqhpp = (LQH_T **) hdr_addr(h_lqh)) == NULL) {
		warn ("IPW file has no LQH; raw values used\n");
		*min = 0;
		*max = pow2 (bih_nbits(bihpp[0])) - 1;
	} else {
		if (lqh_units (lqhpp[0]) != NULL) {
			strcpy (units, lqh_units(lqhpp[0]));
		}
		fpmin = fpfmin (fdi);
		fpmax = fpfmax (fdi);
		*min = fpmin[0];
		*max = fpmax[0];
	}

   /* There should not be an ORH - we need standard orientation */

	if (hdr_addr(h_orh) != NULL)
		warn ("IPW file should be standard orientation\n");

   /* Initialize cell header from GRASS active window (proj and zone) */

	G_get_set_window (cellhd);

   /* if IPW file has no GEO header, use GRASS active window */

	if ((geohpp = (GEOH_T **) hdr_addr(h_geo)) == NULL) {

		warn ("IPW image contains no GEO header - using GRASS window");

		if (cellhd->rows != bih_nlines(bihpp[0])) {
			error ("IPW file has %d rows, GRASS window has %d rows",
				bih_nlines(bihpp[0]), cellhd->rows);
		}

		if (cellhd->cols != bih_nsamps(bihpp[0])) {
			error ("IPW file has %d cols, GRASS window has %d cols",
				bih_nsamps(bihpp[0]), cellhd->cols);
		}

	} else {

		cellhd->cols = bih_nsamps(bihpp[0]);
		cellhd->rows = bih_nlines(bihpp[0]);

		cellhd->ns_res = -geoh_dline(geohpp[0]);
		cellhd->ew_res = geoh_dsamp(geohpp[0]);

		if (!override) {
			cellhd->north = geoh_bline(geohpp[0]) +
				cellhd->ns_res / 2.0;
			cellhd->west = geoh_bsamp(geohpp[0]) -
				cellhd->ew_res / 2.0;
		} else {
			cellhd->north = geoh_bline(geohpp[0]);
			cellhd->west = geoh_bsamp(geohpp[0]);
		}

		cellhd->south = cellhd->north - cellhd->rows * cellhd->ns_res;
		cellhd->east = cellhd->west + cellhd->cols * cellhd->ew_res;
		G_set_window (cellhd);
	}
}
