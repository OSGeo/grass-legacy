
#ifndef lint
static char *SCCSid=	"SCCS version: %Z%   %M%   %I%   %G%";
#endif

/*
** NAME
** 	headers -- write IPW headers
** 
** SYNOPSIS
**	#include "gis.h"
**
**	headers (fdo, raw, override, got_lqh, lqh_fd, cellhd, range, units,
**		 divisor, offset)
** 	int fdo;
**	int raw;
**	int override;
**	int got_lqh;
**	int lqh_fd;
**	struct Cell_head, cellhd;
**	struct Range range;
**	char *units;
**	double divisor, offset;
** 
** DESCRIPTION
** 	headers initializes and	writes the BIH and GEO IPW headers from th
**	given GRASS cell header.  If raw is not set or a header
**	file is supplied, a linear quantization (LQ) header is also written
**	to the output file.
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
#include "fpio.h"
#include "geoh.h"
#include "gethdrs.h"
#include "lqh.h"

static char *meters = "meters";
static char *feet = "feet";
static char *degrees = "degrees";
static char *unknown = "unknown";


void
headers (fdo, raw, override, got_lqh, lqh_fd, cellhd, range, units,
	 divisor, offset)
	int		fdo;		/* file descriptor for ipw image */
	int		raw;		/* true for raw (no FP) output   */
	int		override;	/* true to override GEO origin	 */
	int		got_lqh;	/* true if LQH was supplied      */
	int		lqh_fd;		/* file descriptor for LQH       */
	struct Cell_head cellhd;	/* GRASS file cell header	 */
	struct Range	range;		/* GRASS data ranges structure   */
	char	       *units;		/* units string			 */
	double		divisor;	/* data divisor			 */
	double		offset;		/* data offset			 */
{
	BIH_T         **i_bihpp;	/* -> header file BIH array	 */
	BIH_T         **o_bihpp;	/* -> output BIH array		 */
	GEOH_T        **i_geohpp;	/* -> header file GEOH array	 */
	GEOH_T        **o_geohpp;	/* -> output GEOH array		 */
	LQH_T         **i_lqhpp;	/* -> header file LQH            */
	LQH_T         **o_lqhpp;	/* -> output LQH array		 */
	int             nbits;		/* # bits per pixel		 */
	pixel_t         ival[3];	/* break-points for LQH		 */
	fpixel_t        fval[3];	/* data values at break-points	 */
	double          bline;		/* GEOH coords for first line	 */
	double          bsamp;		/* GEOH coords for first sample	 */
	double          dline;		/* spacing between lines	 */
	double          dsamp;		/* spacing between samples	 */
	char	       *proj;		/* -> name of GRASS projection   */
	char	       *punits;		/* -> name of projection units	 */
	char		zone_label[80]; /* buffer for UTM zone label	 */
	int		punits_id;	/* projection units ID		 */
	int		min;		/* minimum value for LQ		 */
	int		max;		/* maximum value for LQ		 */

	static GETHDR_T h_lqh = {LQH_HNAME, (ingest_t) lqhread};
	static GETHDR_T h_geoh = {GEOH_HNAME, (ingest_t) geohread};
	static GETHDR_T *request[] = {&h_lqh, &h_geoh, 0};


   /* read BIH, LQH and GEOH from supplied header file, if there is one */
   /* duplicate BIH for output image */

	if (got_lqh) {
		i_bihpp = bihread (lqh_fd);
		if (i_bihpp == NULL) {
			error ("can't read BIH of supplied LQH");
		}

		if (bih_nbands (i_bihpp[0]) > 1)
			error ("input header has more than 1 band");

		if ((bih_nlines (i_bihpp[0]) != cellhd.rows) ||
		    (bih_nsamps (i_bihpp[0]) != cellhd.cols))
			error ("header file has different dimensions than map layer");

		o_bihpp = bihdup (i_bihpp);
		if (bihwrite (fdo, o_bihpp) == ERROR) {
			error ("can't write basic image header");
		}

		gethdrs (lqh_fd, request, 1, fdo);
		i_lqhpp = (LQH_T **) hdr_addr(h_lqh);
		i_geohpp = (GEOH_T **) hdr_addr(h_geoh);
		nbits = hnbits (lqh_fd, 0);
	}

	
   /* Create and write BIH. Set nbits large enough to hold data values
    * accurate to the input quantization, unless LQH header is provided
    * as input */

	if (!got_lqh || i_lqhpp == NULL) {
		if (range.nmin != 0) {
			min = range.nmin;
			if (range.pmax != 0) {
				max = range.pmax;
			} else {
				max = -range.nmax;
			}
			nbits = hbit ((unsigned) (1 + max - min));
			if (max < (pow2(nbits) - 1 + min))
				max = pow2(nbits) - 1 + min;
		} else {
			min = 0;
			max = range.pmax;
			nbits = hbit ((unsigned) max);
			if (max < pow2(nbits) - 1)
				max = pow2(nbits) - 1;
		}

		o_bihpp = (BIH_T **) hdralloc (1, sizeof(BIH_T *), fdo,
				      BIH_HNAME);
		o_bihpp[0] = bihmake (0, nbits, (STRVEC_T *) NULL,
			     	(STRVEC_T *) NULL, (BIH_T *) NULL,
			     	cellhd.rows, cellhd.cols, 1);

		if (bihwrite (fdo, o_bihpp) == ERROR) {
			error ("can't write basic image header");
		}
	}

   /* create geodetic header or duplicate GEO header from header file */

	if (!override) {
		bline = cellhd.north - cellhd.ns_res / 2;
		bsamp = cellhd.west + cellhd.ew_res / 2;
	} else {
		bline = cellhd.north;
		bsamp = cellhd.west;
	}
	dline = -cellhd.ns_res;
	dsamp = cellhd.ew_res;
	if (!got_lqh || i_geohpp == NULL) {

		switch (cellhd.proj) {
		   case PROJECTION_XY:
        		proj = "x,y";
			punits = unknown;
			break;
		   case PROJECTION_UTM:
        		proj = "UTM";
			punits = meters;
			break;
		   case PROJECTION_SP:
        		proj = "State Plane";
			punits = feet;
			break;
		   case PROJECTION_LL:
        		proj = "Latitude-Longitude";
			punits = degrees;
			break;
		   default:
        		proj = NULL;
			break;
		}

		if (cellhd.proj == PROJECTION_UTM) {
			sprintf (zone_label, "%s zone %d", proj, cellhd.zone);
			proj = zone_label;
		}

		o_geohpp = (GEOH_T **) hdralloc (1, sizeof(GEOH_T *), fdo,
						GEOH_HNAME);
		o_geohpp[0] = geohmake (bline, bsamp, dline, dsamp,
				punits, proj);

	} else {
		if ((geoh_bline (i_geohpp[0]) != bline) ||
		    (geoh_bsamp (i_geohpp[0]) != bsamp) ||
		    (geoh_dline (i_geohpp[0]) != dline) ||
		    (geoh_dsamp (i_geohpp[0]) != dsamp)) {
			warn ("Geographic region and/or resolution of header file and map layer\ndo not match - using given header values");
		}
		o_geohpp = geohdup (i_geohpp, 1);
	}

   /* write GEO header to IPW file */

	if (geohwrite (fdo, o_geohpp) == ERROR) {
		error("can't write GEO header");
	}

   /* Unless raw output desired or input linear quantization header
    * provided, determine from range of data */

	if (got_lqh && i_lqhpp != NULL) {
		if ((o_lqhpp = lqhdup(i_lqhpp, 1)) == NULL) {
			error("can't duplicate input LQH");
		}
		if (units != NULL &&
		    (lqh_units(o_lqhpp[0]) = hstrdup (units,
					     LQH_HNAME, 0)) == NULL) {
			error("can't duplicate units string");
		}
	} else if (!raw) {
		ival[0] = 0;
		ival[1] = pow2 (nbits) - 1;
		if (divisor > 0) {
			fval[0] = (min + offset) / divisor;
			fval[1] = (max + offset) / divisor;
		} else {
			fval[0] = (max + offset) / divisor;
			fval[1] = (min + offset) / divisor;
		}

		o_lqhpp = (LQH_T **) hdralloc(1, sizeof(LQH_T *), fdo,
					      LQH_HNAME);
		o_lqhpp[0] = lqhmake (nbits, 2, ival, fval, units,
				     (char *) NULL);
	}

	if (!raw || got_lqh) {
		if (lqhwrite (fdo, o_lqhpp) == ERROR) {
			error("can't write LQH header");
		}
	}

   /* done - get ready for image output */

	if (boimage (fdo) == ERROR) {
		error ("can't terminate header output");
	}
}
