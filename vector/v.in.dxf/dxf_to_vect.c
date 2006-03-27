/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_buf */
/* last modified by J Moorman
 * 7/23/90
 *
 * Dxf files may or may not contain a HEADER section
 * if the HEADER section is found then the map extent information is
 * extracted from the $EXTMAX and $EXTMIN groups
 *
 * in dxf_header()
 * 1.if the HEADER section is not found in the file and the ENTITY
 *   section is not found the program exits
 * 2.if the HEADER section is found a value of 1 is returned
 *   and the map extent information is searched for in this section
 *   noting that the ENTITY section must be searched for after the
 *   HEADER section has been read.
 *   if the map extents are successfully read in bounds will
 *   equal 4 and all calls to the check_ext() are bypassed.
 * 3.if no HEADER section is found but the  ENTITY section is found
 *   a value of 0 is returned and the dxf_entities ()is bypassed
 *   The map extents will be calculated at appropriate places to
 *   insure that all points read in or calculated (arcs,circles) will
 *   fall within the map location.
 */

#include <stdlib.h>
#include "global.h"

#ifndef DBL_MAX
#define DBL_MAX		9999999999999999999999.9
#define DBL_MIN		-99999999999999999999.9
#endif

static void make_head(struct Map_info *);

static BOUND_BOX ext, dxf_ext;

int dxf_to_vect(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;			/* atoi of line if numeric */
    int bounds = 0;

    fprintf(stderr, _("\nCONVERSION OF %s TO VECTOR FILE:  "), dxf->name);

    if (dxf_find_header(dxf)) {	/* looks for HEADER in file */
	/* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */
	while ((code = dxf_readcode(dxf))) {
	    if (code == -2 || !dxf_fgets(dxf_buf, 256, dxf))	/* EOF */
		return -1;

	    /* only looking for 9 groups  */
	    if (code != 9)
		continue;

	    if (strcmp(dxf_buf, "$EXTMAX") == 0) {
		/* READS IN LINES AND PROCESSES INFORMATION UNTIL A 9
		 * OR A 0 IS READ IN */
		while ((code = dxf_readcode(dxf)) != 9) {
		    if (code == -2 || !dxf_fgets(dxf_buf, 256, dxf))	/* EOF */
			return -1;

		    switch (code) {
		    case 10:
			dxf_ext.E = atof(dxf_buf);
			bounds++;
			break;
		    case 20:
			dxf_ext.N = atof(dxf_buf);
			bounds++;
			break;
		    case 30:
			dxf_ext.T = atof(dxf_buf);
			bounds++;
			break;
		    default:
			break;
		    }
		}
	    }
	    else if (strcmp(dxf_buf, "$EXTMIN") == 0) {
		/* READS IN LINES AND PROCESSES INFORMATION UNTIL A 9
		 * OR A 0 IS READ IN
		 */

		while ((code = dxf_readcode(dxf)) != 9) {
		    if (code == -2 || !dxf_fgets(dxf_buf, 256, dxf))	/* EOF */
			return -1;

		    switch (code) {
		    case 10:
			dxf_ext.W = atof(dxf_buf);
			bounds++;
			break;
		    case 20:
			dxf_ext.S = atof(dxf_buf);
			bounds++;
			break;
		    case 30:
			dxf_ext.B = atof(dxf_buf);
			bounds++;
		    default:
			break;
		    }
		}
	    }
	    else {
		while ((code = dxf_readcode(dxf)) != 9) {
		    if (code == -2 || !dxf_fgets(dxf_buf, 256, dxf))	/* EOF */
			return -1;
		}
	    }

	    if (bounds == 6)
		break;

	}
    }

    ARR_MAX = ARR_INCR;
    ext.E = ext.N = ext.T = DBL_MIN;
    ext.W = ext.S = ext.B = DBL_MAX;

    xpnts = (double *)G_malloc(ARR_MAX * sizeof(double));
    ypnts = (double *)G_malloc(ARR_MAX * sizeof(double));
    zpnts = (double *)G_malloc(ARR_MAX * sizeof(double));

    Points = Vect_new_line_struct();

    while (dxf_fgets(dxf_buf, 256, dxf)) {
	if (strcmp(dxf_buf, "POLYLINE") == 0)
	    add_polyline(dxf, Map);

	else if (strcmp(dxf_buf, "LINE") == 0)
	    add_line(dxf, Map);

	else if (strcmp(dxf_buf, "POINT") == 0)
	    add_point(dxf, Map);

	else if (strcmp(dxf_buf, "ARC") == 0)
	    add_arc(dxf, Map);

	else if (strcmp(dxf_buf, "CIRCLE") == 0)
	    add_circle(dxf, Map);

#ifdef LABEL
	else if (strcmp(dxf_buf, "TEXT") == 0)
	    add_text(dxf, Map);
#endif
    }

    G_free(xpnts);
    G_free(ypnts);
    G_free(zpnts);

    Vect_destroy_line_struct(Points);

    make_head(Map);
    Vect_build(Map, stderr);

    write_done();

    return 0;
}

int check_ext(double x, double y)
{
    if (y < ext.S)
	ext.S = y;
    if (y > ext.N)
	ext.N = y;
    if (x < ext.W)
	ext.W = x;
    if (x > ext.E)
	ext.E = x;

    return 0;
}

static void make_head(struct Map_info *Map)
{
    char *organization;

    if ((organization = getenv("GRASS_ORGANIZATION")))	/* added MN 5/2001 */
	Vect_set_organization(Map, organization);
    else
	Vect_set_organization(Map, "GRASS Development Team");
    Vect_set_date(Map, G_date());
    Vect_set_person(Map, G_whoami());
    Vect_set_map_date(Map, "");
    Vect_set_scale(Map, 2400);
    Vect_set_comment(Map, "");
    Vect_set_zone(Map, 0);
    Vect_set_thresh(Map, 0.0);

    Vect_box_copy(&ext, &(Map->plus.box));

    return;
}
