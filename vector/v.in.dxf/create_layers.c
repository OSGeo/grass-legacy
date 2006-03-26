/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
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
 *   if the map extents are successfully read in BOUNDARIES will
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

static int close_all_layers(void);

int create_layers(FILE * dxf_file)
{
    int code;			/* atoi of line if numeric */
    int header_flag;		/* set to true if HEADER section found */

    /* initialize the boundary indicating variables */
    BOUNDARIES = 0;

    header_flag = dxf_find_header(dxf_file);	/* looks for HEADER in file */
    /*#ifdef JCM */
    if (header_flag) {
	/* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */
	code = dxf_readcode(dxf_file);
	while (code != 0) {
	    if (code == -2)	/* EOF */
		return 0;
	    dxf_fgets(dxf_line, 256, dxf_file);
	    if (feof(dxf_file) != 0)	/* EOF */
		return 0;

	    if (code == 9 && code != 0) {	/* only looking for 9 groups  */
		if (strcmp(dxf_line, "$EXTMAX") == 0) {
		    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 9
		     * OR A 0 IS READ IN */
		    while ((code = dxf_readcode(dxf_file))) {
			if (code == 9)
			    break;
			if (code == -2)	/* EOF */
			    return 0;
			dxf_fgets(dxf_line, 256, dxf_file);
			if (feof(dxf_file) != 0)	/* EOF */
			    return 0;
			switch (code) {
			case 10:
			    XMAX = atof(dxf_line);
			    BOUNDARIES += 1;
			    break;
			case 20:
			    YMAX = atof(dxf_line);
			    BOUNDARIES += 1;
			    break;
			case 30:
			    ZMAX = atof(dxf_line);
			    BOUNDARIES += 1;
			    break;
			default:
			    break;
			}
		    }
		}
		else if (strcmp(dxf_line, "$EXTMIN") == 0) {
		    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 9
		     * OR A 0 IS READ IN
		     */

		    while ((code = dxf_readcode(dxf_file))) {
			if (code == 9)
			    break;
			if (code == -2)	/* EOF */
			    return 0;

			dxf_fgets(dxf_line, 256, dxf_file);
			if (feof(dxf_file) != 0)	/* EOF */
			    return 0;

			switch (code) {
			case 10:
			    XMIN = atof(dxf_line);
			    BOUNDARIES += 1;
			    break;
			case 20:
			    YMIN = atof(dxf_line);
			    BOUNDARIES += 1;
			    break;
			case 30:
			    ZMIN = atof(dxf_line);
			    BOUNDARIES += 1;
			default:
			    break;
			}
		    }
		}
		else {
		    while ((code = dxf_readcode(dxf_file))) {
			if (code == 9)
			    break;
			if (code == -2)	/* EOF */
			    return 0;
			dxf_fgets(dxf_line, 256, dxf_file);
			if (feof(dxf_file) != 0)	/* EOF */
			    return 0;
		    }
		}
	    }

	    if (BOUNDARIES == 6)
		break;

	}
	/* SHOULD CHECK FOR THE RETURN VALUE OF THIS FUNCTION */
	dxf_find_entities(dxf_file);
	/* looks for ENTITIES in file */
    }
    /*#endif */
    xinfo = (double *)G_malloc(ARR_INCR * sizeof(double));
    yinfo = (double *)G_malloc(ARR_INCR * sizeof(double));
    zinfo = (double *)G_malloc(ARR_INCR * sizeof(double));
    ARR_MAX = ARR_INCR;
    n = e = DBL_MIN;
    w = s = DBL_MAX;
    dxf_fgets(dxf_line, 256, dxf_file);
    while (feof(dxf_file) == 0) {
	if (strcmp(dxf_line, polyline) == 0)
	    add_polyline(dxf_file);

	else if (strcmp(dxf_line, line) == 0)
	    add_line(dxf_file);

	else if (strcmp(dxf_line, point) == 0)
	    add_point(dxf_file);

	else if (strcmp(dxf_line, arc) == 0)
	    add_arc(dxf_file);

	else if (strcmp(dxf_line, circle) == 0)
	    add_circle(dxf_file);

#ifdef LABEL
	else if (strcmp(dxf_line, text) == 0)
	    add_labelbox(dxf_file);
#endif

	dxf_fgets(dxf_line, 256, dxf_file);
    }

    return close_all_layers();
}

int check_ext(double x, double y)
{
    if (y < s) {
	s = y;
    }
    if (y > n) {
	n = y;
    }
    if (x < w) {
	w = x;
    }
    if (x > e) {
	e = x;
    }

    return 0;
}

static int close_all_layers(void)
{
    int count;
    char buf[300];
    FILE *fp;

    head.plus.box.W = w;
    head.plus.box.E = e;
    head.plus.box.S = s;
    head.plus.box.N = n;

    fprintf(stderr, "\n");
    for (count = 0; count < num_open_layers; count++) {
#ifdef DEBUG
	fprintf(stderr, "%d open_layers %s\n", count, layers[count].name);
#endif
	if (layers[count].type != DXF_ASCII)
	    continue;

	/* Current opened layers opened using G_fopen_append to find lines
	 * and append info.
	 * However, G_fopen_modify should be used to update header infomation
	 * which is located starting part of a file.
	 */
	sprintf(buf, "%s/%s_%s", GRASS_VECT_DIRECTORY, base_name,
		layers[count].name);

	fclose(layers[count].Map->dig_fp.file);
	layers[count].Map->dig_fp.file =
	    G_fopen_modify(buf, GRASS_VECT_COOR_ELEMENT);

	if (layers[count].Map->dig_fp.file == NULL) {
	    fprintf(stderr, "error: unable to open dig file\n");
	    exit(-1);
	}

	fprintf(stderr, "%s_%s:\n", base_name, layers[count].name);
	Vect_copy_head_data(&head, layers[count].Map);
	Vect_build(layers[count].Map, stderr);
	Vect_close(layers[count].Map);
	fprintf(stderr, "\n");
    }
    for (count = 0; count < num_closed_layers; count++) {
	if (closed_layers[count].type != DXF_ASCII)
	    continue;
	if (closed_layers[count].status < 0)
	    continue;

#ifdef DEBUG
	fprintf(stderr, "open_layers %s\n", closed_layers[count].name);
#endif
	sprintf(buf, "%s/%s_%s", GRASS_VECT_DIRECTORY, base_name,
		closed_layers[count].name);

	/* temporarily reopen file */
	fp = G_fopen_modify(buf, GRASS_VECT_COOR_ELEMENT);
	closed_layers[count].Map->dig_fp.file = fp;


	if (fp == NULL) {
	    fprintf(stderr, "error: unable to open dig file %s\n", buf);
	    exit(-1);
	}

	fprintf(stderr, "%s_%s:\n", base_name, closed_layers[count].name);
	Vect_copy_head_data(&head, closed_layers[count].Map);
	Vect_build(closed_layers[count].Map, stderr);
	Vect_close(closed_layers[count].Map);
	fprintf(stderr, "\n");
    }

    return 0;
}
