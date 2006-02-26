#include <grass/Vect.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "local_proto.h"

/*Prototype */
void writePoints(struct line_pnts *Points, int dp, FILE * ascii);


/* ************************************************************************* */
/* This function writes the point coordinates and the geometric feature **** */
/* ************************************************************************* */
int writeVTK(FILE * ascii, struct Map_info *Map, int layer, int feature, int dp)
{
    int cat, type, i, j, centroid, generatedata = 1;
    int numpoints = 0, numlines = 0, numareas = 0, cur;
    static struct line_pnts *Points;
    struct line_cats *Cats;

    Points = Vect_new_line_struct();	/* init line_pnts struct */
    Cats = Vect_new_cats_struct();

    if (feature == GV_POINT || feature == GV_KERNEL || feature == GV_CENTROID) {

	/*Get the number of the points to generate */
	numpoints = Vect_get_num_primitives(Map, feature);

	/*break if nothing to generate */
	if (numpoints == 0)
	    G_fatal_error(_
			  ("No coordinates to generate the output! Maybe you choosed the wrong feature?"));

	fprintf(ascii, "POINTS %i float\n", numpoints);

	Vect_rewind(Map);

	/*Write the coordinates */
	cur=0;
	while (1) {
	    if (cur <=numpoints) G_percent(cur,numpoints,2);
	    if (-1 == (type = Vect_read_next_line(Map, Points, Cats)))
		break;
	    if (type == -2)	/* EOF */
		break;
	    if (type == feature) {
		writePoints(Points, dp, ascii);

		if (Cats->n_cats == 0)
		    generatedata = 0;	/*No data generation */
	    }
	    cur++;
	}

	/*Write the vertices */
	fprintf(ascii, "VERTICES %i %i\n%i", numpoints, numpoints + 1,
		numpoints);
	for (i = 0; i < numpoints; i++) {
	    fprintf(ascii, " %i", i);
	}
	fprintf(ascii, "\n");

	if (!generatedata) {
	    G_warning(_
		      ("Category data is not complete, will set the missing values to -1!"));
	}

	/*Write the pointdata */
	fprintf(ascii, "POINT_DATA %i\n", numpoints);
	fprintf(ascii, "SCALARS cat int 1\n");
	fprintf(ascii, "LOOKUP_TABLE default\n");

	Vect_rewind(Map);
	while (1) {
	    if (-1 == (type = Vect_read_next_line(Map, Points, Cats)))
		break;
	    if (type == -2)	/* EOF */
		break;
	    if (type == feature) {
		Vect_cat_get(Cats, layer, &cat);
		fprintf(ascii, " %d", cat);
	    }
	}

    }				/*LINE AND BOUNDARY feature with celldata */
    else if (feature == GV_LINE || feature == GV_BOUNDARY || feature == GV_FACE ) {

	/*count the number of line_nodes and lines */
	Vect_rewind(Map);
	numpoints = 0;
	while (1) {
	    if (-1 == (type = Vect_read_next_line(Map, Points, Cats)))
		break;
	    if (type == -2)	/* EOF */
		break;
	    if (type == feature) {
		numpoints += Points->n_points;
		numlines++;
	    }
	}

	/*break if nothing to generate */
	if (numpoints == 0)
	    G_fatal_error(_
			  ("No coordinates to generate the output! Maybe you choosed the wrong feature?"));

	fprintf(ascii, "POINTS %i float\n", numpoints);

	/*Write the coordinates */
	Vect_rewind(Map);
	cur=0;
	while (1) {
	    if (cur <= numlines) G_percent(cur,numlines,2);
	    if (-1 == (type = Vect_read_next_line(Map, Points, Cats)))
		break;
	    if (type == -2)	/* EOF */
		break;
	    if (type == feature) {
		writePoints(Points, dp, ascii);
	    }
	    cur++;
	}

	/*Write the lines */
	if (feature == GV_FACE)
		fprintf(ascii, "POLYGONS %i %i\n", numlines, numpoints + numlines);
	else
		fprintf(ascii, "LINES %i %i\n", numlines, numpoints + numlines);

	Vect_rewind(Map);
	i = 0;
	while (1) {

	    if (-1 == (type = Vect_read_next_line(Map, Points, Cats)))
		break;
	    if (type == -2)	/* EOF */
		break;
	    if (type == feature) {

		/*Check for data generation */
		if (Cats->n_cats == 0)
		    generatedata = 0;	/*No data generation */

		fprintf(ascii, "%i", Points->n_points);
		while (Points->n_points--) {
		    fprintf(ascii, " %i", i);
		    i++;
		}
		fprintf(ascii, "\n");
	    }
	}

	if (!generatedata) {
	    G_warning(_
		      ("Category data is not complete, will set the missing values to -1!"));
	}

	/*Write the celldata */
	fprintf(ascii, "CELL_DATA %i\n", numlines);
	fprintf(ascii, "SCALARS cat int 1\n");
	fprintf(ascii, "LOOKUP_TABLE default\n");
	Vect_rewind(Map);
	while (1) {
	    if (-1 == (type = Vect_read_next_line(Map, Points, Cats)))
		break;
	    if (type == -2)	/* EOF */
		break;
	    if (type == feature) {
		Vect_cat_get(Cats, layer, &cat);
		fprintf(ascii, " %d", cat);
	    }
	}


    }				/*AREA FEATURE with celldata */
    else if (feature == GV_AREA) {

	numareas = Vect_get_num_areas(Map);
	numpoints = 0;

	/*Count the coordinate points */
	Vect_rewind(Map);
	for (i = 1; i <= numareas; i++) {
	    centroid = Vect_get_area_centroid(Map, i);
	    if (centroid > 0) {
		Vect_read_line(Map, NULL, Cats, centroid);
	    }
	    Vect_get_area_points(Map, i, Points);
	    numpoints += Points->n_points;
	}

	/*break if nothing to generate */
	if (numpoints == 0)
	    G_fatal_error(_
			  ("No coordinates to generate the output! Maybe you choosed the wrong feature?"));

	fprintf(ascii, "POINTS %i float\n", numpoints);

	/*Write the coordinates */
	Vect_rewind(Map);
	for (i = 1; i <= numareas; i++) {
	    centroid = Vect_get_area_centroid(Map, i);
	    if (centroid > 0) {
		Vect_read_line(Map, NULL, Cats, centroid);
	    }
	    Vect_get_area_points(Map, i, Points);
	    writePoints(Points, dp, ascii);
	}

	/*Write the polygons */
	fprintf(ascii, "POLYGONS %i %i\n", numareas, numpoints + numareas);
	Vect_rewind(Map);
	j = 0;
	for (i = 1; i <= numareas; i++) {
	    centroid = Vect_get_area_centroid(Map, i);
	    if (centroid > 0) {
		Vect_read_line(Map, NULL, Cats, centroid);
	    }
	    Vect_get_area_points(Map, i, Points);

	    /*Check for data generation */
	    if (Cats->n_cats == 0)
		generatedata = 0;	/*No data generation */

	    fprintf(ascii, "%i", Points->n_points);
	    while (Points->n_points--) {
		fprintf(ascii, " %i", j);
		j++;
	    }
	    fprintf(ascii, "\n");
	}

	if (!generatedata) {
	    G_warning(_
		      ("Category data is not complete, will set the missing values to -1!"));
	}
	/*Write the celldata */
	fprintf(ascii, "CELL_DATA %i\n", numareas);
	fprintf(ascii, "SCALARS cat int 1\n");
	fprintf(ascii, "LOOKUP_TABLE default\n");
	Vect_rewind(Map);
	j = 0;
	for (i = 1; i <= numareas; i++) {
	    centroid = Vect_get_area_centroid(Map, i);
	    if (centroid > 0) {
		Vect_read_line(Map, NULL, Cats, centroid);
	    }
	    Vect_cat_get(Cats, layer, &cat);
	    fprintf(ascii, " %d", cat);
	}

    }

    return 1;
}

/* ************************************************************************* */
/* This function writes the point coordinates ****************************** */
/* ************************************************************************* */
void writePoints(struct line_pnts *Points, int dp, FILE * ascii)
{
    char *xstring = NULL, *ystring = NULL, *zstring = NULL;
    double *xptr, *yptr, *zptr;

    xptr = Points->x;
    yptr = Points->y;
    zptr = Points->z;

    while (Points->n_points--) {
	G_asprintf(&xstring, "%.*f", dp, *xptr++);
	G_trim_decimal(xstring);
	G_asprintf(&ystring, "%.*f", dp, *yptr++);
	G_trim_decimal(ystring);
	G_asprintf(&zstring, "%.*f", dp, *zptr++);
	G_trim_decimal(zstring);
	fprintf(ascii, "%s %s %s \n", xstring, ystring, zstring);
    }

    return;
}
