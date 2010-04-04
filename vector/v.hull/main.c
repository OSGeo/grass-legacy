
/***************************************************************
 *
 * MODULE:       v.hull (based s.hull)
 * 
 * AUTHOR(S):    Andrea Aime <aaime@libero.it>
 *               Updated by Markus Neteler to 5.7
 *               Updated by Benjamin Ducke to support 3D hull creation
 *               
 * PURPOSE:      Creates the convex hull surrounding a vector points
 *               
 * COPYRIGHT:    (C) 2001, 2010 by the GRASS Development Team
 *
 *               This program is free software under the GNU General
 *               Public License (>=v2).  Read the file COPYING that
 *               comes with GRASS for details.
 *
 **************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

#include "chull.h"

struct Point
{
    double x;
    double y;
    double z;
};

int rightTurn(struct Point *P, int i, int j, int k)
{
    double a, b, c, d;

    a = P[i].x - P[j].x;
    b = P[i].y - P[j].y;
    c = P[k].x - P[j].x;
    d = P[k].y - P[j].y;
    return a * d - b * c < 0;
}

int cmpPoints(const void *v1, const void *v2)
{
    struct Point *p1, *p2;

    p1 = (struct Point *)v1;
    p2 = (struct Point *)v2;
    if (p1->x > p2->x)
	return 1;
    else if (p1->x < p2->x)
	return -1;
    else
	return 0;
}

int convexHull(struct Point *P, const int numPoints, int **hull)
{
    int pointIdx, upPoints, loPoints;
    int *upHull, *loHull;

    /* sort points in ascending x order */
    qsort(P, numPoints, sizeof(struct Point), cmpPoints);

    *hull = (int *)G_malloc(numPoints * 2 * sizeof(int));

    /* compute upper hull */
    upHull = *hull;
    upHull[0] = 0;
    upHull[1] = 1;
    upPoints = 1;
    for (pointIdx = 2; pointIdx < numPoints; pointIdx++) {
	upPoints++;
	upHull[upPoints] = pointIdx;
	while (upPoints > 1 &&
	       !rightTurn(P, upHull[upPoints], upHull[upPoints - 1],
			  upHull[upPoints - 2])
	    ) {
	    upHull[upPoints - 1] = upHull[upPoints];
	    upPoints--;
	}
    }

    G_debug(3, "upPoints: %d", upPoints);
    for(pointIdx = 0; pointIdx <= upPoints; pointIdx++)
	G_debug(5, " %d", upHull[pointIdx]);
    
    /* compute lower hull, overwrite last point of upper hull */
    loHull = &(upHull[upPoints]);
    loHull[0] = numPoints - 1;
    loHull[1] = numPoints - 2;
    loPoints = 1;
    for (pointIdx = numPoints - 3; pointIdx >= 0; pointIdx--) {
	loPoints++;
	loHull[loPoints] = pointIdx;
	while (loPoints > 1 &&
	       !rightTurn(P, loHull[loPoints], loHull[loPoints - 1],
			  loHull[loPoints - 2])
	    ) {
	    loHull[loPoints - 1] = loHull[loPoints];
	    loPoints--;
	}
    }

    G_debug(3, "numPoints:%d loPoints:%d upPoints:%d",
	    numPoints, loPoints, upPoints);

    for (pointIdx = 0; pointIdx <= loPoints; pointIdx++) 
	G_debug(5, " %d", loHull[pointIdx]);
    
    /* reclaim uneeded memory */
    *hull = (int *)G_realloc(*hull, (loPoints + upPoints) * sizeof(int));
    return loPoints + upPoints;
}

void convexHull3d(struct Point *P, const int numPoints, struct Map_info *Map)
{
    int error;
    int i;
    double *px;
    double *py;
    double *pz;

    px = G_malloc(sizeof(double) * numPoints);
    py = G_malloc(sizeof(double) * numPoints);
    pz = G_malloc(sizeof(double) * numPoints);

    for (i = 0; i < numPoints; i++) {
	px[i] = (P)[i].x;
	py[i] = (P)[i].y;
	pz[i] = (P)[i].z;
    }

    /* make 3D hull */
    error = make3DHull(px, py, pz, numPoints, Map);
    if (error < 0) {
	G_fatal_error(_("Simple planar hulls not implemented yet"));
    }
    
    G_free(px);
    G_free(py);
    G_free(pz);

}

#define ALLOC_CHUNK 256
int loadSiteCoordinates(struct Map_info *Map, struct Point **points, int all,
			struct Cell_head *window)
{
    int i, pointIdx;
    struct line_pnts *sites;
    struct line_cats *cats;
    BOUND_BOX box;
    int cat, type;

    sites = Vect_new_line_struct();
    cats = Vect_new_cats_struct();

    *points = NULL;
    pointIdx = 0;
    
    /* copy window to box */
    Vect_region_box(window, &box);

    while ((type = Vect_read_next_line(Map, sites, cats)) > -1) {

	if (type != GV_POINT && !(type & GV_LINES))
	    continue;
	
	Vect_cat_get(cats, 1, &cat);
	
	for (i = 0; i < sites->n_points; i++) {
	    G_debug(4, "Point: %f|%f|%f|#%d", sites->x[i], sites->y[i],
		    sites->z[i], cat);
	    
	    if (!all && !Vect_point_in_box(sites->x[i], sites->y[i], sites->z[i], &box))
		continue;
	    
	    G_debug(4, "Point in the box");

	    if ((pointIdx % ALLOC_CHUNK) == 0)
		*points = (struct Point *) G_realloc(*points,
						     (pointIdx + ALLOC_CHUNK) * sizeof(struct Point));
	    
	    (*points)[pointIdx].x = sites->x[i];
	    (*points)[pointIdx].y = sites->y[i];
	    (*points)[pointIdx].z = sites->z[i];
	    pointIdx++;
	}
    }

    if (pointIdx > 0)
	*points =
	    (struct Point *)G_realloc(*points,
				      (pointIdx + 1) * sizeof(struct Point));
    return pointIdx;
}

/*
 * Outputs the points that comprises the convex hull as a single closed line
 * and the hull baricenter as the label points (as it is a linear combination
 * of points on the hull is guaranteed to be inside the hull, follow from the
 * definition of convex polygon)
 */
int outputHull(struct Map_info *Map, struct Point *P, int *hull,
	       int numPoints)
{
    struct line_pnts *Points;
    struct line_cats *Cats;
    double *tmpx, *tmpy;
    int i, pointIdx;
    double xc, yc;

    tmpx = (double *)G_malloc((numPoints + 1) * sizeof(double));
    tmpy = (double *)G_malloc((numPoints + 1) * sizeof(double));

    xc = yc = 0;
    for (i = 0; i < numPoints; i++) {
	pointIdx = hull[i];
	tmpx[i] = P[pointIdx].x;
	tmpy[i] = P[pointIdx].y;
	/* average coordinates calculation... may introduce a little
	   numerical error but guaratees that no overflow will occurr */
	xc = xc + tmpx[i] / numPoints;
	yc = yc + tmpy[i] / numPoints;
    }
    tmpx[numPoints] = P[hull[0]].x;
    tmpy[numPoints] = P[hull[0]].y;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    Vect_copy_xyz_to_pnts(Points, tmpx, tmpy, 0, numPoints + 1);
    G_free(tmpx);
    G_free(tmpy);

    /* write out convex hull */
    Vect_write_line(Map, GV_BOUNDARY, Points, Cats);

    /* find and add centroid */
    Vect_reset_line(Points);
    Vect_append_point(Points, xc, yc, 0.0);
    Vect_cat_set(Cats, 1, 1);
    Vect_write_line(Map, GV_CENTROID, Points, Cats);
    Vect_destroy_line_struct(Points);

    return 0;
}

int main(int argc, char **argv)
{
    struct GModule *module;
    struct Option *input, *output;
    struct Flag *all, *flat;
    struct Cell_head window;

    char *mapset;
    char *sitefile;

    struct Map_info Map;
    struct Point *points;	/* point loaded from site file */
    int *hull;			/* index of points located on the convex hull */
    int numSitePoints, numHullPoints;

    int MODE2D;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector, geometry");
    module->description =
	_("Produces a convex hull for a given vector map.");

    input = G_define_standard_option(G_OPT_V_INPUT);
    input->label = _("Name of input vector map");
    input->description = _("For vector lines reads their vertices");
    
    output = G_define_standard_option(G_OPT_V_OUTPUT);
    
    all = G_define_flag();
    all->key = 'a';
    all->description =
	_("Use all vector points (do not limit to current region)");

    flat = G_define_flag();
    flat->key = 'f';
    flat->description =
	_("Create a 'flat' 2D hull even if the input is 3D points");
    
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    sitefile = input->answer;

    mapset = G_find_vector2(sitefile, "");
    if (mapset == NULL)
	G_fatal_error(_("Vector map <%s> not found"), sitefile);

    Vect_check_input_output_name(input->answer, output->answer,
				 GV_FATAL_EXIT);

    /* open site file */
    if (Vect_open_old(&Map, sitefile, mapset) < 0)
	G_fatal_error(_("Unable to open vector map <%s>"),
		      G_fully_qualified_name(sitefile, mapset));
    
    /* load site coordinates */
    G_get_window(&window);
    numSitePoints = loadSiteCoordinates(&Map, &points, all->answer, &window);
    if (numSitePoints < 0)
	G_fatal_error(_("Error loading vector points from <%s>"),
		      G_fully_qualified_name(sitefile, mapset));

    if (numSitePoints < 3)
	G_fatal_error(_("Convex hull calculation requires at least three points. Exiting."));

    G_verbose_message(_("%d points read from vector map <%s>"),
		      numSitePoints, G_fully_qualified_name(sitefile, mapset));
    
    /* create a 2D or a 3D hull? */
    MODE2D = 1;
    if (Vect_is_3d(&Map)) {
	MODE2D = 0;
    }
    if (flat->answer) {
	MODE2D = 1;
    }

    /* create vector map */
    if (0 > Vect_open_new(&Map, output->answer, MODE2D ? WITHOUT_Z : WITH_Z)) {
	G_fatal_error(_("Unable to create vector map <%s>"), output->answer);
    }
    
    Vect_hist_command(&Map);

    if (MODE2D) {
	/* compute convex hull */
	numHullPoints = convexHull(points, numSitePoints, &hull);

	/* output vector map */
	outputHull(&Map, points, hull, numHullPoints);
    }
    else {
	/* this does everything for the 3D hull including vector map creation */
	convexHull3d(points, numSitePoints, &Map);
    }
    
    /* clean up and bye bye */
    Vect_build(&Map);
    Vect_close(&Map);

    exit(EXIT_SUCCESS);
}
