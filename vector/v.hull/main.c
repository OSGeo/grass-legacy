/******************************************************************************
* hull.c <s.hull>
* Creates the convex hull surrounding a sites list

* @Copyright Andrea Aime <aaime@libero.it>
* 23 Sept. 2001
* Last updated 19 Dec 2003, Markus Neteler to 5.7
*

* This file is part of GRASS GIS. It is free software. You can
* redistribute it and/or modify it under the terms of
* the GNU General Public License as published by the Free Software
* Foundation; either version 2 of the License, or (at your option)
* any later version.

* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "gis.h"
#include "Vect.h"
#include "site.h"


struct Point {
   double x;
   double y;
};


int rightTurn(struct Point *P, int i, int j, int k) {
    double a, b, c, d;
    a = P[i].x - P[j].x;
    b = P[i].y - P[j].y;
    c = P[k].x - P[j].x;
    d = P[k].y - P[j].y;
    return a*d - b*c < 0;	
}


int cmpPoints(const void* v1, const void* v2) {
    struct Point *p1, *p2;
    p1 = (struct Point*) v1;
    p2 = (struct Point*) v2;
    if( p1->x > p2->x )
        return 1;
    else if( p1->x < p2->x )
        return -1;
    else
        return 0;
}


int convexHull(struct Point* P, int numPoints, int **hull) {
    int pointIdx, upPoints, loPoints;
    int *upHull, *loHull;

    /* sort points in ascending x order*/
    qsort(P, numPoints, sizeof(struct Point), cmpPoints);

    *hull = (int*) G_malloc(numPoints * sizeof(int));

    /* compute upper hull */
    upHull = *hull;
    upHull[0] = 0;
    upHull[1] = 1;
    upPoints = 1;
    for(pointIdx = 2; pointIdx < numPoints; pointIdx++) {
        upPoints++;
        upHull[upPoints] = pointIdx;
        while( upPoints > 1 &&
               !rightTurn(P, upHull[upPoints], upHull[upPoints-1],
                             upHull[upPoints-2])
             ) {
            upHull[upPoints-1] = upHull[upPoints];
            upPoints--;
        }
    }

    /*
    printf("upPoints: %d\n", upPoints);
    for(pointIdx = 0; pointIdx <= upPoints; pointIdx ++)
        printf("%d ", upHull[pointIdx]);
    printf("\n");
    */

    /* compute lower hull, overwrite last point of upper hull */
    loHull = &(upHull[upPoints]);
    loHull[0] = numPoints - 1;
    loHull[1] = numPoints - 2;
    loPoints = 1;
    for(pointIdx = numPoints - 3; pointIdx >= 0; pointIdx--) {
        loPoints++;
        loHull[loPoints] = pointIdx;
        while( loPoints > 1 &&
               !rightTurn(P, loHull[loPoints], loHull[loPoints-1],
                             loHull[loPoints-2])
             ) {
             loHull[loPoints-1] = loHull[loPoints];
             loPoints--;
        }
    }

    /*
    printf("loPoints: %d\n", loPoints);
    for(pointIdx = 0; pointIdx <= loPoints; pointIdx ++)
        printf("%d ", loHull[pointIdx]);
    printf("\n");
    */

    /* reclaim uneeded memory */
    *hull = (int*) G_realloc((char*)(*hull), (loPoints + upPoints) * sizeof(int));
    return loPoints + upPoints;
}



#define ALLOC_CHUNK 256
int loadSiteCoordinates(FILE* fdsite, struct Point **points , int all, struct Cell_head *window) {
    int pointIdx;
    Site *site;
    int n, s, d;
    RASTER_MAP_TYPE c;

    if(G_site_describe(fdsite, &n, &c, &s, &d) != 0) return -1;
    site = G_site_new_struct (c, n, s, d);

    pointIdx = 0;
    *points = NULL;
    while( G_site_get(fdsite, site) == 0 )
    {
        if(all || G_site_in_region(site, window) )
        {
            if(pointIdx % ALLOC_CHUNK == 0);
               *points = (struct Point*) G_realloc((char*)(*points), (pointIdx + ALLOC_CHUNK) * sizeof(struct Point));

            (*points)[pointIdx].x = site->east;
            (*points)[pointIdx].y = site->north;
            pointIdx++;
        }
    }

    if(pointIdx > 0)
        *points = (struct Point*) G_realloc((char*)(*points), pointIdx * sizeof(struct Point));
    return pointIdx;
}

/*
 * Outputs the points that comprises the convex hull as a single closed line
 * and the hull baricenter as the label points (as it is a linear combination
 * of points on the hull is guaranteed to be inside the hull, follow from the
 * definition of convex polygon)
 */
int outputHull(struct Map_info *Map, struct Point* P, int *hull,
               int numPoints) {
    struct line_pnts *Points;
    struct line_cats *Cats;
    double *tmpx, *tmpy;
    int i, pointIdx;
    double xc, yc;

    tmpx = (double *) G_malloc((numPoints + 1) * sizeof(double));
    tmpy = (double *) G_malloc((numPoints + 1) * sizeof(double));

    xc = yc = 0;
    for(i = 0; i < numPoints; i++) {
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

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    Vect_copy_xyz_to_pnts(Points, tmpx, tmpy, 0, numPoints+1);
    G_free(tmpx);
    G_free(tmpy);

    /* write out convex hull */
    Vect_write_line (Map, GV_BOUNDARY, Points, Cats);
    
    /* find and add centroid */
    Vect_reset_line (Points);
    Vect_append_point (Points,xc, yc, 0.0);
    Vect_cat_set ( Cats, 1, 1 );
    Vect_write_line (Map, GV_CENTROID, Points, Cats);
    Vect_destroy_line_struct (Points);


    return 0;
}



int main(int argc, char **argv) {
    struct GModule *module;
    struct Option *input, *output;
    struct Flag *all;
    struct Cell_head window;

    char *mapset;
    char *sitefile;

    FILE* fdsite;
    struct Map_info Map;
    struct Point *points;  /* point loaded from site file */
    int *hull;   /* index of points located on the convex hull */
    int numSitePoints, numHullPoints;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description = "Uses a GRASS vector points map to produce a convex hull vector map";
    input = G_define_option ();
    input->key = "input";
    input->type = TYPE_STRING;
    input->required = YES;
    input->description = "name of a vector points map to be input";
    input->gisprompt = "old,vector,vector,input";

    output = G_define_option ();
    output->key = "output";
    output->type = TYPE_STRING;
    output->required = YES;
    output->description = "name of a vector area map to be output";
    output->gisprompt = "new,dig,binary file,output";

    all = G_define_flag ();
    all->key = 'a';
    all->description = "Use all vector points (do not limit to current region)";

    if (G_parser (argc, argv)) exit (1);

    Vect_check_input_output_name ( input->answer, output->answer, GV_FATAL_EXIT );

    /* look for mapset containing site file */
    sitefile = input->answer;
    mapset = G_find_sites (sitefile, "");
    if (mapset == NULL)
        G_fatal_error ("Vector points map [%s] not found", sitefile);

    /* open site file */
    fdsite = G_sites_open_old (sitefile, mapset);
    if (fdsite == NULL)
        G_fatal_error ("Cannot open vector points map [%s]", sitefile);

    /* load site coordinates */
    G_get_window (&window);
    numSitePoints = loadSiteCoordinates(fdsite, &points, all->answer, &window);
    if(numSitePoints < 0 )
        G_fatal_error ("Error loading vector points map [%s]", sitefile);

    if(numSitePoints < 3 )
        G_fatal_error ("Convex hull calculation requires at least three points");

    /* create vector file */
    if (0 > Vect_open_new (&Map, output->answer, 0) )
        G_fatal_error ("Unable to open vector file <%s>\n", output->answer);

    Vect_hist_command ( &Map );

    /* compute convex hull */
    numHullPoints = convexHull(points, numSitePoints, &hull);

    /* output vector file */
    outputHull(&Map, points, hull, numHullPoints);

    /* clean up and bye bye */
    Vect_build (&Map, stdout);
    Vect_close (&Map);

    return 0;
}
