#include "gis.h"
#include "gtoa.h"
#include "Vect.h"
#include <stdio.h>

/*
 * modified for v.out.shape
 *   - Markus Neteler 6/2000
 */

/****************************************************************************
*  	    	   NECTEC -- High Performance Computing Center --
*      	    	    	    	Bangkok, Thailand
*
* PROJECT: 	Grass tool for v.out.poly
* FILENAME:	main.c
* AUTHOR(S):	Justin Hickey - jhickey@hpcc.nectec.or.th
* PURPOSE: 	To output the polygons of a vector file to a text file. The
*   	    	format of the output file is as follows:
*   	    	    A	numPoints   catNumber	catLabel
*   	    	      x y
*   	    	      x y
*   	    	    END
*   	    	The "A" character indicates the following data is an area just
*   	    	like the v.out.ascii output. It is redundant since only areas
*   	    	are output but it provides a marker for scripts to indicate the
*   	    	start of the polygon. The "numPoints" value is the number of
*   	    	points in the polygon. The "catNumber" value is the category
*   	    	number assigned to the polygon and the "catLabel" is the
*   	    	category label assigned to the polygon. These fields are
*   	    	delimited by tabs. The "x" and "y" values are the coordinates
*   	    	of each point in the polygon, which are reversed from the
*   	    	output of the v.out.ascii command. Note that "x" corresponds to
*   	    	the easting, while "y" corresponds to the northing. The "END"
*   	    	value signifies the end of the polygon. This program does not
*   	    	have an interactive mode and takes both an input file and an
*   	    	output file for arguments.
* CMD LINE ARG: Input vector file and output text file
* OUTPUT:    	Text file containing information about polygons
* DATE CREATED: Mar 05 1999
* HISTORY:
*
*   Mar 05 1999 Finished the program
*
*   Mar 08 1999 Refined the output format of the x and y points. Modified by
*   	    	Justin Hickey
*
*   Mar 12 1999 Added the code to output the END marker and switched the order
*   	    	of the x and y output. Modified by Justin Hickey
*
****************************************************************************/

int 
write_areas (char *name, char *mapset, struct Map_info map, FILE *lines_file, FILE *points_file, FILE *text_file)
{
	int count;
	struct line_pnts points;
	struct Categories cats;
	int NumAreas;
    int     	    	catNum;     	/* category number */
    int     	    	haveCat;    	/* flag to indicate category file */
    int     	    	i, j;  	    	/* counters */
    char    	    	*catLabel;  	/* category label */


#ifdef DEBUG
	fprintf (stdout,"write_areas %s %s\n",name,mapset);
#endif

    if (G_read_vector_cats(name, mapset, &cats) < 0)
    {
    	haveCat = 0;
#ifdef DEBUG
	fprintf (stdout,"   error %d reading vector cats\n",catflag);
#  endif

    }
    else
    {
    	haveCat = 1;
fprintf(stderr, "Reading cats...\n");
    }

    /* LOOP through all areas in the dig file */

    NumAreas = V2_num_areas(&map);

    if (NumAreas > 0)
    {
    	/* Go through the areas */
    	for (i = 1; i <=NumAreas; i++)
    	{
fprintf(stderr,"%d\n", i);
     	    /* Get the points of the polygon */
    	    Vect_get_area_points(&map, i, &points);
    	    
   	    /* Print out the number of points */
   	    fprintf(lines_file, "A\t%d\t", points.n_points);
   	    
   	    /* Get and print the category information */
    	    if(haveCat)
    	    {
    	    	catNum = map.Att[map.Area[i].att].cat;
    	    	catLabel = G_get_cat(catNum, &cats);
    	    	
    	    	fprintf(lines_file, "%d\t%s\n", catNum, catLabel);
    	    }
    	    
    	    /* Print the actual points */
    	    for (j = 0; j < points.n_points; j++)
    	    {
    	    	fprintf(lines_file, " %10.6f %10.6f\n", points.x[j], points.y[j]);
    	    }
    	    
    	    /* Print the end marker */
    	    fprintf(lines_file, "END\n");
    	}
    }
    
    /* Print end message */
    fprintf(stderr, "\nCOMPLETE\n");

    return 0;
}
