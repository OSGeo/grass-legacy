/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */


#include "htmlmap.h"
#include "gis.h"
#include "driverlib.h"



/* point in polygon test by Randolph Franklin */
/* http://www.ecse.rpi.edu/Homepages/wrf/     */
/* adapted for integer coordinates            */

static int pnpoly (int npol, int *xp, int *yp, int x, int y)
{
  int i, j, c = 0;
  for (i = 0, j = npol-1; i < npol; j = i++) {
    if ((((yp[i] <= y) && (y < yp[j])) ||
         ((yp[j] <= y) && (y < yp[i]))) &&
        (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))
      c = !c;
  }
  return c;
}



int 
Graph_Close (void)
{
     struct MapPoly *poly, *test_poly;

     int i;
     int inside;

    /* 
     * exmaine the list of polygons, if a polygon wholly exists inside of
     * another polygon, then remove it.
     *
     */

    for (poly = head; poly != NULL; poly = poly->next_poly) {

        for (test_poly = head; test_poly != NULL; 
					test_poly = test_poly->next_poly) {
            if (poly == test_poly) {
                continue;	/* don't check ourselves */
            }
             
            inside = 1;
            for (i = 0; i < poly->num_pts && inside; i++) {
                inside = pnpoly(test_poly->num_pts, 
                                  test_poly->x_pts, test_poly->y_pts, 
                                  poly->x_pts[i],   poly->y_pts[i]);
            }
            if (inside) {
                poly->num_pts = 0;	/* mark polygon as having no points */
                break;
            }
        }

    }


    /*
     * write any beginning prologue appropriate for the map type
     */
    
    switch (html_type) {

      case APACHE:
        fprintf(output,"#base _base_\n#default _default_\n");
        break;

      case RAW:
        break;

      case CLIENT:
        fprintf(output,"<MAP NAME=\"map\">\n");
        break;
    }

    /*
     * write the polygons in a specific format
     */

    for (poly = head; poly != NULL; poly = poly->next_poly) {
        if (poly->num_pts >= 3) {

            switch (html_type) {

              case APACHE:
                fprintf(output,"poly %s", poly->url);
                for (i = 0; i < poly->num_pts; i++) {
                    fprintf(output," %d,%d", poly->x_pts[i], poly->y_pts[i]);
                }
                fprintf(output,"\n");
                break;

              case RAW:
                fprintf(output,"%s", poly->url);
                for (i = 0; i < poly->num_pts; i++) {
                    fprintf(output," %d %d", poly->x_pts[i], poly->y_pts[i]);
                }
                fprintf(output,"\n");
                break;

              case CLIENT:
                fprintf(output,"<AREA SHAPE=\"POLY\"\n HREF=\"%s\"\n COORDS=\"", 
			poly->url);
                for (i = 0; i < poly->num_pts; i++) {
                    if (i > 0) fprintf(output,", ");
                    if (i % 8 == 0) fprintf(output,"\n  ");
                    fprintf(output,"%d,%d", poly->x_pts[i], poly->y_pts[i]);
                }
                fprintf(output,"\">\n");
                break;

            }

        } 

    }

    /* final stuff, if needed */

    switch (html_type) {

      case APACHE:
        break;

      case RAW:
        break;

      case CLIENT:
	fprintf(output,"</MAP>\n");
        break;

    }

    /*
     * close file 
     */

    fclose(output);

	return 0;
}
