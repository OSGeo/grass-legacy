#include <string.h>
#include <stdlib.h>
#include "math.h"
#include "htmlmap.h"
#include "driverlib.h"

#define RAD_DEG 57.29578

static void delete_point (int *x, int *y, int count)
{
    int i;

    for (i = 0; i < count; i++) {
        x[i] = x[i+1];
        y[i] = y[i+1];
    }


}

static double find_azimuth (double x1, double y1, double x2, double y2)
{
    double xx, yy;

    xx = x1 - x2;
    yy = y1 - y2;

    if (x1 == x2) {
	return (y2 > y1) ? 90.0 : 270.0; 
    } else { 
	if (y2 < y1) {
	    if (x2 > x1) {
		return 360.0 + (RAD_DEG * atan(yy / xx));
	    } else {
		return 180.0 + (RAD_DEG * atan(yy / xx));
	    }
	} else {
	    if (x2 > x1) {
		return         (RAD_DEG * atan(yy / xx));
	    } else {
		return 180.0 + (RAD_DEG * atan(yy / xx));
	    }
	}
    }
}


int Polygon_abs (int *x, int *y, int n)
{
    struct MapPoly *new;
    int i;
 
    double min_azimuth = 1.0;
    double azimuth1, azimuth2, diff1, diff2;


    /* 
     * remove points that have adjacent duplicates, including first and last
     */

    i = 0;
    while (i < (n-1)) {
        if (x[i] == x[i+1] && y[i] == y[i+1]) {
            delete_point(&x[i+1], &y[i+1], n-i-1);
            --n;
        } else {
            ++i;
        }
    }
    /* check if last point is same as first point */
    if (x[0] == x[n-1] && y[0] == y[n-1]) {
        --n;
    }


    /* 
     * remove points in excess of 100 vertices
     */

    while (n > 100) {

        for (i = 0; i < (n-2); i++) {
            
            /* 
             * see if middle point can be removed, by checking if the
             * relative bearing to the middle is less than our current tolerance
             */

            azimuth1 = find_azimuth((double) x[i],    (double) y[i], 
				    (double) x[i+1],  (double) y[i+1]);
            azimuth2 = find_azimuth((double) x[i],    (double) y[i], 
				    (double) x[i+2],  (double) y[i+2]   );

            diff1 = fmod(fabs((azimuth2+360.0) - azimuth1), 360.0);
            diff2 = fmod(fabs((azimuth1+360.0) - azimuth2), 360.0);

            if (diff1 <= min_azimuth || diff2 <= min_azimuth) {

                delete_point(&x[i+1], &y[i+1], n-i-1);
                --n;
                ++i;
                /* either stop deleting points because we're less than 100,
                   or keep deleting points with the same difference as this 
                   one (which might make a smaller polygon yet).  
                if (n <= 100) {
                    break;
                }
                */
            }

        }        

        /* increase minimum azimuth difference for next round */
        min_azimuth += 1.0;
    }

    /*
     * copy remaining points into a new MapPoly
     */

    if (n >= 3) {

	new = (struct MapPoly *) G_malloc(sizeof(struct MapPoly));

	/* grab the last text string written as url */
	new->url = (char *) G_malloc(strlen(last_text)+1);
	strcpy(new->url, last_text);

	/* hook up new MapPoly into list */
	new->next_poly = NULL;
	*tail = new;
	tail  = &(new->next_poly);

	new->num_pts = n;
	new->x_pts = (int *) G_malloc(sizeof(int) * n);
	new->y_pts = (int *) G_malloc(sizeof(int) * n);

	for (i = 0; i < n; i++) {
	    new->x_pts[i] = x[i];
	    new->y_pts[i] = y[i];
	}

    }

    return 0;
}

