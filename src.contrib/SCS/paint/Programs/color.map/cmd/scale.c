/****************************************************************
 *
 * scale (window, panel, margin, text)
 *
 *   reconfigures the window resolution, rows and cols
 *   based on the scale
 *
 *   window: window to be reconfigured.
 *           note: the window resolutions, rows, cols are ignored
 *
 *   panel:   number of pixels (max) in one panel including margins
 *   margin:  number of pixels in margins (outside map)
 *
 *   text:    scale text
 *
 * note: must call check_scale() first. errors here are fatal
 ***************************************************************/
#include "gis.h"

#define INCHES_TO_CM ((double)2.54)
#define STDSCALE ((double)7920.0)

double Phres(), Pvres();


double scaled (window, rows, cols)
    struct Cell_head *window;
{
    double ns, ew;
	double res, scale;

	res = Phres();
	if (res <= 0) return(0.0);

    ns = (window->ns_res * window->rows) / rows;
    ew = (window->ew_res * window->cols) / cols;

    if (ns > ew)
	ew = ns;
    else
	ns = ew;

	scale = (ns * 100.0) /(INCHES_TO_CM * res);
	if (scale < STDSCALE) {
		scale = STDSCALE;
		ns = STDSCALE * INCHES_TO_CM * res / 100.0;
		}
	else {
		fprintf(stderr,"\nWARNING: The scale of this map is 1:%.0lf\n",scale);
		sleep(5);
		}
	ew = ns;

    window->ns_res = ns;
    window->ew_res = ew;

    window->rows = (window->north - window->south) / ns;
    window->cols = (window->east  - window->west ) / ew;
	return(scale);
}
