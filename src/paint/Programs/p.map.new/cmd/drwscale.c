#include "gis.h"
#include "misc.h"

extern double floor() ;

int 
drwscale (double e1, double n1, double e2, double n2)
{

/* set color to black, line width to 1 */



	G_plot_line(e1, n1, e2, n2);



	set_line_style_solid();



}
