#include "gis.h"
#include "fullwindow.h"
#include "regionline.h"
#include "misc.h"

extern double floor() ;

drwline (e1, n1, e2, n2)
double e1, n1, e2, n2;
{

/* set color to black, line width to 1 */


    set_color (BLACK);
    set_width (1);


/* new additions */
	set_line_style_solid();
	if (dline.hwidth)
	{
	set_color (dline.hcolor);
	set_width (dline.width + 2*dline.hwidth);
	G_plot_line(e1, n1, e2, n2);
		}


 	set_width (dline.width);
	if (dline.linestyle != NULL)
		set_line_style(dline.linestyle, dline.colors);
	else
		set_color(dline.colors[0]);

	G_plot_line(e1, n1, e2, n2);



	set_line_style_solid();



}
