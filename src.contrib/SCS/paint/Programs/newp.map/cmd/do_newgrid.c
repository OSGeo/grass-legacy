
#include "gis.h"
#include "graphics.h"
#include "text.h"
#include "misc.h"
#include "parms.h"
#include "fullwindow.h"
#include "grid.h"

extern double floor() ;

do_newgrid ()
{
    double g;
    double east, west, incr;
    int i;




    if (parms.grid <= 0)
        return;

/* set color to black, line width to 1 */

    set_color (parms.grid_color);
    set_width (1);




/* new additions */
	set_line_style_solid();
	if (grid.hwidth)
	{
	set_color (grid.hcolor);
	set_width (grid.width + 2*grid.hwidth);
	drawgrid();
		}


 	set_width (grid.width);
	if (grid.linestyle != NULL)
		set_line_style(grid.linestyle, grid.colors);
	else
		set_color(grid.colors[0]);


	drawgrid();


	set_line_style_solid();



}
