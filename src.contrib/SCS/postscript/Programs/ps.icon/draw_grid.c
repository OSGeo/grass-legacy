/* Function draw_grid
**
** This function displays the grid.
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_icon.h"

draw_grid(t, b, l, r)
int t, b, l, r;
{
    char buf[4];
    int i, x, y, tsize, h, w, delta;

    /* clear graphics screen */
    R_open_driver();
    R_erase();
    R_standard_color(D_translate_color("black"));
    R_box_abs(l, t, r, b);

    /* green grid */
    R_standard_color(D_translate_color("green"));

    /* set text size */
    tsize = (int)(0.03 * ((double)b - (double)t));
    R_text_size(tsize, tsize);

    /* center grid on screen */
    icon.cx = (r + l) / 2;
    icon.cy = (t + b) / 2;

    /* compute grid line spacing */
    if (icon.cx > icon.cy) delta = (int)(0.08 * ((double)b - (double)t));
    else                   delta = (int)(0.08 * ((double)r - (double)l));

    /* compute grid limits */
    icon.x_min = icon.cx - 5 * delta; 
    icon.x_max = icon.cx + 5 * delta; 
    icon.y_min = icon.cy - 5 * delta;
    icon.y_max = icon.cy + 5 * delta;

    /* draw grid */
    for (i = 0; i <= 10; i++)
    {
	/* line label */
  	sprintf(buf, "%d", abs(i - 5));

	/* horizontal lines */
	y = icon.cy + (i - 5) * delta;
	R_move_abs(icon.x_min - (int)(1.5 * (double)tsize), 
		    y + (int)(0.5 * (double)tsize));
	R_text(buf);
	R_move_abs(icon.x_min, y);
	R_cont_abs(icon.x_max, y);
	if (i == 5)
	{
	    R_move_abs(icon.x_min, y - 1);
	    R_cont_abs(icon.x_max, y - 1);
	    R_move_abs(icon.x_min, y + 1);
	    R_cont_abs(icon.x_max, y + 1);
	}
	R_move_abs(icon.x_max + (int)(0.5 * (double)tsize), 
		    y + (int)(0.5 * (double)tsize));
	R_text(buf);

	/* vertical lines */
	x = icon.cx + (i - 5) * delta; 
	R_move_abs(x - (int)(0.5 * (double)tsize),
		   icon.y_min - (int)(0.5 * (double)tsize));
	R_text(buf);
	R_move_abs(x, icon.y_min);
	R_cont_abs(x, icon.y_max);
	if (i == 5)
	{
	    R_move_abs(x - 1, icon.y_min);
	    R_cont_abs(x - 1, icon.y_max);
	    R_move_abs(x + 1, icon.y_min);
	    R_cont_abs(x + 1, icon.y_max);
	}
	R_move_abs(x - (int)(0.5 * (double)tsize),
		   icon.y_max + (int)(1.5 * (double)tsize));
	R_text(buf);
    }
    R_close_driver();
}
