#include "gis.h"
#include "dlg.h"
#include "graphics.h"
#include "parms.h"
#include "vector.h"
#include "misc.h"
#include "fullwindow.h"
#include "colormode.h"
#include "pattern.h"
#include "sgrid.h"

draw_cat(x, y,w,h, catnum, pcolr)
double x, y;
int w,h;
int catnum;
struct Colors *pcolr;
{
int legendx, legendy, legend_width, legend_height;
unsigned char black;
unsigned char white;
int		i, j, k;
int		c;

	black	= BLACK;
	white	= WHITE;

	G_plot_where_xy(x, y, &legendx, &legendy);

	set_color (black);
	draw_line (legendx,legendy,legendx+w, legendy);
	draw_line (w+legendx, legendy, w+legendx, h+legendy);
	draw_line (legendx,legendy,legendx, h+legendy);
	draw_line (legendx,h+legendy,w+legendx, h+legendy);

	set_diffusion_color(w);

	for (j=2; j<=h-2; j++)
	{
	    k = 0;
	    for (i=2; i<=w-2; i++) {
		c = get_catcolr(catnum, legendy+j, k);
		set_color(c);
		dot(legendx+i,legendy+j);
		k++;
	    }
	}

}

