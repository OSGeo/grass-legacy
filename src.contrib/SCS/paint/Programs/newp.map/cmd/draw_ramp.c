#include "gis.h"
#include "dlg.h"
#include "graphics.h"
#include "parms.h"
#include "misc.h"
#include "fullwindow.h"
#include "colormode.h"
#include "pattern.h"

draw_ramp(x, y,w,orient, pcolr, pcats, statf)
double x, y;
int w;
int orient;
struct Colors *pcolr;
struct Categories *pcats;
struct Cell_stats *statf;
{
int legendx, legendy, legend_width, legend_height;
unsigned char black;
unsigned char white;
int		i, j, k;
int		c;
int 	catnum;
long	count;
int     numofcats;
int 	ilegendy;
int 	ilegendx;

	black	= BLACK;
	white	= WHITE;
	numofcats = 0;

	G_plot_where_xy(x, y, &legendx, &legendy);

	ilegendy	= legendy;
	ilegendx	= legendx;

	set_diffusion_color(w);


	k = 0;

	if (!orient) {
	G_rewind_cell_stats(statf);
	while (G_next_cell_stat(&catnum, &count, statf) )
	{
	numofcats++;
	for (j=legendy+2; j<=legendy+4; j++)
		for (i=legendx+2; i<=legendx+w-2; i++) {
			c = get_catcolr(catnum, j, k);
			set_color(c);
			dot(i,j);
			k++;
			}

	legendy	= legendy+2;
	k = 0;
	}
	}
	else {
	G_rewind_cell_stats(statf);
	while (G_next_cell_stat(&catnum, &count, statf) )
	{
	numofcats++;
	    for (j=legendy+2; j<=legendy+w-2; j++)
		for (i=legendx+2; i<=legendx+4; i++) {
			c = get_catcolr(catnum, j, k);
			set_color(c);
			dot(i,j);
			k++;
			}

	legendx = legendx +2;
	k = 0;
	}
	}



	set_color (black);
if (!orient){
	draw_line (legendx,ilegendy,legendx+w, ilegendy);
	draw_line (w+legendx, ilegendy, w+legendx, 2*numofcats+ilegendy);
	draw_line (legendx,ilegendy,legendx, 2*numofcats+ilegendy);
	draw_line (legendx,2*numofcats+ilegendy+2,w+legendx, 2*numofcats+ilegendy+2);
	}
else {
	draw_line (ilegendx,ilegendy,2*numofcats+ilegendx+2, ilegendy);
	draw_line (ilegendx, ilegendy, ilegendx, w+ilegendy);
	draw_line (ilegendx,ilegendy+w,2*numofcats+ilegendx+2, ilegendy+w);
	draw_line (2*numofcats+ilegendx+2,ilegendy,2*numofcats+ilegendx+2, ilegendy+w);
	}




}
