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
#include "local_proto.h"

int draw_cat(double x,double y,int w,int h,
	DCELL dmin,DCELL dmax,struct Colors *pcolr)
{
DCELL val;
int legendx, legendy;
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

        val = dmin; 
        if(G_is_d_null_value(&val))
	{
	    for (j=2; j<=h-2; j++)
	    {
	        k = 0;
	        for (i=2; i<=w-2; i++) {
		    c = get_catcolr(val, legendy+j, k);
		    set_color(c);
		    dot(legendx+i,legendy+j);
	    	    k++;
	        }
	    }
	    return 0;
	}

	for (j=2; j<=h-2; j++)
	{
	    k = 0;
	    if(parms.map_type != CELL_TYPE)
	        val = dmin + (double) (j-2) * (dmax-dmin) /(double) (h-3);
	    for (i=2; i<=w-2; i++) {
		c = get_catcolr(val, legendy+j, k);
		set_color(c);
		dot(legendx+i,legendy+j);
		k++;
	    }
	}

	return 0;
}

