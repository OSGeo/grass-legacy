#include "gis.h"
#include "dlg.h"
#include "graphics.h"
#include "parms.h"
#include "misc.h"
#include "fullwindow.h"
#include "colormode.h"
#include "pattern.h"

draw_ramp(x, y,w,h,orient, statf)
double x, y;
int w,h;
int orient;
struct Cell_stats *statf;
{
int legendx, legendy ;
unsigned char black;
int		i, j, k;
int		c;
int 	catnum;
long	count;
int     numofcats;
int 	ilegendy;
int 	ilegendx;

	black	= BLACK;
	numofcats = 0;

	G_plot_where_xy(x, y, &legendx, &legendy);

	ilegendy	= legendy;
	ilegendx	= legendx;

	set_diffusion_color(w);

	if (!orient) {
	/*vertical ramp*/
	   G_rewind_cell_stats(statf);
	   while (G_next_cell_stat(&catnum, &count, statf) )
	   {
	      numofcats++;
	      for (j=legendy+2; j<=legendy+h+2; j++)
	      {
	          k = 0;
	          for (i=legendx+2; i<=legendx+w-2; i++) {
		      c = get_catcolr(catnum, j, k);
		      set_color(c);
		      dot(i,j);
		      k++;
		  }
	      }
	      legendy	= legendy+h;
	   }
	}
	else {
	   G_rewind_cell_stats(statf);
	   /* horizontal ramp */
	   while (G_next_cell_stat(&catnum, &count, statf) )
	   {
	      numofcats++;
	      for (j=legendy+2; j<=legendy+h-2; j++)
	      {
	          k = 0;
		  for (i=legendx+2; i<=legendx+w+2; i++) {
		      c = get_catcolr(catnum, j, k);
		      set_color(c);
		      dot(i,j);
		      k++;
		  }
	      }
	      legendx = legendx + w ;
	   }

	}

	set_color (black);
        if (!orient){
        /* vertical ramp */
	   draw_line (legendx,ilegendy,legendx+w, ilegendy);
	   draw_line (w+legendx, ilegendy, w+legendx, h*numofcats+ilegendy +2);
	   draw_line (legendx,ilegendy,legendx, h*numofcats+ilegendy+2);
	   draw_line (legendx,h*numofcats+ilegendy+2,w+legendx, h*numofcats+ilegendy+2);
	}
	else {
        /* horizontal ramp */
	   draw_line (ilegendx,ilegendy,w*numofcats+ilegendx+2, ilegendy);
	   draw_line (ilegendx, ilegendy, ilegendx, h+ilegendy);
	   draw_line (ilegendx,ilegendy+h,w*numofcats+ilegendx+2, ilegendy+h);
	   draw_line (w*numofcats+ilegendx+2,ilegendy,w*numofcats+ilegendx+2, ilegendy+h);
	}

}
