#include <stdio.h>
#include "fullwindow.h"
#include "gis.h"
#include "graphics.h"

int redraw_barscale (double initeast, double initnorth,
    int tlength, double numofintervals, int width, int tick)
{
double tmpe, tmpn, tmpn1, tmpn2;
int x, y;
int i, j ;
int ilength;
int notick = 0;
double incr;
double e, w;
double gap;
int xtmp, ytmp; 
double etmp, ntmp;

	if (tick) notick = 0; 
	else
		  notick = 1;

	G_plot_where_xy(fullwindow.west,
		fullwindow.north, &x, &y);

	if (tick) { 
	   x = x + width;
	   G_plot_where_en(x, y, &tmpe, &tmpn); 
	   gap = tmpe - fullwindow.west;
	   ilength = (tlength-((numofintervals+1)*gap))/numofintervals/*-(width*2)*/;
	}
	else
	if (notick) {
	   x = x + width/2 + 6;
	   G_plot_where_en(x, y, &tmpe, &tmpn);
	   gap = tmpe - fullwindow.west;
	   ilength = (tlength-((numofintervals-1)* gap))/numofintervals-(width*2);
	}

	set_width (width);

/* the 1st end line*/
	if (tick) {
	double e1, e2, n1, n2;
	   G_plot_where_xy(initeast,initnorth, &x, &y);
	   G_plot_where_en(x, y-width-width, &e1, &n1);
	   G_plot_where_en(x, y+width+width, &e2, &n2);
	   G_plot_line (e1, n1, e2, n2); 
	   G_plot_where_en(x, y-width/2-width, &tmpe, &tmpn1);
	   G_plot_where_en(x, y+width+width/2, &tmpe, &tmpn2);
	   initeast = initeast + gap;
	}

G_plot_where_xy (initeast, initnorth, &xtmp, &ytmp);
G_plot_where_en (xtmp, ytmp, &etmp , &ntmp);

	set_width(width);
	
	for (i=0; i<(int)numofintervals; i++)
        {
	    w = initeast;
	    incr = (ilength)/3;
	    for (j=0; j<3; j++)
	    {
	       e = w+incr;
	       G_plot_line(w, ntmp, e, ntmp);
	       w = e;
	    }
	    if (tick && i !=(int) numofintervals-1)
	       G_plot_line(e, tmpn1, e, tmpn2);

	    set_width (width);
	    initeast = initeast+gap+ilength;
         }

         set_width (width);
/* the 2nd end line */
	if (tick) {
	double e1, e2, n1, n2;
	   G_plot_where_xy(initeast-gap/2,initnorth, &x, &y);
	   G_plot_where_en(x, y-width-width, &e1, &n1);
	   G_plot_where_en(x, y+width+width, &e2, &n2);
	   G_plot_line (e1, n1, e2, n2); 
	}

	return 0;
}


