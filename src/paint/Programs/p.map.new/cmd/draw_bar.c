#include <stdio.h>
#include "gis.h"
#include "fullwindow.h"
#include "text.h"
#include "graphics.h"

#define METERS_TO_INCHES ((double)39.37)
#define KILOMETERS_TO_MILES  ((double)1.6) 

static double Mheight (void);

int draw_barscale (FILE *fd, int width, int length,
    double interval, char *unit, double textsize,
    double east, double north,
    int tick, int color, int background, int border)
{
double tmpe, tmpn, tmpn1, tmpn2;
double ieast ;
double initeast, initnorth;
double etmp, ntmp;
int x, y;
int i, j ;
char textval[100];
char textunit[100];
double ilength, tlength;
float numofintervals;
int   notick = 0;
double incr, ew_res_in_meters;
double e, w;
double gap = 0.;
BOX	rbox;

int x1, x2;
int y1, y2;
int xtmp, ytmp;

	ilength	= 0.;
	tlength = 0.;
	initeast = east; 
	initnorth= north;
	y2       = 0;
	if (tick) notick = 0; 
	else
		  notick = 1;
	
	G_begin_distance_calculations();
	numofintervals = (float)length/(float)interval;

	ew_res_in_meters = G_distance(east, north, east + fullwindow.ew_res, north);
	if (strcmp (unit, "km") == 0) 
	{
	   ilength = ((double) interval * 1000. * fullwindow.ew_res)
				  /ew_res_in_meters;
	   tlength = ((double) length   * 1000. * fullwindow.ew_res)
				  /ew_res_in_meters;
	}
	else 
	if (strcmp (unit, "m") == 0)
	{ 
	   ilength = ((double)interval * fullwindow.ew_res)
			          / ew_res_in_meters;
	   tlength = ((double)length * fullwindow.ew_res)
			          / ew_res_in_meters;
	}
	else 
	if (strcmp (unit, "mi") == 0)
	{
	   ilength = ((double)interval * 1.6 * 1000. * fullwindow.ew_res)
			          / ew_res_in_meters;
	   tlength = ((double)length * 1.6 * 1000. * fullwindow.ew_res)
			          / ew_res_in_meters; 
	}
	else 
	if (strcmp (unit, "ft") == 0)
	{
	   ilength = (((double)interval / (double)METERS_TO_INCHES) * 12.)
				  * fullwindow.ew_res / ew_res_in_meters;; 
	   tlength = (((double)length / (double)METERS_TO_INCHES) * 12.)
				  * fullwindow.ew_res / ew_res_in_meters;; 
	}
	else return 0;
	
	G_plot_where_xy(fullwindow.west,fullwindow.north, &x, &y);
	if (tick) { 
	   x = x + width;
	   G_plot_where_en(x, y, &tmpe, &tmpn); 
	   gap = tmpe - fullwindow.west;
	   /* width in coordinates */
	   ilength = (tlength-((numofintervals+1)*gap))/numofintervals;
	}
	else 
	{
	   x = x + width;
	   G_plot_where_en(x, y, &tmpe, &tmpn); 
	   gap = tmpe - fullwindow.west;
	   ilength = (tlength-((numofintervals-1)*gap))/numofintervals;
	}

	set_width (width);

/* the 1st end line*/
	if (tick) {
	double e1, e2, n1, n2;

	   G_plot_where_xy(east,north, &x, &y);
	   G_plot_where_en(x, y-width-width, &e1, &n1);
	   G_plot_where_en(x, y+width+width, &e2, &n2);
	   G_plot_line (e1, n1, e2, n2); 
	   G_plot_where_en(x, y-width/2-width, &tmpe, &tmpn1);
	   G_plot_where_en(x, y+width+width/2, &tmpe, &tmpn2);
	   east = east + gap;
	}

	G_plot_where_xy(east, north, &xtmp, &ytmp);
	G_plot_where_en(xtmp, ytmp, &etmp, &ntmp);

	ieast  = east;
	for (i=0; i<(int)numofintervals; i++)
        {
	   w = east;
	   incr = (ilength)/3.;
	   for (j=0; j<3; j++)
	   {
	      e	= w+incr;
	      G_plot_line(w, ntmp, e, ntmp);
	      w = e;
	   }
	   if (tick && i !=(int) numofintervals-1)
	      G_plot_line(e, tmpn1, e, tmpn2);

	   set_width (width);
	   east = east+gap+ilength;
        }




/* the 2nd end line */
	if (tick) {
	double e1, e2, n1, n2;
	   G_plot_where_xy(east-gap/2,initnorth, &x, &y);
	   G_plot_where_en(x, y-width-width, &e1, &n1);
	   G_plot_where_en(x, y+width+width, &e2, &n2);
	   G_plot_line (e1, n1, e1, n2); 
	   y2 = width + width + width;
	}


	G_plot_where_xy(ieast, initnorth, &x, &y);
	G_plot_where_en(x-width-width/2*tick, y-width*2*tick-6, &tmpe, &tmpn1);
	text_bounds("0", 0,0,&rbox, 0);
	x1= x-width-width/2*tick - (rbox.right-rbox.left)/2;
	fprintf (fd, "ref: center lower\n");
	fprintf (fd, "east: %8.2f\n", tmpe);
	fprintf (fd, "north: %8.2f\n", tmpn1);
	fprintf (fd, "text:0\n");
	
	G_plot_where_xy(east-gap, north, &x, &y);
	G_plot_where_en(x+width/2*tick, y-width*2*tick-6, &tmpe, &tmpn1);
	fprintf (fd, "ref: center lower\n");
	fprintf (fd, "east: %8.2f\n", tmpe);
	fprintf (fd, "north: %8.2f\n", tmpn1);
	sprintf (textval, "%d ", length);	
	set_text_size(textsize/Mheight());
	set_text_width(1);
	set_text_border(-1);
	set_text_background(-1);
	set_text_hwidth(0);
	set_text_rotation(0);
	text_bounds(textval, 0,0,&rbox, 0);
	G_plot_where_xy(tmpe,tmpn1, &x, &y);

        x2= x + width + width/2*tick;
	x = x + (rbox.right-rbox.left)/2; 
	G_plot_where_en(x, y, &tmpe, &tmpn1);

	x2= x2+(rbox.right-rbox.left)/2  ;

	y1= y;
	y1=y1-(rbox.bottom-rbox.top) - 5 ;
	y2= y2+y1 + width + 15 + (rbox.bottom-rbox.top);

	if (strcmp (unit, "m") == 0)
		sprintf (textunit, " meters " );
	else if (strcmp (unit, "km") == 0)
		sprintf (textunit, " kilometers " );
	else if (strcmp (unit, "mi") == 0)
		sprintf (textunit, " miles " );
	else if (strcmp (unit, "ft") == 0)
		sprintf (textunit, " feet " );

	text_bounds(textunit, 0,0,&rbox, 0);

	x2= x2+(rbox.right-rbox.left);

	if (background>=0)
	{
	int i;
	set_color(background);
	for (i=y1; i<=y2; i++)
	    draw_line (x1, i, x2,i);

	set_color(color);
	redraw_barscale(initeast, initnorth,
		(int) tlength,
		numofintervals,width,tick);
	}

	if (border>=0)
	{
	set_width(1);
	set_color(border);
	draw_line(x1-1,y1-1,x2+1,y1-1);
	draw_line(x1-1,y2+1,x2+1,y2+1);
	draw_line(x1-1,y1-1,x1-1,y2+1);
	draw_line(x2+1,y1-1,x2+1,y2+1);
	}

	fprintf(fd, "text:%s\n", textval);
	fprintf (fd, "east: %8.2f\n", tmpe);
	fprintf (fd, "ref: left lower\n");
	fprintf(fd, "text:%s\n", textunit);

	return 0;
}


static double Mheight (void)
{
	BOX box;

	set_text_border(-1);
	set_text_background(-1);
	set_text_width(1);
	set_text_size(100.0);
	text_bounds ("M",0,0,&box, 0);
	return (fullwindow.ns_res * (box.bottom-box.top+1) / 100.0) ;
}

