#include <stdio.h>
#include "gis.h"
#include "fullwindow.h"

#define METERS_TO_INCHES ((double)39.37)
#define KILOMETERS_TO_MILES  ((double)2.2) 

draw_barscale(fd,width, length, interval, unit, east, north, tick)
FILE *fd;
int width, length, interval;
char *unit;
double east;
double north;
{
double tmpe, tmpn, tmpe1,tmpe2, tmpn1, tmpn2;
double ieast, inorth;
int x, y;
int i, j, k;
char textval[100];
char textunit[100];
int ilength;
float numofintervals;
int   rlength;
int   notick = 0;
double incr;
double e, w, n;
double gap;


	if (tick) notick = 0; 
	else
		notick = 1;
	
	/*
	numofintervals = (float)length/(float)interval;

printf ("int floatnumofintervals-intnumofintervals is %f\n",
	(float)numofintervals - (int)numofintervals);
	*/
	numofintervals = (float)length/(float)interval;


	
	G_plot_where_xy(fullwindow.west,fullwindow.north, &x, &y);
	if (tick) 
	x = x + 2;
	else
	x = x + 6;
	G_plot_where_en(x, y, &tmpe, &tmpn);
	gap =  tmpe - fullwindow.west;

	ilength = length*100/numofintervals;

	rlength = ((float)numofintervals-(int)numofintervals)*ilength;

	printf ("ilength is %d\n", ilength);
	printf ("rlength is %d\n", rlength);
	printf ("length is %d\n", length);
	printf ("interval is %d\n", interval);
	
	ieast  = east;
	inorth = north;

	set_width (width);

/* the 1st end line*/
	if (tick) {
	double e1, e2, n1, n2;
	G_plot_where_xy(east,north, &x, &y);
	G_plot_where_en(x-width, y-width-width, &e1, &n1);
	G_plot_where_en(x-width, y+width+width, &e2, &n2);
	G_plot_line (e1, n1, e2, n2); 

	G_plot_where_en(x, y-width/2-width, &tmpe, &tmpn1);
	G_plot_where_en(x, y+width+width/2, &tmpe, &tmpn2);
	

	}
	
	for (i=0; i<(int)numofintervals; i++)
    {
	w		= east;
	incr 	= ilength/3;
	for (j=0; j<3; j++)
	{
	e	= w+incr;
	G_plot_line(w, inorth, e, inorth);
	w   = e;
	}
	if (tick && i !=(int) numofintervals-1){
		set_width (2);
		G_plot_line(e, tmpn1, e, tmpn2);
		}


	set_width (width);
	east = east+gap+ilength;
	}

/*

	if (rlength) {
	G_plot_where_xy(tmpe1, inorth, &x, &y);
	G_plot_where_en(x-width+3, y, &east, &north);
	for (j=0; j<width; j++) {
    G_plot_line(east,north, east+rlength,north);
	G_plot_where_xy(east+rlength, north, &x, &y);
	G_plot_where_en(x+width-1, y, &tmpe1, &north);
	G_plot_where_xy(east, north, &x, &y);
	G_plot_where_en(x, y+1, &east, &north);
	}


	}
	*/



/* the 2nd end line */
	if (tick) {
	double e1, e2, n1, n2;
	G_plot_where_xy(east,north, &x, &y);
	G_plot_where_en(x-width, y-width-width, &e1, &n1);
	G_plot_where_en(x-width, y+width+width, &e2, &n2);
	G_plot_line (e1, n1, e2, n2); 
	}

	G_plot_where_xy(ieast, inorth, &x, &y);
	G_plot_where_en(x-width-width*tick, y-width*tick-6, &tmpe, &tmpn1);

	fprintf (fd, "ref: left lower\n");
	fprintf (fd, "east: %8.2f\n", tmpe);
	fprintf (fd, "north: %8.2f\n", tmpn1);
	fprintf (fd, "text:0\n");
	
	G_plot_where_xy(east-gap, inorth, &x, &y);
	G_plot_where_en(x, y-width*tick-6, &tmpe, &tmpn1);
	fprintf (fd, "ref: right lower\n");
	fprintf (fd, "east: %8.2f\n", tmpe);
	fprintf (fd, "north: %8.2f\n", tmpn1);


	if (strcmp (unit, "m") == NULL)
	{ int itmp;
		itmp = length * 100;
		sprintf (textval, "%d ", itmp);
		sprintf (textunit, " meters " );
	}
	else if (strcmp (unit, "km") == NULL)
	{
		sprintf (textval, "%d ", length);
		sprintf (textunit, " kilometers " );
		}

	else if (strcmp (unit, "mi") == NULL)
	{
		sprintf (textval, "%8.2f ", length/KILOMETERS_TO_MILES);
		sprintf (textunit, " miles " );
		}
	else if (strcmp (unit, "ft") == NULL)
	{
		sprintf (textval, "%8.2f ", METERS_TO_INCHES*length*100*16);
		sprintf (textunit, " feet " );
		}


	fprintf(fd, "text:%s\n", textval);
	fprintf (fd, "ref: left lower\n");
	fprintf(fd, "text:%s\n", textunit);

}





