#include <stdio.h>
#define METERS_TO_INCHES ((double)39.37)
#define KILOMETERS_TO_MILES  ((double)2.2) 

draw_barscale(fd,width, length, interval, unit, east, north, tick)
FILE *fd;
int width, length, interval;
char *unit;
double east;
double north;
{
double tmpe, tmpe1,tmpe2, tmpn1, tmpn2;
double ieast, inorth;
int x, y;
int i, j, k;
char textval[100];
char textunit[100];
int ilength;
float numofintervals;
int   rlength;
int   notick = 0;;


	if (tick) notick = 0; 
	else
		notick = 1;
	
	numofintervals = (float)length/(float)interval;

	(float)numofintervals - (int)numofintervals);


	ilength = length*1000/numofintervals;

	rlength = ((float)numofintervals-(int)numofintervals)*ilength;

	
	ieast  = east;
	inorth = north;

/* the 1st end line*/
	if (tick) {
	G_plot_where_xy(east,north, &x, &y);
	for (j=-1; j<width-1; j++) {
	G_plot_where_en(x-j, y-width, &tmpe, &tmpn1);
	G_plot_where_en(x-j, y+width+width, &tmpe, &tmpn2);
    G_plot_line(tmpe,tmpn1,tmpe,tmpn2);
	}
	}
	
	
	for (j=0; j<width+1; j++) {
	for (i=0; i<(int)numofintervals; i++)
    {
	if (j ==0) 
		continue;
		
    G_plot_line(east,north, east+ilength,north);
	G_plot_where_xy(east+ilength, north, &x, &y);
	if (tick) {
	if (j==1 && i != (int)numofintervals-1) {
	for (k=0; k<2; k++) {
		G_plot_where_en(x+k, y-width/2-width, &tmpe, &tmpn1);
		G_plot_where_en(x+k, y+width+width/2, &tmpe, &tmpn2);
		G_plot_line(tmpe, tmpn1, tmpe, tmpn2);
		}
	

	}
	}
	G_plot_where_en(x+notick*2+2, y, &east, &north);
	G_plot_where_en(x+tick*width-1, y, &tmpe1, &north);
    }

	G_plot_where_xy(ieast, north, &x, &y);
	G_plot_where_en(x, y+1, &east, &north);
	
	}

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


/* the 2nd end line */
	if (tick) {
	G_plot_where_xy(tmpe1,inorth, &x, &y);
	for (j=1; j<width+1; j++) {
	G_plot_where_en(x-j, y-width, &tmpe, &tmpn1);
	G_plot_where_en(x-j, y+width+width, &tmpe, &tmpn2);
    G_plot_line(tmpe,tmpn1,tmpe,tmpn2);
	}
	}

	G_plot_where_xy(ieast, inorth, &x, &y);
	G_plot_where_en(x-width*tick, y-width-2, &tmpe, &tmpn1);

	fprintf (fd, "ref: left lower\n");
	fprintf (fd, "east: %8.2f\n", tmpe);
	fprintf (fd, "north: %8.2f\n", tmpn1);
	fprintf (fd, "text:0\n");
	
	G_plot_where_xy(tmpe1, inorth, &x, &y);
	G_plot_where_en(x, y-width-2, &tmpe, &tmpn1);
	fprintf (fd, "ref: right lower\n");
	fprintf (fd, "east: %8.2f\n", tmpe);
	fprintf (fd, "north: %8.2f\n", tmpn1);


	if (strcmp (unit, "m") == NULL)
	{ int itmp;
		itmp = length * 1000;
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
		sprintf (textval, "%8.2f ", METERS_TO_INCHES*length*1000*16);
		sprintf (textunit, " feet " );
		}



	fprintf(fd, "text:%s\n", textval);
	fprintf (fd, "ref: left lower\n");
	fprintf(fd, "text:%s\n", textunit);

}





