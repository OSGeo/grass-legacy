#include "gis.h"
#include "graphics.h"
#include "text.h"
#include "misc.h"
#include "parms.h"
#include "fullwindow.h"

extern double floor() ;

do_rscale()
{
    double g;
	double ieast, inorth;
    double east, west, incr;
    int i;
	int len	= 500;
	int interval = 2;
	int x, y;
	int x1, y1;

/*
	ieast = 695516.80; 
	inorth = 5467444.00;
	*/
	ieast = 695077.00; 
	inorth = 5467884.05;

printf (" in do rscale \n");

	G_plot_where_xy(ieast, inorth, &x1,&y1);

/* set color to black, line width to 1 */

    set_color (BLACK);
    set_width (1);


/* vertical lines */


printf (" x1 is %d\n", x1);
printf (" y1 is %d\n", y1);

	for (i=0; i<interval; i++)
    {
    G_plot_line(ieast,inorth, ieast+len,inorth);
	set_color(WHITE);
	ieast = ieast+len+20;
    }
}


