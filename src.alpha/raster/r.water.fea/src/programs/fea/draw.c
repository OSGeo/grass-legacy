#define TSIZE .02
#define NO_TICKS 10
#define PRIN 41
#include "gis.h"
#include "hyd.h"
#define SIZE .03

void 
draw_hydrograph()
{
	diffy =(double) b - t;
	diffx = (double) r - l;

R_standard_color(D_translate_color("gray"));
/*  x axis and division of Y axis */
R_text_size((int)(hsize*diffx),(int)(vsize * diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.15*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.15*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.23*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.23*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.31*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.31*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.39*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.39*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.47*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.47*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.55*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.55*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.63*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.63*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.71*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.71*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.79*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.79*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.87*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.87*diffy));
R_move_abs(l +(int)(0.12*diffx) , b -(int)(0.95*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.95*diffy));
/*  Y axis and  division of X axis */
R_move_abs(l +(int)(0.15*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.15*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.23*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.23*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.31*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.31*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.39*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.39*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.47*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.47*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.55*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.55*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.63*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.63*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.71*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.71*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.79*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.79*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.87*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.87*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.95*diffx) , b -(int)(0.12*diffy));
R_cont_abs(l +(int)(0.95*diffx) , b -(int)(0.95*diffy));
R_move_abs(l +(int)(0.45*diffx) , b -(int)(0.07*diffy));
R_text_size((int)(TSIZE * diffx), (int)(TSIZE * diffy)) ;
R_text("Time[minutes]");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.80*diffy));
R_text("D");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.75*diffy));
R_text("I");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.70*diffy));
R_text("S");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.65*diffy));
R_text("C");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.60*diffy));
R_text("H");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.55*diffy));
R_text("A");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.50*diffy));
R_text("R");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.45*diffy));
R_text("G");
R_move_abs(l +(int)(0.04*diffx) , b -(int)(0.40*diffy));
R_text("E");
R_move_abs(l +(int)(0.01*diffx) , b -(int)(0.35*diffy));
R_text("[CUSECS]");
}

void
make_setup()
{
	char window_name[64];
	if (D_get_cur_wind(window_name))
	  	G_fatal_error("No current window") ;
    	if (D_set_cur_wind(window_name))
      		G_fatal_error("Current window not available") ;
    	if (D_get_screen_window(&t, &b, &l, &r))
	  	G_fatal_error("Getting screen window") ;

	R_set_window(t,b,l,r);

    diffy =(double) b - t;
    diffx = (double) r - l;

	hsize = vsize = SIZE;

	draw_hydrograph();
}

putxy(xarray,yarray,monit_time)
int monit_time;
double *xarray,*yarray;
{
	char time[100],ytop[100];
	int i;
	double xper,yper,scale = 0;
	yarray[0] = 0;
	for(i=0;i<PRIN;i++)
		if(yarray[i] > scale)
			scale = yarray[i];
	scale = scale + 1;
	R_standard_color(D_translate_color("yellow"));
	R_move_abs(l +(int)(0.1*diffx) , b -(int)(0.15*diffy));
	R_text("0");
	R_move_abs(l +(int)(0.15*diffx) , b -(int)(0.1*diffy));
	R_text("0");
	R_move_abs(l +(int)(0.15*diffx) , b -(int)(0.15*diffy));

	for(i=0;i<PRIN;i++){
		xper = .15 + (.8 * xarray[i]);
		yper = .15 + (.8 * (yarray[i]/scale));
		R_cont_abs(l +(int)(xper*diffx) , b -(int)(yper*diffy));
		R_flush();
	}
	/* put other details */
	/* ticks for the division of y axis */
	{
		double one_tick;
		one_tick = scale/10;
		/* use ytop to store ticks */
		for(i=1;i<=NO_TICKS;i++){
			yper = 0.15 + .08*i;
			sprintf(ytop,"%-8.2lf",one_tick * i);
			R_move_abs(l +(int)(0.08*diffx) , b -(int)(yper*diffy));
			R_text(ytop);
		}
		/*ticks for the division of x axis */
		one_tick = monit_time/10;
		for(i=1;i<=NO_TICKS;i++){
			xper = 0.12 + .08*i;
			sprintf(time,"%-8d",(int)one_tick * i);
			R_move_abs(l +(int)(xper*diffx) , b -(int)(0.1*diffy));
			R_text(time);
		}
	}
	R_move_abs(l +(int)(0.15*diffx) , b -(int)(0.15*diffy));
}	
