/*  %W%  %G%  */

#define COUNT_CM 400.0
#define MAIN
#define ERR -1
#include <stdio.h>
#include "gis.h"
#include "mdef.h"


d_get_cord(x,y,xr,yr,p)
double x,y,*xr,*yr;
int p;
{
    int t, b, l, r ;
    char window_name[64] ;
    double scale;
    FILE *scalefp;


/* Initialize the GIS calls */
    G_gisinit("get_cord") ;

    R_open_driver();

    if (D_get_cur_wind(window_name))
	G_fatal_error("No current graphics window") ;

    if (D_set_cur_wind(window_name))
	G_fatal_error("Current graphics window not available") ;


/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting screen window") ;

    if ((scalefp = fopen(".d_scale","r")) == NULL) {
	 fprintf(stderr,"get_cord: ERROR can not open scale file\n");
	 fprintf(stderr,"get_cord: Please redisplay you map\n");
	exit(ERR);
	 }
    else {
	 fscanf(scalefp,"%lf",&scale);
	 fclose(scalefp);
	 }
    where_am_i(l,b,scale,x,y,xr,yr,p) ;

    fprintf(stderr,"\n") ;

    R_close_driver();
}

where_am_i(l,b,scale,x,y,xr,yr,p)
int l,b;
double scale,x,y,*xr,*yr;
int p;
{
    int screen_x, screen_y ;
    int button ;
    int white, black ;

    
    white = D_translate_color("white") ;
    black = D_translate_color("black") ;


scale = COUNT_CM * scale;
fprintf(stderr,"Buttons:\nLeft:   where am i\nRight:  select point and quit\n\n");

    while (button != 3) 
    {
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	{
	if (button == 1) {
	    *xr = (screen_x - l)/scale - x;
	    *yr = (b - screen_y)/scale  - y;
	    fprintf(stderr,"-x %3.3lf\n-y %3.3lf\n",*xr,*yr);
	   }
	if (button == 3) {
	    *xr = (screen_x - l)/scale - x;
	    *yr = (b - screen_y)/scale  - y;
            if (p) printf("-x %3.3lf\n-y %3.3lf\n",*xr,*yr);
	    if (!(isatty(1))) 
            	fprintf(stderr,"-x %3.3lf\n-y %3.3lf\n",*xr,*yr);
	   }
	}
    } 

}

