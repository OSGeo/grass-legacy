#include "text.h"


# define RpD ((2 * 3.1415926535897932384626434) / 360.0) /* radians/degree */
# define D2R(d) ((double)(d * RpD))         /* degrees->radians */



double sin(), cos();
extern	BOX rbox;


void getbox(rot, border, bg )
float	rot;
int		border;
int		bg;
{
int x1, y1, x2,y2, x3,y3, x4,y4;
int	width, height;
float rot1;
float sinrot, cosrot;
float sinrot90, cosrot90;
int	yhalf, xhalf;

rot1	= 90.0 - rot;

sinrot = sin (D2R(rot));
cosrot = cos (D2R(rot));
cosrot90 = cos (D2R(rot1));
sinrot90 = sin (D2R(rot1));


/*
printf ("rbox.bottom is %d \n", rbox.bottom);
printf ("rbox.top is %d \n", rbox.top);
printf ("rbox.left is %d \n", rbox.left);
printf ("rbox.right is %d \n", rbox.right);
*/

height	= rbox.bottom - rbox.top;
width	= rbox.right - rbox.left;


xhalf	= 0.5 *height*cosrot90;
yhalf	= 0.5 *height*sinrot90;


if (rot > 0.0 & rot <= 90.0) {

	if ( rot == 90.0) {
	x1 = rbox.left + xhalf + 0.5 * xhalf;
	y1 = rbox.bottom + yhalf;
	}
	else {
	x1 = rbox.left + xhalf;
	y1 = rbox.bottom + yhalf;

	}
	}
else if (rot > 90.0 && rot <= 180.0) {
	if ( rot == 180.0) {
    x1= rbox.left   ;
    y1= rbox.bottom + yhalf  +0.5* yhalf ;
	}
	else {
    x1= rbox.left  + 0.7 * xhalf  ;
    y1= rbox.bottom + yhalf;
	}
	}
else if (rot > 180.0 && rot <= 270.0) {
	if (rot == 270.0) {
    x1= rbox.left  +  xhalf + 0.5* xhalf ;
    y1= rbox.bottom  /*+ 0.5 * yhalf*/ ;
	}
	else {
    x1= rbox.left  + xhalf/*  0.5 * xhalf*/  ;
    y1= rbox.bottom + 0.5 * yhalf ;
	}
}
else if (rot > 270.0 && rot < 360.0) {
	x1= rbox.left + xhalf;
	y1= rbox.bottom + yhalf;
}
else   
{
x1= rbox.left   ;
y1= rbox.bottom ;
}


x2= width * cosrot + x1 ;
y2= y1 - width * sinrot ;

x3= x2-height * cosrot90 ;
y3= y2-height * sinrot90 ;

x4= x1  - height * cosrot90 ; 
y4= y1  - height * sinrot90  ; 



if (bg >=0) {
if (rot == 0.0 || rot == 90.0 || rot == 180.0 || rot == 270.0 || rot == 360.0) {	
	int i;
	int top, bottom;
	set_color(bg);
	if (y3 < y2)  { bottom=y3; top=y2;}
	else if (y3 > y2) { bottom= y2; top= y3; } 
	else if (y4 < y2) {top= y2; bottom= y4; }
	else { top = y4; bottom=y2;}
	for (i=bottom; i<=top; i++)
		draw_line(x1, i, x3, i);
		}
else
if (rot > 0.0 && rot < 90.0) 
dobg(x3,y3, x1,y1, x4,y4, x2,y2, bg);
else if (rot > 90.0 && rot < 180.0)
dobg(x2,y2, x4,y4, x3, y3, x1, y1, bg); 
else if (rot > 180.0 && rot < 270.0)
dobg(x1,y1, x3,y3, x2,y2, x4, y4, bg); 
else if (rot > 270.0 && rot < 360.0)
dobg(x4,y4, x2,y2, x1,y1, x3, y3, bg); 

}
if (border >=0) {
set_color(border);
draw_line(x1, y1, x2, y2);
draw_line(x2, y2, x3, y3);
draw_line(x3, y3, x4, y4);
draw_line(x4, y4, x1, y1);
}



}
