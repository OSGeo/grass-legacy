
#include "text.h"



void  dobg(tx,ty, bx,by, lx,ly, rx, ry, bg)
{
int xleft, xright;
int i;
int c1, c2, c3, c4;
float grad1, grad2;
int	fx,fy, sx, sy; 

if (ry >= ly) {
	fx 	= lx;
	fy	= ly;
	sx	= rx;
	sy	= ry;
	}
else {
	fx 	= rx;
	fy	= ry;
	sx	= lx;
	sy	= ly;
	}

grad1 =  (float) (ty - fy) / (float) ( tx - fx);
c1	  = ty - grad1 * tx;

grad2 =  (float) (ty - sy) / (float)( tx - sx);
c2	  = ty - grad2 * tx;

c3	  = sy - grad1 * sx;

c4	  = fy - grad2 * fx;

set_color (bg);



for (i=ty; i<fy; i++) {
	xleft = (i - c1) / grad1;
	xright= (i - c2) / grad2;
	draw_line (xleft, i, xright, i);
	}
	
for (i=fy; i<sy; i++) {
	xleft = (i - c2) / grad2;
	xright= (i - c4) / grad2;
	draw_line (xleft, i, xright, i);
	}

for (i=sy; i<by; i++) {
	xleft = (i - c3) / grad1;
	xright= (i - c4) / grad2;
	draw_line (xleft, i, xright, i);
	}



}
