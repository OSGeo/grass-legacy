# include <stdio.h>
static float T = 0. ;
static float DY = 100. ;
static float L = 0. ;
static float DX = 100. ;
static int cur_x = 0 ;
static int cur_y = 0 ;

line (x1, y1, x2, y2)
{
	float x_convert() ;
	float y_convert() ;
	if (x1 != cur_x || y1 != cur_y)
		printf("m %.2f %.2f\n", x_convert(x1), y_convert(y1)) ;
	printf("d %.2f %.2f\n", x_convert(x2), y_convert(y2)) ;
	cur_x = x2 ;
	cur_y = y2 ;
}

space (l, b, r, t)
{ T = (float)t; DY = (float)(b-t); DX = (float)(r-l); L = (float)l; }

linemod (s)
{ printf ("c %s\n", s) ; }

move (x, y)
{
	float x_convert() ;
	float y_convert() ;
	printf("m %.2f %.2f\n", x_convert(x), y_convert(y)) ;
	cur_x = x ;
	cur_y = y ;
}

erase ()
{}

openpl ()
{}

closepl ()
{}

float
x_convert(x)
{
	return(100. * (x - L) / DX) ;
}

float
y_convert(y)
{
	return(100. * (y - T) / DY) ;
}
