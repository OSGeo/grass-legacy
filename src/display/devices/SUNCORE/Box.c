
extern int SCREEN_BOTTOM ;

Box_abs(x1,y1,x2,y2)
{
	float x[5] ;
	float y[5] ;

	y1 = SCREEN_BOTTOM - y1 ;
	y2 = SCREEN_BOTTOM - y2 ;
	x[0] = x1 ; y[0] = y1 ;
	x[1] = x1 ; y[1] = y2 ;
	x[2] = x2 ; y[2] = y2 ;
	x[3] = x2 ; y[3] = y1 ;
	x[4] = x1 ; y[4] = y1 ;

	polygon_abs_2(x, y, 4) ;
}
