#include "colors.h"
#include <stdio.h>
main()
{
	int row ;
	int r, c ;
	int xarr[4], yarr[4] ;
	int x_pos ;
	int y_pos ;
	int nrows ;
	int npixles ;
	int b ;
	char buff[128] ;

	Graph_Set() ;
	x_pos = 10 ;
	y_pos = 10 ;
	nrows = -40 ;
	npixles = 40 ;

	xarr[0] = npixles ;
	yarr[0] = 0 ;
	xarr[1] = 0 ;
	yarr[1] = nrows ;
	xarr[2] = -npixles ;
	yarr[2] = 0 ;
	xarr[3] = 0 ;
	yarr[3] = -nrows ;

	Move_abs(0,40) ;
	Color(RED) ;
	Polygon_rel(xarr, yarr, 4) ;

	Move_abs(0,80) ;
	Color(GREEN) ;
	Polygon_rel(xarr, yarr, 4) ;

	Move_abs(0,120) ;
	Color(BLUE) ;
	Polygon_rel(xarr, yarr, 4) ;

	Get_location_with_pointer(&x_pos, &y_pos, &b) ;

	sprintf(buff,"echo 'GOT: x: %d y: %d b: %d %o' > /dev/console\n", x_pos, y_pos, b, b) ;
	system(buff) ;
}
