#include <stdio.h>
#include "colors.h"
Erase()
{
	int xarr[4], yarr[4] ;
	extern int SCREEN_LEFT  ;
	extern int SCREEN_BOTTOM ;
	extern int SCREEN_RIGHT  ;
	extern int SCREEN_TOP ;

	xarr[0] = SCREEN_LEFT ;
	yarr[0] = SCREEN_TOP ;
	xarr[1] = SCREEN_RIGHT ;
	yarr[1] = SCREEN_TOP ;
	xarr[2] = SCREEN_RIGHT ;
	yarr[2] = SCREEN_BOTTOM ;
	xarr[3] = SCREEN_LEFT ;
	yarr[3] = SCREEN_BOTTOM ;
	Color(BLACK) ;
	Polygon_abs(xarr, yarr, 4) ;
	fflush(stdout) ;
}
