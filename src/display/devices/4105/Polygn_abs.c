#include <stdio.h>
#define ESC 033
#define BS	010
#define US	031

Polygon_abs(xarray, yarray, number)
	int xarray[], yarray[] ;
	int number ;
{
	int i ;
	int n ;
	char *s ;
	char *encode_xy() ;
	extern int current_x_pos ;
	extern int current_y_pos ;

/* If any part of the polygon is outside window, ignore ad return */
	for(n=0; n<number; n++)
		if (! is_inside(xarray[n], yarray[n]))
			return ;

/* Begin panel */
	s = encode_xy(xarray[0],yarray[0]) ;
	printf("%cLP%c%c%c%c%c", ESC, s[0],s[1],s[2],s[3],s[4]) ;
	printf("%c", '0') ;    /* Do not display panel boundaries */

/* Send coors */
	for(n=1; n<number; n++)
	{
		s = encode_xy(xarray[n],yarray[n]) ;
		printf("%cLG%c%c%c%c%c", ESC, s[0],s[1],s[2],s[3],s[4]) ;
	}

/* End panel */
	printf("%cLE", ESC) ;
	
/*
	current_x_pos = xarray[number-1] ;
	current_y_pos = yarray[number-1] ;
*/

	fflush(stdout) ;
}
