#include "graphics.h"
#define ONE	'1'
#define TWO	'2'
#define THR	'3'

Get_location_with_line(cx, cy, wx, wy, button)
	int cx, cy ;
	int *wx, *wy ;
	int *button ;
{
	char buff[10] ;
	char first ;
	int x, y ;
	int a, b ;

	for(;;)
	{
		printf("%c%c", 033, 032) ;
		gets(buff) ;
		if(buff[0] != ONE && buff[0] != TWO && buff[0] != THR)
			continue ;
		a = buff[1]; b = buff[2] ;
		x = ((a & 037) << 5 ) | (b & 037) ;
		a = buff[3]; b = buff[4] ;
		y = ((a & 037) << 5 ) | (b & 037) ;
		tek_to_screen(x, y, wx, wy) ;
		*button = buff[0] - '0' ;
		return ;
	}
}
