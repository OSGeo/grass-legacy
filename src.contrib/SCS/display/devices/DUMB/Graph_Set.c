/* %W%   %G% */
#include "driver.h"
#include "deblib/gisglobals.h"
#include <stdio.h>

int SCREEN_LEFT ;
int SCREEN_RIGHT ;
int SCREEN_BOTTOM ;
int SCREEN_TOP ;
int NCOLORS;
int cur_color;

Graph_Set() 
{
	int r, g, b ;
	unsigned char R, G, B ;
	int i ;
	FILE *freopen();

	/* Generate "fixed" color table */
	i = 0 ;
	for(r = 0; r < 5; r++)
	{	R = (int)(r * 63.75) ;
		for(g = 0; g < 5; g++)
		{	G = (int)(g * 63.75) ;
			for(b = 0; b < 5; b++)
			{	B = (int)(b * 63.75) ;
				Reset_color(R, G, B, i++) ;
			}
		}
	}

	SCREEN_LEFT   = 0 ;
	SCREEN_RIGHT  = 639 ;
	SCREEN_BOTTOM = 399 ;
	SCREEN_TOP    = 0 ;
	NCOLORS      = 125 ;

	cur_x = 0;
	cur_y = 0;

	freopen("/dev/null","w",stdout);
	enter_gmode() ;
}
