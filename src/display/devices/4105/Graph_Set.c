#define ESC 033

#include "graphics.h"
#include "stdio.h"
#include "colors.h"

int current_x_pos = 0 ;
int current_y_pos = 0 ;
int color_lookup[1024] ;
int N_COLORS      ;
int CUR_COL ;
int FILL_PAT ;
int TEXT_COL ;
int SCREEN_LEFT ;
int SCREEN_RIGHT ;
int SCREEN_BOTTOM ;
int SCREEN_TOP ;

Graph_Set() 
{
	int i ;
	SCREEN_LEFT	  = 0 ;
	SCREEN_RIGHT  = 542/2 ; /* 639 for pixle,  542 for square image */
	SCREEN_BOTTOM = 399/2 ;
	SCREEN_TOP    = 0 ;
	N_COLORS      = 125 ;

	for(i=0; i<1024; i++)
		color_lookup[i] = i % 125 + 50 ;

	fixed_color[- GRAY]   = 81 ;  /* gray */
	fixed_color[- WHITE]  =174 ;  /* white */
	fixed_color[- BLACK]  = 50 ;  /* black */
	fixed_color[- VIOLET] =154 ;  /* violet */
	fixed_color[- INDIGO] =104 ;  /* indigo */
	fixed_color[- BLUE]   = 54 ;  /* blue */
	fixed_color[- GREEN]  = 70 ;  /* green */
	fixed_color[- YELLOW] =170 ;  /* yellow */
	fixed_color[- ORANGE] =155 ;  /* orange */
	fixed_color[- RED]    =150 ;  /* red */

	line_color[- RED]    =  2 ;  /* red */
	line_color[- GRAY]   =  1 ;  /* gray */
	line_color[- WHITE]  =  1 ;  /* white */
	line_color[- BLACK]  =  0 ;  /* black */
	line_color[- VIOLET] =  6 ;  /* violet */
	line_color[- INDIGO] =  5 ;  /* indigo */
	line_color[- BLUE]   =  4 ;  /* blue */
	line_color[- GREEN]  =  3 ;  /* green */
	line_color[- YELLOW] =  7 ;  /* yellow */
	line_color[- ORANGE] =  2 ;  /* orange */

/* Set text size land rotation value */
	_text_size_x = 1.0 ;
	_text_size_y = 1.0 ;
	_text_rotation = 0.0 ;

/* Set line colors */
	current_color = 1 ;
	last_color = 1 ;
/* set linestyle to solid */
	printf("%cMV0", ESC) ;
/* set line color to white */
	printf("%cML%c", ESC, '1') ;
}
