#include <stdio.h>
#include "colors.h"
#define ESC 033

Text(text)
	char *text ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;
	extern int SCREEN_BOTTOM ;
	soft_text(current_x_pos, current_y_pos, text) ;
	fflush(stdout) ;
}

text_line(x,y)
	int x,y ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;
	int cx, cy ;
	char *encode_xy() ;
	char *s ;

/* make sure line color is current_color */
	if (last_color != current_color)
	{
		last_color = current_color ;
		printf("%cML%c", ESC, '0' + (char)last_color) ;
	}

	if (! is_inside(current_x_pos, current_y_pos))
	{
		move_abs_no_update(x,y) ;
		return ;
	}

	cx = current_x_pos ;
	cy = current_y_pos ;

	move_abs_no_update(x,y) ;

	if (! is_inside(current_x_pos, current_y_pos))
		return ;

	s = encode_xy(x,y) ;
	printf("%cLG%c%c%c%c%c", ESC, s[0],s[1],s[2],s[3],s[4]) ;
}
