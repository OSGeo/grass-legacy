/* These return the values as defined in the Graph_Set() routine */

extern int SCREEN_LEFT;
extern int SCREEN_RIGHT;
extern int SCREEN_BOTTOM;
extern int SCREEN_TOP;
#define  NCOLORS    256

#include "driverlib.h"

int Screen_left (int *index)
{
	*index = SCREEN_LEFT ;
	return 0;
}

int Screen_rite (int *index)
{
	*index = SCREEN_RIGHT ;
	return 0;
}

int Screen_bot (int *index)
{
	*index = SCREEN_BOTTOM ;
	return 0;
}

int Screen_top (int *index)
{
	*index = SCREEN_TOP ;
	return 0;
}

int Get_num_colors (int *index)
{
	*index = NCOLORS ;
	return 0;
}

int get_num_colors (void)
{
	return NCOLORS ;
}
