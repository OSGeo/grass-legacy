/*
* These return the values as defined in the Set_env() routine 
*
*  Written by the GRASS Team in the Winter of 88.
*/

extern int SCREEN_LEFT	  ;
extern int SCREEN_RIGHT  ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_TOP    ;
extern int NCOLORS    ;

/*
*  Take the position on the full screen and convert it to the position
*  inside the working area of the sub-window.
*/

Screen_left(index)
	int *index ;
{
	*index = 0 ;
}

Screen_rite(index)
	int *index ;
{
	*index = SCREEN_RIGHT - SCREEN_LEFT + 1 ;
}

Screen_bot(index)
	int *index ;
{
	*index = SCREEN_BOTTOM - SCREEN_TOP + 1 ;
}

Screen_top(index)
	int *index ;
{
	*index = 0 ;
}

/*  Returns the number of active colors  */

Get_num_colors(index)
	int *index ;
{
	*index = NCOLORS ;
}

get_num_colors()
{
	return NCOLORS ;
}
