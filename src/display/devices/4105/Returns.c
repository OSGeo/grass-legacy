extern int SCREEN_LEFT ;
extern int SCREEN_RIGHT ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_TOP ;
extern int N_COLORS ;

Screen_left(index)
	int *index ;
{
	*index = SCREEN_LEFT ;
}

Screen_rite(index)
	int *index ;
{
	*index = SCREEN_RIGHT ;
}

Screen_bot(index)
	int *index ;
{
	*index = SCREEN_BOTTOM ;
}

Screen_top(index)
	int *index ;
{
	*index = SCREEN_TOP ;
}

Get_num_colors(index)
	int *index ;
{
	*index = N_COLORS ;
}

get_num_colors()
{
        return N_COLORS ;
}
