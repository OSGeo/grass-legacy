Get_text_box(text, t, b, l, r)
	char *text ;
	int *t, *b, *l, *r ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;
	extern int SCREEN_BOTTOM ;
	soft_text_ext(current_x_pos, SCREEN_BOTTOM - current_y_pos, text) ;
	get_text_ext (t, b, l, r) ;
}
