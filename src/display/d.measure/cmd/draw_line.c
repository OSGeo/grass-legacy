draw_line(screen_x,screen_y,cur_screen_x,cur_screen_y,color1,color2) 
	int screen_x,screen_y,cur_screen_x,cur_screen_y ;
{
	R_standard_color(color1);
	R_move_abs(cur_screen_x, cur_screen_y) ;
	R_cont_abs(screen_x, screen_y) ;
	R_standard_color(color2);
	if(abs(screen_y-cur_screen_y) <= abs(screen_x-cur_screen_x))
	{
		R_move_abs(cur_screen_x, cur_screen_y-1) ;
		R_cont_abs(screen_x, screen_y-1) ;
	}
	else
	{
		R_move_abs(cur_screen_x+1, cur_screen_y) ;
		R_cont_abs(screen_x+1, screen_y) ;
	}
	
	R_flush() ;
}
