/*  @(#)mouse_yn.c	1.1  6/26/87  */
mouse_yes_no ( header)
	char	*header ;
{
	int button ;
	int	screen_x, screen_y ;

	screen_x = screen_y = 1;

	_Clear_base () ;
	Write_base(10, header) ;
	Write_base(12, "    Buttons:") ;
	Write_base(13, "       Left:   yes") ;
	Write_base(14, "       Middle: no") ;
	Write_base(15, "       Right:  yes") ;


	R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;

	return (!(button == 2)) ;
}

/* this is for node_lines () */
mouse_next_prev ( header)
	char	*header ;
{
	int button ;
	int	screen_x, screen_y ;

	_Clear_base () ;
	Write_base(10, header) ;
	Write_base(12, "    Buttons:") ;
	Write_base(13, "       Left:   Previous line") ;
	Write_base(14, "       Middle: Quit") ;
	Write_base(15, "       Right:  Next line") ;

	R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;

	return(button) ;
}

