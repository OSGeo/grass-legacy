/*  @(#)quit.c	2.1  6/26/87  */
quit()
{
	if ('y' == curses_yes_no(1, "Shall we continue?  ", "") )
		return(1) ;
	return(0) ;
}
