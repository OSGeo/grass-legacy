/*  @(#)yes.c	2.1  6/26/87  */
yes(line, prompt, def)
	int line ;
	char *prompt ;
	int def ;
{
	char buffer[20] ;

	for(;;)
	{
		Write_message(line, prompt) ;
		Get_curses_text(buffer) ;
		switch (*buffer)
		{
			case 'n':
			case 'N':
				return (0) ;
				break ;
			case 'y':
			case 'Y':
				return (1) ;
				break ;
			case 000:
				return (def) ;
				break ;
			default:
				Write_message(line, prompt) ;
				break ;
		}
	}
}
