/* %W% %G% */
#include "menu.h"
/*=====================================================================
 |	This package of menu routines was developed according to the
 |	protocal designed and specified by Van Warren for the Fort Hood
 |	Information System. The implementation was catered and prepared
 |	by James Bozek. Thus, to a notoriously well-nourished system
 |	such as VAX/UNIX is brought yet another exquisit co-authored
 |	culinary delicacy.
 |
 |====================================================================
 |		Each of the routines herein has been prepared 
 |	and documented ala carte and can be called as such.
 | 	TablW is left totally untouched (except for refreshment) by
 |	the menu complex, thereby leaving its contents under control
 |	of the calling program. The utility routine P_writowin()
 |	serves this end.
 |
 |		P_terminit() and P_menuint() must be called before
 |	any other as these initialize the terminal and 'curses'
 |	facilities, repectively. Upon program termination, P_termexit()
 |	and P_menuexit() will restore the terminal to its original
 |	state.
 |	
 |	The protocal of the F_menu function is as follows:
 |		Choicefile - Input
 |			The name of the file containing the
 |			information to be displayed in MenuW.
 |			The file can be of any length (measured by
 |			number of lines) with the last line
 |			representing a prompt string to be
 |			displayed in PrmptW.
 |		Helpfile - Input
 |			The name of the file containing the
 |			information to be displayed in StatehelpW
 |			upon a HELP request with respect to
 |			information in the MenuW.
 |		Choice - Output
 |			Upon return contains the number of characters
 |			entered by the user during any type of 
 |			response except LONGRESPONSE, which implies
 |			a file edit response.
 |		Respbuf - Output
 |			A pointer to a character buffer with a length
 |			of at least a standard terminals width (approx.
 |			80 chars).
 |		Flags - Input
 |			The type of restriction placed upon the type
 |			of response desired from the user (ie.
 |			MICRORESP, MINIRESP, WORDRESP, LINERESP,
 |			LONGRESP, CHOOSERESP). See menu.h which
 |			contains all constants, data structures and
 |			global variables.
 |
 |		F_menu itself returns an integer which signals the
 |		absence (i = 0) or presence (i < 0) of an error.
 |		The error codes are defined in menu.h and are handled
 |		for the menu system through P_menuerror(). All
 |		practical errors which may happen during control
 |		by F_menu (file errors, memory allocations, etc.)
 |		are handled internal to F_menu.
 |		
 */

F_menu (Choicefile, Helpfile, Choice, Respbuf, Flags)
char 	*Choicefile, *Helpfile, *Respbuf; 
int 	*Choice, *Flags;
{
char	*malloc(), *Prmptstr, *Resptype;
char	*Men_strbuf, *Strptr, Usrcmnd;
char	*F_universe() ;
int	Index, Linecntr, Hlplength, Menlength;
int	Curline, Menstat, Hlpstat, Tmpstat;

	Tmpstat = 0;
	Menstat = F_fetchfile (Choicefile, &Men_strbuf, &Menlength);

	Prmptstr = Men_strbuf;
	if (Prmptstr == NULL)
		Prmptstr = "(Return to Continue> ";
	else
		{
		Index = 1;
		while (Index++ < Menlength)
			{
			while (*Prmptstr++ != RET)
				;
			}
		}
		
	if (Menlength > 0)
		Menlength--;
	Curline = 1;

/*
	if (*Flags == MICRORESP)
		sprintf (Resptype, MICROMESG);
	else if (*Flags == MINIRESP)
		sprintf (Resptype, MINIMESG);
	else if (*Flags == LINERESP)
		sprintf (Resptype, LINEMESG);
	else if (*Flags == LONGRESP)
		sprintf (Resptype, LONGMESG);
	else if (*Flags == CHOOSERESP)
		sprintf (Resptype, CHOOSEMESG);
	else
		{
		*Flags = DEFLTRESP;
		sprintf (Resptype, DEFLTMESG);
		}
 */
	if (*Flags == MICRORESP)
		Resptype = F_universe( MICROMESG);
	else if (*Flags == MINIRESP)
		Resptype = F_universe( MINIMESG);
	else if (*Flags == LINERESP)
		Resptype = F_universe( LINEMESG);
	else if (*Flags == LONGRESP)
		Resptype = F_universe( LONGMESG);
	else if (*Flags == CHOOSERESP)
		Resptype = F_universe( CHOOSEMESG);
	else
		{
		*Flags = DEFLTRESP;
		Resptype = F_universe( DEFLTMESG);
		}

	if (Menstat < 0)
		P_menuerror (Menstat, Choicefile);
	else
		P_writowin (PlanetW, Resptype, 1, 1, 1);
	box (TablW, VERTCHAR, HORZCHAR);
	wrefresh (TablW);
	P_writowin (MenuW, Men_strbuf, Curline, Menlength,
			MENWINHITE - 2);
	P_writowin (PrmptW, Prmptstr, 1, 1, 1);

	*Respbuf = NULL;
	while ((Usrcmnd = getchar()) != RET)
	{
	    if (isalpha (Usrcmnd) || isdigit (Usrcmnd))
		switch(*Flags) 
		{
			case MICRORESP:
				if (isdigit (Usrcmnd))
				{
					*Respbuf++ = Usrcmnd;
					*Respbuf = NULL;
					*Choice = 1;
					if (Men_strbuf != NULL)
						free (Men_strbuf);
					Menstat = (Menstat <= Tmpstat) ?
							Menstat : Tmpstat;
					return (Menstat);
				}
				break ;
	
			case MINIRESP:
				ungetc (Usrcmnd, stdin);
				*Choice = P_getusrinpt (Respbuf, Prmptstr);
				if (*Choice)
				{
					if (Men_strbuf != NULL)
						free (Men_strbuf);
					Menstat = (Menstat <= Tmpstat) ?
							Menstat : Tmpstat;
					return (Menstat);
				}
				break ;
	
			case CHOOSERESP:
				P_writowin (PrmptW, "vi(v) or oneline > ",
				    	1, 1, 1);
				Usrcmnd = getchar();

				if (Usrcmnd == 'v')
				{
					Tmpstat = F_longresponse ();

					if (Tmpstat < 0)
						P_menuerror (Tmpstat, TMPFILE);
					else	
						P_writowin (PlanetW,
					    	"Respond again or <ret>",
					     	1, 1, 1);

					box (TablW, VERTCHAR, HORZCHAR);
					wrefresh (TablW);

				}
				else
				{
					*Choice = P_getusrinpt (Respbuf, "(> ");
					P_writowin (PlanetW,
					    	"Respond again or <ret>",
					     	1, 1, 1);
					P_writowin (StatehelpW, Respbuf,
					    	1, 1, 1);
					P_writowin (PrmptW, Prmptstr, 1, 1, 1);
				}
				break ;
	
			case LINERESP:
				ungetc (Usrcmnd, stdin);
				*Choice = P_getusrinpt (Respbuf, "(> ");
				P_writowin (PlanetW,
				    	"Respond again or <ret>",
				     	1, 1, 1);
				P_writowin (StatehelpW, Respbuf,
				    	1, 1, 1);
				P_writowin (PrmptW, Prmptstr, 1, 1, 1);
				break ;

			case LONGRESP:
				Tmpstat = F_longresponse ();

				if (Tmpstat < 0)
					P_menuerror (Tmpstat, TMPFILE);
				else	
					P_writowin (PlanetW,
					    	"Respond again or <ret>",
					     	1, 1, 1);

				box (TablW, VERTCHAR, HORZCHAR);
				wrefresh (TablW);

				break;
		}

		else if(Usrcmnd == REFRESH)
			wrefresh (curscr);

		else if(Usrcmnd == HELP)
		{
			Hlpstat = F_helpctrl (Helpfile);

			if (Menstat < 0)
				P_menuerror (Menstat, Choicefile);
			else
			{
				Resptype = F_universe(Resptype) ;
				P_writowin (PlanetW, Resptype, 1, 1, 1);
			}

			box (TablW, VERTCHAR, HORZCHAR);
			wrefresh (TablW);
		}

		else if(Usrcmnd == PRINTOUT)
			dump_window() ;

		else if (Menlength > (MENWINHITE - 2))
		{
		switch (Usrcmnd)
			{
			case FORLIN:
				{
				if (Curline <
					((Menlength - (MENWINHITE - 3)))) 
						Curline++;
				break;
				}

			case FORPAG:
				{
				Curline += (MENWINHITE - 3);
				Curline =
					Curline <=
					(Menlength - (MENWINHITE - 3)) ?
						Curline :
					(Menlength - (MENWINHITE - 3));
				break;
				}

			case BAKLIN:
				{
				if (Curline > 1)
					Curline--;
				break;
				}

			case BAKPAG:
				{
				Curline -= (MENWINHITE - 3);
				Curline =
					Curline < 1 ? 1 : Curline;
				break;
				}

			case TOPPAG:
				{
				Curline = 1;
				break;
				}

			case BOTPAG:
				{
				Curline = Menlength - (MENWINHITE - 3); 
				break;
				}

			default:
				break;
			}

		}
		P_writowin (MenuW, Men_strbuf, Curline,
				Menlength, MENWINHITE - 2);
		P_writowin (PrmptW, Prmptstr, 1, 1, 1);
		}

	if (Men_strbuf != NULL)
		free (Men_strbuf);

	Menstat = (Menstat <= Hlpstat) ? Menstat : Hlpstat;
	Menstat = (Menstat <= Tmpstat) ? Menstat : Tmpstat;

	return (Menstat);

}

/*--------------------------------------------------------------------
 |
 |	This routine restores the terminal to its original
 |	status after having been changed by P_terminit().
 |
 */

P_termexit()
{
 	signal (SIGINT, P_termexit);  
 	signal (SIGQUIT, P_termexit);
	signal (SIGTERM, P_termexit);
#ifdef COMMENTED_OUT
	stty (0, &slclstty);
#endif
	Old_tty();

	/*
	chmod (Mytermname, Ttymodstat.st_mode | 022);
	*/

	return;
}

/*---------------------------------------------------------------------
 |
 |	This routine initializes the terminal for
 |   interactivity, and saves the default status for
 |   restoration upon end of session.
 |
 */

P_terminit()	
{
/*
char	*ttyname ();
*/

	Get_tty();
#ifdef COMMENTED_OUT
	gtty (0, &slclstty);
	gtty (0, &lclstty);
	lclstty.sg_flags |= CBREAK;	/* No <ret> needed */
	lclstty.sg_flags &= ~(ECHO);	/* No echo please */
#endif

 	signal (SIGINT, SIG_IGN);
 	signal (SIGQUIT, SIG_IGN);

	signal (SIGTERM, P_termexit);
#ifdef COMMENTED_OUT
	stty (0, &lclstty);
#endif
	New_tty();

	/*
	Mytermname = ttyname (2);
	if (Mytermname == NULL)
		return;
	if (stat (Mytermname, &Ttymodstat) < 0)
		return;

	chmod (Mytermname, Ttymodstat.st_mode & ~022);
	*/

	return;

}

/*---------------------------------------------------------------------
 |	This routine counts the lines in the file whose stdio.h
 |	"FILE" file pointer is passed by the calling routine, and
 |	returns the number in the form of an integer. (The file must
 |	be fopened previous to the calling of this routine.) Memory
 |	is used dynamically and the file is repositioned to the
 |	beginning before returning.
 |
 */

F_linecnt (Fileptr)
FILE	*Fileptr;
{
int	Maxlines;
char	*malloc(), *fgets();
char	*Dumptr ,*Bufptr;

	Maxlines = 0;
	if ((Bufptr = malloc(80)) == NULL)	/* Get buffer */
		return (Maxlines);

	while ((Dumptr = fgets (Bufptr, 80, Fileptr)) != NULL)
		Maxlines++;			/* Count the lines */
	free (Bufptr);				/* Emancipation */
	rewind (Fileptr);			/* Reposition */

	return (Maxlines);
}

/*---------------------------------------------------------------------
 |		This routine initializes the 'curses' data structures
 |	and screens. It must be called by the main control program
 |	before using any of the menu package routines.
 |
 */

P_menuinit ()
{

	Get_old_tty();
	initscr ();
	crmode ();
	noecho ();
	Get_new_tty();

	erase ();
	refresh ();

	PlanetW = newwin (3, NCOLS - 1, 0, 0);
	ErrorW = newwin (3, NCOLS - 1, 0, 0);
	TablW = newwin (HELPWINHITE, NCOLS - 1, 2, 0);
	StatehelpW = newwin (HELPWINHITE, NCOLS - 1, 2, 0);
	MenuW = newwin (MENWINHITE, NCOLS - 1, 15, 0);
	CommandhelpW = newwin (CMDWINHITE, NCOLS - 1, 15, 0);
	PrmptW = newwin (3, NCOLS - 1, 21, 0);

	wclear (PlanetW);
	wclear (ErrorW);
	wclear (TablW);
	wclear (StatehelpW);
	wclear (MenuW);
	wclear (CommandhelpW);
	wclear (PrmptW);
	
	return;
}

/*---------------------------------------------------------------------
 |		This routine is the inverse of the P_menuinit()
 |	routine and should be called by the main control program
 |	before its termination.
 |
 */

P_menuexit ()
{

	delwin (PlanetW);
	delwin (TablW);
	delwin (StatehelpW);
	delwin (MenuW);
	delwin (CommandhelpW);
	delwin (PrmptW);

	clear ();
	refresh ();
	mvcur (0, COLS - 1, LINES - 1, 0);
	endwin ();

	return;
}

/*---------------------------------------------------------------------
 |	This routine takes the first 23 characters of the string passed
 |	by the calling program and sprints them into a string
 |	followed by the day, date, and time and the present users'
 | 	login and terminal name. The string is approx. 70 characters
 |	in length.
 |
 */

char *
F_universe (Message)
char	*Message;
{
long	time(), Thismoment;
char	*ctime();
char	*Dumptr1, *Dumptr2;
static char	Dumstring[256];

	time (&Thismoment);
	Dumptr1 = ctime (&Thismoment);
	for (Dumptr2 = Dumptr1; *Dumptr2 ; Dumptr2++)
		if (*Dumptr2 == NULL)
		    *Dumptr2 = NULL;

	sprintf (Dumstring, "%-23s %-30s", Message, Dumptr1);

	return (Dumstring);
}

/*---------------------------------------------------------------------
 |	This routine takes control upon a request for help from the
 |	user. It opens the file containing help information for the
 |	present state of the Fort Hood System, allocates space for the
 |	text, gives the user what is desired for as long as it is
 |	desired (within legal G_limits), and returns to the calling
 |	control loop. The address of a character string containing
 |	the name of the file is passed by the calling
 |	program.
 |		In addition to the helpfile name, a file whose name
 |	is #defined 'MENCOMMANDS' must be included to expect error
 |	free returns. This file is a formatted ascii text file (an
 |	example of which is found in 'Mencommands') and shows the
 |	user the one key commands available to manipulate the menu
 |	windows. These commands are #defines in the file menu.h.
 |	See that file for further information.
 |
 */

F_helpctrl (Helpfile)
char	*Helpfile;
{
char	*Hlp_strbuf, *Cmd_strbuf, *Strptr, Usrcmnd;
char	*malloc(), *F_universe();
int	Curline, Hlplength, Cmdlength, Linecntr, Hlpstat, Cmdstat;

	Hlpstat = F_fetchfile (Helpfile, &Hlp_strbuf, &Hlplength);
	Cmdstat = F_fetchfile (COMMANDFILE, &Cmd_strbuf, &Cmdlength);
	Hlpstat = (Hlpstat <= Cmdstat) ? Hlpstat : Cmdstat;

	Curline = 1;

	if (Hlpstat < 0)
		P_menuerror (Hlpstat, Helpfile);
	else
		{
		Strptr = F_universe (Helpfile);
		P_writowin (PlanetW, Strptr, 1, 1, 1);
		}

	P_writowin (StatehelpW, Hlp_strbuf, Curline,
		    Hlplength, HELPWINHITE - 2);
	P_writowin (CommandhelpW, Cmd_strbuf, Curline,
		      Cmdlength, CMDWINHITE - 2);
	P_writowin (PrmptW, "(Return to Continue> ", 1, 1, 1);

	while ((Usrcmnd = getchar()) != RET)
		{
		if (Usrcmnd == REFRESH)
			wrefresh(curscr) ;
		else if(Usrcmnd == PRINTOUT)
			dump_window() ;
/*
			{
			P_writowin (PlanetW, NULLSTR, 1, 1, 1);
			P_writowin (StatehelpW, NULLSTR, 1, 1, 1);
			P_writowin (CommandhelpW, NULLSTR, 1, 1, 1);
			P_writowin (PrmptW, NULLSTR, 1, 1, 1);

			if (Hlpstat < 0)
				P_menuerror (Hlpstat, Helpfile);
			else
				{
				Strptr = F_universe (Helpfile);
				P_writowin (PlanetW, Strptr, 1, 1, 1);
				}
			P_writowin (StatehelpW, Hlp_strbuf, Curline,
				    Hlplength, HELPWINHITE - 2);
			P_writowin (CommandhelpW, Cmd_strbuf, Curline,
				      Cmdlength, CMDWINHITE - 2);
			P_writowin (PrmptW, "(Return to Continue> ",
						 1, 1, 1);
			}
*/

		else if (Hlplength > (HELPWINHITE - 3))
		{
		switch (Usrcmnd)
			{
			case FORLIN:
				{
				if (Curline <
					((Hlplength - (HELPWINHITE - 3)))) 
						Curline++;
				break;
				}

			case FORPAG:
				{
				Curline += (HELPWINHITE - 3);
				Curline =
					Curline <=
					(Hlplength - (HELPWINHITE - 3)) ?
						Curline :
					(Hlplength - (HELPWINHITE - 3));
				break;
				}

			case BAKLIN:
				{
				if (Curline > 1)
					Curline--;
				break;
				}

			case BAKPAG:
				{
				Curline -= (HELPWINHITE - 3);
				Curline =
					Curline < 1 ? 1 : Curline;
				break;
				}

			case TOPPAG:
				{
				Curline = 1;
				break;
				}

			case BOTPAG:
				{
				Curline = Hlplength - (HELPWINHITE - 3); 
				break;
				}

			default:
				break;
			}

		P_writowin (StatehelpW, Hlp_strbuf, Curline,
				Hlplength, HELPWINHITE - 2);
		P_writowin (PrmptW, "(Return to Continue> ", 1, 1, 1);
		}
	}

	if (Hlp_strbuf != NULL)
		free (Hlp_strbuf);
	if (Cmd_strbuf != NULL)
		free (Cmd_strbuf);

	return (Hlpstat);
}

/*---------------------------------------------------------------------
 |	This routine accepts a user response string into the array
 |	whose pointer is passed by the calling program. The routine
 |	will return no more than about 70 characters depeding upon
 |	the width of a screen so that the menu display will not
 |	be jazzed up. The characters are echoed to the screen. Back-
 |	space and a line restart (KILLINE) facility are available.
 |	See menu.h.
 |
 */

P_getusrinpt (Respbuf, Prmptstr)
char	*Respbuf;
char	*Prmptstr;
{
char	*Saveptr;
int	Charcntr;

	Charcntr = 0;
	Saveptr = Respbuf;
	P_writowin (PrmptW, Prmptstr, 1, 1, 1);
	
	while ((*Respbuf = getchar()) != RET)
		{
		if (*Respbuf == KILLINE)
			{
			Charcntr = 0;
			Respbuf = Saveptr;
			P_writowin (PrmptW, Prmptstr, 1, 1, 1);
			*Respbuf = NULL;
			return (Charcntr);
			}
		else if ((*Respbuf == BKSPC) && (Charcntr > 0))
			{
			waddch (PrmptW, *Respbuf);
			wrefresh (PrmptW);
			waddch (PrmptW, BLANK);
			wrefresh (PrmptW);
			waddch (PrmptW, *Respbuf);
			wrefresh (PrmptW);
			Respbuf--;
			if (--Charcntr == 0)
				{
				*Respbuf = NULL;
				return (Charcntr);
				}
			}
		else if ((*Respbuf != BKSPC)
			 && (PrmptW->_curx <= (NCOLS - 5)))
			{
			waddch (PrmptW, *Respbuf++);
			wrefresh (PrmptW);
			Charcntr++;
			}
		}

	*Respbuf = NULL;

	return (Charcntr);
}

/*---------------------------------------------------------------------
 |		This routine spawns a process to perform an execl()
 |	system call to the visual editor.
 */

F_longresponse ()
{
int	Procid, Status, Returnval;

	switch (Procid = fork())
		{
		case -1:
			{
			return (~FRKERR_ERRNUM);
			break;
			}
		case 0: 
			{
			execl ("/usr/ucb/vi", "vi", TMPFILE);
			break;
			}
		default:
			{
			while ((Returnval = wait (&Status))
				!= Procid && Returnval != -1)
					;
			clear ();
			erase ();
			refresh ();
						
			break;
			}
		}
	return (NOERR_ERRNUM);
}

/*---------------------------------------------------------------------
 |		This routine opens the file whose name is pointed to
 |	by Filename (how novel!), allocates memory enough to hold its
 |	entirety and places a pointer to that buffer in In_buf, and
 |	returns its newline terminated ('\n') line length in the
 |	variable which is pointed to by Length.
 |		If errors occur during any of these phases, *Length
 |	becomes 0, *In_buf becomes NULL, and F_fetchfile() returns
 |	an integer less than zero cooresponding to the negative value
 |	of the appropriate error code found in menu.h. Otherwise,
 |	0 (NOERR_ERRNUM) is returned.
 |
 */

F_fetchfile (Filename, In_buf, Length)
char		*Filename, **In_buf;
int		*Length;
{
FILE		*Fdesc;
char		*malloc();
int		Linecntr, Index, Filength;
struct stat	Filestatus;

	if ((stat (Filename, &Filestatus)) < 0)
		{
		*In_buf = NULL;
		*Length = 0;
		return (~FILSTA_ERRNUM);
		}

	if ((Fdesc = fopen (Filename, "r")) == NULL)
		{
		*In_buf = NULL;
		*Length = 0;
		return (~FILOPN_ERRNUM);
		}

	if ((*In_buf = malloc (Filestatus.st_size)) == NULL)
		{
		fclose (Fdesc);
		*Length = 0;
		return (~NOMEM_ERRNUM);
		}

	if ((*Length = F_linecnt (Fdesc)) == 0)
		{
		free (*In_buf);
		fclose (Fdesc);
		*In_buf = NULL;
		return (~FILRED_ERRNUM);
		}

	if ((fread (*In_buf, sizeof (char),
		 Filestatus.st_size, Fdesc)) < Filestatus.st_size)
		{
		free (*In_buf);
		fclose (Fdesc);
		*In_buf = NULL;
		*Length = 0;
		return (~FILRED_ERRNUM);
		}

	fclose (Fdesc);

	return (NOERR_ERRNUM);
}

/*---------------------------------------------------------------------
 |	This is a utility routine which enables the programmer to write
 |	a number of lines of text to the window whose pointer is passed
 |	as Windoname. The text is written in the window beginning at the
 |	upper left hand corner and will go to the next window line upon
 |	reaching a newline character, a NULL character, or if the 
 |	length of the line of text is longer 
 |	than the width of the window minus a righthand margin width,
 |	until the bottom line of the window is reached. It is therfore
 |	indestructible to the display
 |	in that it simply refuses to venture outside the boundries of
 |	its window. If the passed Bufptr is NULL, a box is drawn
 |	around the window perimeter and the routine returns. 
 |	Segmentation faults can occur if an inaccurate buffer length 
 |	(Buflength) is passed. (See F_linecnt). If all
 |	is well, Numline lines are printed, beginning with the line in
 |	the buffer whose number is Firstline.
 |		Example:
 |			To print one line to the PrmptW;
 |	P_writowin (PrmptW, Charstringptr, 1, 1, 1);
 |
 */

P_writowin (Windoname, Bufptr, Firstline, Buflength, Numlines)
WINDOW		*Windoname;
char		*Bufptr;
int		Firstline, Buflength, Numlines;
{
int	Index, Maxlines;

	if (Bufptr == NULL)
		{
		box (Windoname, VERTCHAR, HORZCHAR);
		wrefresh (Windoname);
		return;
		}

	Firstline = (Firstline <= Buflength) ?
			Firstline : Buflength;

	Numlines = (Numlines <= (Buflength - (Firstline - 1))) ?
			Numlines : Buflength - (Firstline - 1);

	Maxlines = ((short)Numlines <= Windoname->_maxy) ?
			Numlines : Windoname->_maxy;

	Index = 1;
	while (Index++ < Firstline)
		{
		while ((*Bufptr != NULL) && (*Bufptr++ != RET))
			;
		}
	
	wclear (Windoname);

	Index = 0;
	while (Index++ < Maxlines)
		{
		wmove (Windoname, Index, LEFTMARG);
		while ((*Bufptr != RET) && (*Bufptr != NULL) &&
				(Windoname->_curx < (NCOLS - 3)))
			waddch (Windoname, *Bufptr++);
		if ((*Bufptr == RET) || (*Bufptr == NULL))
			Bufptr++;
		}

	box (Windoname, VERTCHAR, HORZCHAR);
	wrefresh (Windoname);

	return;
}

/*---------------------------------------------------------------------
 |		This routine prints the string whose pointer and the
 |	error whose number is passed by the calling routine. The error
 |	window can be cleared by passing NOERR (0) and a NULL string.
 |	If a programmer message only is desired, NOERR and the string
 |	may be passed. The string length will be truncated at 35
 |	characters.
 |
 */

P_menuerror (Screwupnum, Messg)
int	Screwupnum;
char	*Messg;
{
char	*Dumptr, Dumstring[256];
static char	*Screwupmsg[] = {
				"",
				"File not found",
				"File open error",
				"File read error",
				"File close error",
				"No memory error",
				"Can't fork to edit",
				"Invalid command entry",
				"Unknown error - invalid error code"
				};

	if (Screwupnum < 0)
		Screwupnum = ~Screwupnum;
	if (Screwupnum > 7)
		Screwupnum = 8;

	if (*Messg != NULL)
		{
		Dumptr = Messg;
		while ((*Dumptr != NULL) &&
			   (*Dumptr != RET) &&
				((Dumptr - Messg) < 35))
					Dumptr++;
		*Dumptr = NULL;
		}

	sprintf (Dumstring, "%d %-35s%35s", Screwupnum,
					    Screwupmsg[Screwupnum],
					    Messg);
	P_writowin (ErrorW, Dumstring, 1, 1, 1);

	wrefresh (PrmptW);

	return;
}

#include <pwd.h>

dump_window()
{
	int atrow, atcol ;
	FILE *fopen(), *file ;
	struct passwd *getpwuid() ;
	char home[80] ;
	int curx, cury ;

	sprintf(home,"%s/visual_ask", getpwuid(getuid())->pw_dir ) ;

	if ((file=fopen(home, "a")) == NULL)
		return(-1) ;

	getyx(curscr, cury, curx) ;

	fprintf(file,"--------------------------------------------------------\n") ;
	for (atrow=0; atrow<LINES; atrow++)
	{
		for (atcol=0; atcol<NCOLS-1; atcol++)
		{
			wmove(curscr, atrow, atcol) ;
			fprintf(file,"%c",winch(curscr)) ;
		}
		fprintf(file,"\n") ;
	}
	fprintf(file,"--------------------------------------------------------\n") ;
	fprintf(file,"\n\n") ;
	fclose(file) ;

	wmove(curscr, cury, curx) ;
}
