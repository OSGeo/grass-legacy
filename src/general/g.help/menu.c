#include "config.h"
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "menu.h"
#include "help_proto.h"

/* Switched functions of BLANK and SPACE 7/98 Markus Neteler
 *                              neteler@geog.uni-hannover.de
 * for a more intuitive use of g.help.
 */
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
 |		P_menuint() must be called before
 |	any other as these initialize the terminal and 'curses'
 |	facilities, repectively. Upon program termination
 |	P_menuexit() will restore the terminal to its original
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
 |		absence (i >= 0) or presence (i < 0) of an error.
 |		The error codes are defined in menu.h and are handled
 |		for the menu system through P_menuerror(). All
 |		practical errors which may happen during control
 |		by F_menu (file errors, memory allocations, etc.)
 |		are handled internal to F_menu.
 |		
 */

int F_menu (
char 	*Choicefile,char *Helpfile, int *Choice,
char *Respbuf, int *Flags,
int *curline, int *hilite )
{
char	*Men_strbuf;
int	Usrcmnd;
char	*F_universe() ;
char	buffer[128] ;
int	Menlength;
int	Menstat, Hlpstat, Tmpstat;
int Curline ;

	Tmpstat = 0;
	Menstat = F_fetchfile (Choicefile, &Men_strbuf, &Menlength);

	/*
	Prmptstr = Men_strbuf;
	if (Prmptstr == NULL)
		Prmptstr = "(Return to Continue> ";
	else
		{
		Index = 1;
		while (Index++ < Menlength)
			{
			while (*Prmptstr++ != LF)
				;
			}
		}
	*/
		
	Curline = *curline;

	/*
	strcpy(buffer, F_universe(" GRASS HELP.  Enter ? for instructions"));

	if (Menstat < 0)
		P_menuerror (Menstat, Choicefile);
	else
		P_writowin (PlanetW, buffer, 1, 1, 1);
	*/

	box (TablW, VERTCHAR, HORZCHAR);
	wrefresh (TablW);

	P_writo_Menu_win (MenuW, Men_strbuf, Curline, Menlength,
			MENWINHITE - 2, hilite, Respbuf);
	/*
	P_writowin (PrmptW, Prmptstr, 1, 1, 1);
	*/

	for(;;)
	{
		Usrcmnd = getch() ;
		switch(Usrcmnd)
		{
			case REFRESH:
				{
				wrefresh (curscr);
				break ;
				}

			case HELP:
				{
				Hlpstat = F_helpctrl (Helpfile);

				if (Menstat < 0)
					P_menuerror (Menstat, Choicefile);
				else
					P_writowin (PlanetW, buffer, 1, 1, 1);
				box (TablW, VERTCHAR, HORZCHAR);
				wrefresh (TablW);
				break ;
				}

			case PRINTOUT:
				{
				dump_window() ;
				break ;
				}

			case ARROWDOWN: /* line down */
				{
				if (Curline <
					((Menlength - (MENWINHITE - 3)))) 
						Curline++;
				break;
				}

			case PAGEDOWN: /* page down */
				{
				Curline += (MENWINHITE - 4);
				Curline =
					Curline <=
					(Menlength - (MENWINHITE - 3)) ?
						Curline :
					(Menlength - (MENWINHITE - 3));
				Curline = Curline < 1 ? 1 : Curline;
				break;
				}

			case ARROWUP: /* line up */
				{
				if (Curline > 1)
					Curline--;
				break;
				}

			case PAGEUP: /* page up */
				{
				Curline -= (MENWINHITE - 4);
				Curline =
					Curline < 1 ? 1 : Curline;
				break;
				}

			case 't':
				{
				Curline = 1;
				break;
				}

			case 'b':
				{
				Curline = Menlength - (MENWINHITE - 3); 
				break;
				}

			case CR:
				{
				(*hilite)++ ;
				break ;
				}

			case 'q':
				{
				*Respbuf = ESC ; *(Respbuf+1) = '\0' ;
				goto doreturn ;
				}

			case BLANK:
				{
				goto doreturn;
				}

			default:
				break;
		}

		P_writo_Menu_win (MenuW, Men_strbuf, Curline, Menlength,
				MENWINHITE - 2, hilite, Respbuf);
		/*
		P_writowin (PrmptW, Prmptstr, 1, 1, 1);
		*/
		*curline = Curline ;
	}

doreturn:
	if (Men_strbuf != NULL)
		G_free (Men_strbuf);

	return (Menstat);
}

/*--------------------------------------------------------------------
 |
 |	This routine restores the terminal to its original
 |	status after having been changed by P_menuinit().
 |
 */

void P_termexit(int dummy)
{
 	signal (SIGINT, P_termexit);  
 	signal (SIGQUIT, P_termexit);
	signal (SIGTERM, P_termexit);
	Old_tty();
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

int F_linecnt ( FILE	*Fileptr)
{
int	Maxlines;
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

int P_menuinit ()
{

	signal (SIGINT, SIG_IGN);
	signal (SIGQUIT, SIG_IGN);

	initscr ();
	if (LINES < 15)
	{
		endwin() ;
		fprintf(stderr,"Sorry, screen must be 15 lines or longer\n") ;
		exit(-1) ;
	}
	if (COLS < 80)
	{
		endwin() ;
		fprintf(stderr,"Sorry, screen must be 80 columns or wider\n") ;
		exit(-1) ;
	}
	Get_old_tty();

	crmode ();
	noecho ();

/* ctrlz must be set after curses initialization */
#ifdef SIGTSTP
	signal (SIGTSTP, SIG_IGN);
#endif

	Get_new_tty();

	erase ();
	refresh ();

	MENWINHITE = LINES - TABLWINHITE - 1 ;
	HELPWINHITE = LINES - CMDWINHITE + 1 ;
	PlanetW = newwin (3, COLS - 1, 0, 0);
	ErrorW = newwin (3, COLS - 1, 0, 0);
	MenuW = newwin (MENWINHITE, COLS - 1, 2, 0);
	StatehelpW = newwin (HELPWINHITE, COLS - 1, 2, 0);
	TablW = newwin (TABLWINHITE, COLS - 1, LINES - TABLWINHITE , 0);
	CommandhelpW = newwin (CMDWINHITE, COLS - 1, LINES - 3 - CMDWINHITE + 1, 0);
	PrmptW = newwin (3, COLS - 1, LINES - 3, 0);

	werase (PlanetW);
	werase (ErrorW);
	werase (TablW);
	werase (StatehelpW);
	werase (MenuW);
	werase (CommandhelpW);
	werase (PrmptW);
	
	return 0;
}

/*---------------------------------------------------------------------
 |		This routine is the inverse of the P_menuinit()
 |	routine and should be called by the main control program
 |	before its termination.
 |
 */

int P_menuexit (void)
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

	return 0;
}

/*---------------------------------------------------------------------
 |	This routine takes the first 23 characters of the string passed
 |	by the calling program and sprints them into a string
 |	followed by the day, date, and time and the present users'
 | 	login and terminal name. The string is approx. 70 characters
 |	in length.
 |
 */

char *F_universe (char	*Message)
{
long	Thismoment;
char	*Dumptr1, *Dumptr2;
static char	Dumstring[256];

	time (&Thismoment);
	Dumptr1 = ctime (&Thismoment);
	for (Dumptr2 = Dumptr1; *Dumptr2 ; Dumptr2++)
		if (*Dumptr2 == '\0')
		    *Dumptr2 = '\0';

	sprintf (Dumstring, "%-48s %-30s", Message, Dumptr1);

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

int F_helpctrl (char	*Helpfile)
{
char	*Hlp_strbuf, *Cmd_strbuf, *Strptr, Usrcmnd;
int	Curline, Hlplength, Cmdlength, Hlpstat, Cmdstat;

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

	while ((Usrcmnd = getch()) != LF)
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

int P_getusrinpt (char *Respbuf, char	*Prmptstr)
{
char	*Saveptr;
int	Charcntr;
int	curx, cury;

	Charcntr = 0;
	Saveptr = Respbuf;
	P_writowin (PrmptW, Prmptstr, 1, 1, 1);
	
	while ((*Respbuf = getch()) != CR && *Respbuf != LF)
		{
		if (*Respbuf == KILLINE)
			{
			Charcntr = 0;
			Respbuf = Saveptr;
			P_writowin (PrmptW, Prmptstr, 1, 1, 1);
			*Respbuf = '\0';
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
				*Respbuf = '\0';
				return (Charcntr);
				}
			}
		else if ((*Respbuf != BKSPC)
			 && (getyx(PrmptW, cury, curx), curx <= (COLS - 5)))
			{
			waddch (PrmptW, *Respbuf++);
			wrefresh (PrmptW);
			Charcntr++;
			}
		}

	*Respbuf = '\0';

	return (Charcntr);
}

/*---------------------------------------------------------------------
 |		This routine spawns a process to perform an execl()
 |	system call to the visual editor.
 */

int F_longresponse ()
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
			/*execl ("/usr/ucb/vi", "vi", TMPFILE);*/
			execl ("vi", "vi", TMPFILE);
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

int F_fetchfile (char	*Filename,char **In_buf, int *Length)
{
FILE		*Fdesc;
/* int		Linecntr, Index, Filength; */
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

int P_writowin (WINDOW	*Windoname, char *Bufptr,
int Firstline,int Buflength,int Numlines)
{
	int	Index, Maxlines;
	int std_out_on ;
	int	curx, cury;


	if (Bufptr == NULL)
	{
		box (Windoname, VERTCHAR, HORZCHAR);
		wrefresh (Windoname);
		return  0;
	}

	Firstline = (Firstline <= Buflength) ?
			Firstline : Buflength;

	Numlines = (Numlines <= (Buflength - (Firstline - 1))) ?
			Numlines : Buflength - (Firstline - 1);

	Maxlines = ((short)Numlines <= Windoname->CURSES_MAXY) ?
			Numlines : Windoname->CURSES_MAXY;

	Index = 1;
	while (Index++ < Firstline)
	{
		while ((*Bufptr != '\0') && (*Bufptr++ != LF))
			;
	}
	
	werase (Windoname);

	std_out_on = 0 ;
	Index = 0;
	while (Index++ < Maxlines)
	{
		wmove (Windoname, Index, LEFTMARG+1);
		while ((*Bufptr != LF) && (*Bufptr != '\0') &&
				(getyx(Windoname, cury, curx), curx < (COLS - 3)))
		{
			if(*Bufptr == '\134')
			{
				if (std_out_on)
					wstandend(Windoname) ;
				else
					wstandout(Windoname) ;

				std_out_on = ! std_out_on ;
				Bufptr++ ;
			}
			else
				waddch (Windoname, *Bufptr++);
		}
		if ((*Bufptr == LF) || (*Bufptr == '\0'))
			Bufptr++;
	}

	box (Windoname, VERTCHAR, HORZCHAR);

	wrefresh (Windoname);

	return 0;
}

int P_writo_Menu_win (
	WINDOW *Windoname, char *Bufptr,
	int Firstline,int Buflength,int Numlines,
	int *hilite, char *response)
{
	int	Index, Maxlines;
	int std_out_on ;
	int first_hilite ;
	int last_hilite ;
	int do_hilite ;
	int at_hilite ;
	char	*bufptr ;
	int	curx, cury;


	if (Bufptr == '\0')
	{
		box (Windoname, VERTCHAR, HORZCHAR);
		wrefresh (Windoname);
		return 0;
	}

	Firstline = (Firstline <= Buflength) ?
			Firstline : Buflength;

	Numlines = (Numlines <= (Buflength - (Firstline - 1))) ?
			Numlines : Buflength - (Firstline - 1);

	Maxlines = ((short)Numlines <= Windoname->CURSES_MAXY) ?
			Numlines : Windoname->CURSES_MAXY;

	if (hilite != NULL)
	{
		first_hilite = 0 ;
		last_hilite = 0 ;
		bufptr = Bufptr ;
		Index = 1;
		while (*bufptr != '\0')
		{
			if(*bufptr == LF)
				Index++ ;
			else if(*bufptr == '\134')
			{
				if(Index < Firstline)
					first_hilite++ ;
				if(Index < Firstline + Numlines)
					last_hilite++ ;
			}
			bufptr++ ;
		}
		first_hilite /= 2 ;
		first_hilite++ ;
		last_hilite /= 2 ;
		if(first_hilite > last_hilite) first_hilite = last_hilite ;
		if(*hilite < first_hilite) *hilite = first_hilite ;
		if(*hilite > last_hilite) *hilite = first_hilite ;
		do_hilite = *hilite - first_hilite + 1 ;
	}
	else
		do_hilite = 0 ;

	Index = 1;
	while (Index++ < Firstline)
		{
		while ((*Bufptr != '\0') && (*Bufptr++ != LF))
			;
		}
	
	werase (Windoname);

	at_hilite = 0 ;
	std_out_on = 0 ;
	Index = 0;
	while (Index++ < Maxlines)
	{
		char *ptr1, *ptr2 ;
		wmove (Windoname, Index, LEFTMARG+1);
		while ((*Bufptr != LF) && (*Bufptr != '\0') &&
				(getyx(Windoname, cury, curx), curx < (COLS - 3)))
		{
			if(*Bufptr == '\134')
			{
				if (std_out_on)
					wstandend(Windoname) ;
				else
				{
					if(++at_hilite == do_hilite)
					{
						wstandout(Windoname) ;
						waddch (Windoname, '*') ;
						ptr1 = Bufptr ;
						ptr2 = response ;
						while(*(++ptr1) != '\134')
							*ptr2++ = *ptr1 ;
						*ptr2 = '\0' ;
					}
							
					else
					{
						waddch (Windoname, ' ') ;
						wstandout(Windoname) ;
					}
				}

				std_out_on = ! std_out_on ;
				Bufptr++ ;
			}
			else
				waddch (Windoname, *Bufptr++);
		}
		if ((*Bufptr == LF) || (*Bufptr == '\0'))
			Bufptr++;
	}

	box (Windoname, VERTCHAR, HORZCHAR);

	if (MenuW == Windoname)
	{
	if(Firstline != 1)
	{
		wmove (Windoname, 1, LEFTMARG-1);
		waddch (Windoname, '^') ;
		wmove (Windoname, 2, LEFTMARG-1);
		waddch (Windoname, '|') ;
		wmove (Windoname, 3, LEFTMARG-1);
		waddch (Windoname, '|') ;
	}

	if(Firstline+Numlines < Buflength+1)
	{
		wmove (Windoname, Numlines, LEFTMARG-1);
		waddch (Windoname, 'v') ;
		wmove (Windoname, Numlines-1, LEFTMARG-1);
		waddch (Windoname, '|') ;
		wmove (Windoname, Numlines-2, LEFTMARG-1);
		waddch (Windoname, '|') ;
	}
	}

	wrefresh (Windoname);

	return 0;
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

int P_menuerror (
int	Screwupnum,
char	*Messg)
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

	if (*Messg != '\0')
		{
		Dumptr = Messg;
		while ((*Dumptr != '\0') &&
			   (*Dumptr != LF) &&
				((Dumptr - Messg) < 35))
					Dumptr++;
		*Dumptr = '\0';
		}

	sprintf (Dumstring, "%d %-35s%35s", Screwupnum,
					    Screwupmsg[Screwupnum],
					    Messg);
	P_writowin (ErrorW, Dumstring, 1, 1, 1);

	wrefresh (PrmptW);

	return 0;
}

#include <pwd.h>

int dump_window(void)
{
	int atrow, atcol ;
	FILE *file ;
	char home[80] ;
	int curx, cury ;

	sprintf(home,"%s/visual_ask", getpwuid(getuid())->pw_dir ) ;

	if ((file=fopen(home, "a")) == NULL)
		return(-1) ;

	getyx(curscr, cury, curx) ;

	fprintf(file,"--------------------------------------------------------\n") ;
	for (atrow=0; atrow<LINES; atrow++)
	{
		for (atcol=0; atcol<COLS-1; atcol++)
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

	return 0;
}
