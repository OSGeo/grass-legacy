/*
 * GEOGRAPHICS SERIAL DIGITIZER DIAGNOSTIC PROGRAM 
 *
 * Version C1.00    Author - D.V. McNEELY 
 *
 * This program demonstrates how to access the digitizer interface cards I/O
 * ports.  the program itself is useful to diagnose whether a problem exists
 * with the digitizer or with an application program written for the
 * digitizer. 
 *
 */

/********************************* Start **********************************/



#include <curses.h>
#include <signal.h>
#include "digit.h"

float   Xcoord;
float   Ycoord;
int     Xraw;
int     Yraw;
int     KpdChar;
int     KpdStat;
int     FtswStat;
int 	cur_screen_line = 0 ;
short   X;
short   Y;

int     Mycon;

float   Scale;
int     Actual;
int     Error;

unsigned  ser_num;	  /* the serial number is a sixteen bit number  */

int     Ft_Dly;       /* delays to keep hit Status on screen        */
                      /* long enough to see it                      */
int     Kpd_Dly;
int     Clr_On_Ftsw; /* if true, we clear counters when footswitch is hit */
int     Ftsw_Bp;     /* if true, we beep when the foot switch is hit */
int     Kpd_Bp;      /* if true, we beep when the key pad is hit */
int     Done;        /* Shows when we want to get out of this program */
int     KbdChar;     /* char input from keyboard, not keypad */
int     KeyHit;

char    Xdir_Str[20];  /* Strings for displaying which direction */
char    Ydir_Str[20];  /* causes the coordinates to increment */

char    Edge_Str[20];  /* String for displaying where it's mounted  */
char    Cpi_Str[20];   /* String for displaying count rate       */
char    Scale_Str[20]; /* String for displaying scaling factor   */
char    Ftsw_Str[20];  /* String for displaying footswitch Status */
char    Kpd_Str[20];   /* String for displaying key pad Status   */

int     Xdir;
int     Ydir;
int		Cpi ;
int     Edge;		    /* side of table that digitizer is mounted on */

/********************************* Main **************************************/
main (argc, argv)
int     argc;
char   *argv[];
{
	int     n;
	char   *dev_ptr;
	int     undoit ();
	int     open_failed ();
	char   *rindex ();
	char   *ptr;
	char    buf[BUFSIZ];

	if (argc != 2)
	{
		printf ("You must supply the tty device name\n");
		exit (1);
	}
/* 
 * point to the first argument 
 */
	dev_ptr = argv[1];
/* 
 * default to the /dev area.  This can be gotten around by using a
 * ./tty01 
 */
	if ((ptr = rindex (dev_ptr, '/')) == NULL)
	{
		sprintf (buf, "/dev/%s", dev_ptr);
		dev_ptr = buf;
	}

fprintf( stderr, "\n Opening %s\n", dev_ptr) ;

/*
 * Initialize the digitizer
 */
	signal(SIGALRM, open_failed) ;
	alarm(5) ;

	if (D_open_serial(dev_ptr) == -1)
	{
		fprintf( stderr, " Error opening digitizer\n") ;
		exit(-1) ;
	}

	signal(SIGALRM, SIG_DFL) ;

	Cpi  = CPI_800 ;
	Xdir = HOR_RT ;
	Ydir = VRT_UP ;
	Edge = TOP_MOUNT ;
	fprintf( stderr, " Initializing digitizer\n") ;

	D_digit_init(Cpi, Xdir, Ydir, Edge) ;
	fprintf( stderr, " Setting scale\n") ;
	D_get_scale(&Scale) ;
	if (Scale == 0.0)
	{
		printf("Error in digitizer initialization\n") ;
		exit(-1) ;
	}
/*
 * Initialize variables
 */
	fprintf( stderr, " Initilizing variables\n") ;
	init_var () ;

/*
 * Initializing curses package
 */
	signal (SIGQUIT, undoit);
	signal (SIGHUP, undoit);
	signal (SIGINT, undoit);

	fprintf( stderr, " Initilizing curses\n") ;
	init_curses() ;

	Done = 0;
	do			/* loop */
	{
		prnt_info ();
		prnt_menu ();
		digitize ();	/* Display coordinates until a key is pressed  */
		if (Done != 1)
			get_option ();
				/* a key was pressed so find out what * we're to */

	} while (Done != 1);

	 undoit();

}				/* ** end of main ** */

/***************************** init_var *********************************/
init_var ()
{
	int     n;

	n = strncpy (Cpi_Str, "800\0", 4);

/* scaling factor for converting counts to inches and displaying it */
	n = strncpy (Scale_Str, "0.00125\0", 8);

/* 
 * the initial configuration assumes the digitizer is mounted at the
 * top 
 */
	n = strncpy (Xdir_Str, "Right\0", 6);
	n = strncpy (Ydir_Str, "Up\0", 3);
	n = strncpy (Edge_Str, "Top\0", 4);
	n = strncpy (Kpd_Str, "Up  \0", 5);
	n = strncpy (Ftsw_Str, "Up  \0", 5);
	FtswStat = 0;
	Ft_Dly = 0;
	KpdStat = 0;
	Kpd_Dly = 0;
	KpdChar = 0x20;
	Clr_On_Ftsw = 1;	/* Do we want to reset the X & Y counters when   */
 						/* we find the foot switch has been pressed?     */
	Ftsw_Bp = 0;		/* Do we want to give a beep when the foot switch */
 						/* is hit?      */
	Kpd_Bp = 0;		/* Do we want to beep when the key pad is * hit?   */
	Done = 0;		/* we aren't Done, were just begining    */
}
/***************************** prnt_header *********************************/
prnt_header ()
{
	clear ();
	move (2, 18);
	cur_screen_line = 2 ;
	printw ("Geographics Digitizer Diagnostics Version C1.00S");
}
/****************************** digitize **********************************/
digitize ()
{
	int     n;
	int     nfds,
	        readfd,
	        writefd,
	        execptfds;

	cur_screen_line++ ;
	dwn_scr () ;
	printw ("| FootSwitch | KeyPad |      Key Value     |          Coordinates           |");
	dwn_scr () ;
	printw ("|   Status   | Status |   ASCII   Decimal  |        X              y        |");
	dwn_scr () ;
	printw ("|---------------------------------------------------------------------------|");
	dwn_scr () ;
	refresh() ;

	KeyHit = 0;

	while (KeyHit == 0)
	{

		if (ioctl (0, FIONREAD, &KeyHit) < 0)
		{
			dwn_scr () ;
			printw ("ioctl failed");
			refresh() ;
			exit (1);
		}

	/* get string of all info from * digitizer */
		Error = D_readall (&Xraw, &Yraw, &FtswStat, &KpdStat, &KpdChar) ;
		if (Error < 0)
		{
			dwn_scr () ;
			printw ("Cannot read digitizer");
			refresh() ;
			delay(3000) ;
			exit (1);
		}


		if (FtswStat == 1)
		{
			Ft_Dly = 10;
			n = strncpy (Ftsw_Str, "Down\0", 5);
			if (Clr_On_Ftsw == 1)
				/* do we want to reset the * counters?  */
				D_set_origin ();
				/* if so, then reset'em to * zero       */
			if (Ftsw_Bp == 1)
				/* do we want a beep if it's * been hit? 
				*/
				printf ("%c", 0x7);
		}
		if (Ft_Dly > 0)
		{
			if (--Ft_Dly < 1)
				n = strncpy (Ftsw_Str, "Up  \0", 5);
		}
		if (KpdStat == 1)
		{
			Kpd_Dly = 10;
			n = strncpy (Kpd_Str, "Down\0", 5);
			if (Kpd_Bp == 1)
				/* do we want a beep if it's * been hit? 
				printf ("\007");
				*/
			if (KpdChar == 0x0A)
				KpdChar = 'C';
			
			D_writekpd(KpdChar) ;
		}
		if (Kpd_Dly > 0)
		{
			if (--Kpd_Dly == 0)
				n = strncpy (Kpd_Str, "Up  \0", 5);
		}
		X = (short) Xraw;
		Y = (short) Yraw;
		Xcoord = (float) (Scale * X);
		Ycoord = (float) (Scale * Y);

		move(cur_screen_line,0) ;
		printw("    %s        %s       %c       %x            %3.5f         %3.5f            ",
				Ftsw_Str, Kpd_Str, KpdChar, KpdChar, Xcoord, Ycoord);
		refresh() ;

	}			/* * end of while * */

}				/* ** end of digitize ** */
/****************************** prnt_info **********************************/
prnt_info ()
{
	prnt_header ();
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	printw("The X Coordinate increases when the cursor is moved: %s", Xdir_Str);
	dwn_scr() ;
	printw("The Y Coordinate increases when the cursor is moved: %s", Ydir_Str);
	dwn_scr() ;
	printw("The Digitizer is mounted on the %s edge of the table", Edge_Str);
	dwn_scr() ;
	printw("The Serial Number of the Interface Card is: %d", ser_num);
	dwn_scr() ;
	printw("The Resolution of the Digitizer is 1/%s inch", Cpi_Str);
	dwn_scr() ;
	printw("The Scaling Factor for inches is: %s", Scale_Str);
	refresh() ;

}				/* ** end of prnt_info ** */
/**************************** prnt_menu ************************************/
prnt_menu ()
{

	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	printw("Options:");
	dwn_scr() ;
	printw("        0: Exit From Program                3: Set X and Y Dir");
	dwn_scr() ;
	printw("        1: Set Origin                       4: Set Base Address");
	dwn_scr() ;
	printw("        2: Set Resolution                   5: R/W Config File");
	dwn_scr() ;
	refresh() ;

}				/* ** end of prnt_menu ** */
/************************** set_resolution *********************************/
set_resolution ()
{
	char    answer;
	int     n, isok;

	isok = 0;
	prnt_header ();

	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	printw("The digitizer has four possible resolutions: ");
	dwn_scr() ;
	printw("");
	dwn_scr() ;
	printw("         1: 0.01    =  1/100 of an inch  ");
	dwn_scr() ;
	printw("         2: 0.005   =  1/200 of an inch  ");
	dwn_scr() ;
	printw("         3: 0.0025  =  1/400 of an inch  ");
	dwn_scr() ;
	printw("         4: 0.00125 =  1/800 of an inch  ");
	dwn_scr() ;
	printw("");
	dwn_scr() ;
	printw("Enter your choice 1-4: ");
	refresh() ;

	do
	{
		answer = getchar ();

		isok = 1;
		switch(answer & 0x7f)
		{
			case 0x0D: 
				return;
				/* No change wanted so exit proc. */

			case '1': 
				n = strncpy (Cpi_Str, "100\0", 4);
				n = strncpy (Scale_Str, "0.01\0", 5);
				break;

			case '2': 
				n = strncpy (Cpi_Str, "200\0", 4);
				n = strncpy (Scale_Str, "0.005\0", 5);
				break;

			case '3': 
				n = strncpy (Cpi_Str, "400\0", 4);
				n = strncpy (Scale_Str, "0.0025\0", 6);
				break;

			case '4': 
				n = strncpy (Cpi_Str, "800\0", 4);
				n = strncpy (Scale_Str, "0.00125\0", 8);
				break;

			default: 
				era_line ();
				dwn_scr() ;
				printw("That was an invalid response, please try again.");
				refresh() ;
				delay (2000);
				era_line ();
				isok = 0;

		}		/* end of switch */
	} while (isok == 0);

	D_set_scale(answer & 0x7f - '0') ;
	D_get_scale(&Scale) ;

}				/* ** end of set_resolution ** */
/******************************** set_xy  **********************************/
set_xy ()
{
/* **************   X   ************* */
	char    answer;
	int     n,
	        isok;

	prnt_header ();

	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	printw("                 The X coordinate can be set to increase when");
	dwn_scr() ;
	printw("                      the cursor is moved left or when it");
	dwn_scr() ;
	printw("                               is moved right.");
	dwn_scr() ;
	printw("                  Do you want X to increase when the cursor");

	do
	{
		dwn_scr() ;
		printw("                         is moved right? (y/n) [y] ");
		refresh() ;
		answer = getchar ();

		isok = 1;

		switch (answer & 0x7f)
		{
			case 'y': 
			case 'Y': 
				Xdir = HOR_RT;
				n = strncpy (Xdir_Str, "Right\0", 6);
				break;
			case 'n': 
			case 'N': 
				Xdir = HOR_LF;
				n = strncpy (Xdir_Str, "Left\0", 5);
				break;
			default: 
				era_line ();
				dwn_scr() ;
				printw("                That was an invalid response,");
				dwn_scr() ;
				printw(" please try again.");
				refresh() ;
				delay (2000);
				era_line ();
				isok = 0;
				break;
		}
	} while (isok == 0);

/* **************   y   ************* */

	prnt_header ();

	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	printw("                 The y coordinate can be set to increase when");
	dwn_scr() ;
	printw("                      the cursor is moved down or when it");
	dwn_scr() ;
	printw("                                 is moved up.");
	dwn_scr() ;
	printw("                  Do you want y to increase when the cursor");

	do
	{
		dwn_scr() ;
		printw("                         is moved up? (y/n) [y] ");
		refresh() ;
		answer = getchar ();

		isok = 1;

		switch (answer & 0x7f)
		{
			case '\n': 
			case 'y': 
			case 'Y': 
				Ydir = VRT_UP;
				n = strncpy (Ydir_Str, "Up\0", 3);
				break;

			case 'n': 
			case 'N': 
				Ydir = VRT_DN;
				n = strncpy (Ydir_Str, "Down\0", 5);
				break;

			default: 
				era_line ();
				dwn_scr() ;
				printw("              That was an invalid response,");
				dwn_scr() ;
				printw(" please try again.");
				refresh() ;
				delay (2000);
				era_line ();
				isok = 0;
				break;
		}
	} while (isok == 0);

/* ************** table ************* */

	prnt_header ();

	dwn_scr() ;
	dwn_scr() ;
	dwn_scr() ;
	printw("                 The digitizer can be mounted on the Top, Left");
	dwn_scr() ;
	printw("                          or Right Edge of the table.");
	dwn_scr() ;
	printw("                              T = Top");
	dwn_scr() ;
	printw("                              L = LeFt");
	dwn_scr() ;
	printw("                              R = Right");

	do
	{
		isok = 1;
		dwn_scr() ;
		printw("                  Which edge is the digitizer mounted on? [T] ");
		refresh() ;
		answer = getchar ();

		switch (answer & 0x7f)
		{
			case '\n': 
			case 't': 
			case 'T': 
				n = strncpy (Edge_Str, "Top\0", 4);
				Edge = TOP_MOUNT;
				break;
			case 'l': 
			case 'L': 
				n = strncpy (Edge_Str, "Left\0", 5);
				Edge = LEFT_MOUNT;
				break;
			case 'r': 
			case 'R': 
				n = strncpy (Edge_Str, "Right\0", 6);
				Edge = RIGHT_MOUNT;
				break;

			default: 
				era_line ();
				dwn_scr() ;
				printw("              That was an invalid response,");
				dwn_scr() ;
				printw(" please try again.");
				refresh() ;
				delay (2000);
				era_line ();
				isok = 0;
		}
	} while (isok == 0);

	D_digit_init(Cpi, Xdir, Ydir, Edge) ;
	D_get_scale(&Scale) ;

}				/* ** end of set_xy ** */
/*************************** R_W_Config_File *********************************/
R_W_Config_File ()
{

}

/***************************** get_option **********************************/
get_option ()
{
	int     isok;
	char    answer;

	do
	{
		isok = 1;
		answer = getchar ();

		switch (answer)
		{
			case '\n': 
				return;
				/* No change wanted so exit proc. */
			case '0': 
				Done = 1;/* We're finished so leave */
			case '1': 
				D_set_origin ();
				break;
			case '2': 
				set_resolution ();
				break;
			case '3': 
				set_xy ();
				break;
			case '4': 
				break;
			case '5': 
				R_W_Config_File ();
				break;
			default: 
				era_line ();
				dwn_scr() ;
				printw("That was an invalid response, please try again.");
				delay (2000);
				era_line ();
				dwn_scr() ;
				printw("Option: ");
				refresh() ;
				isok = 0;
		}
	} while (isok = 0);

}				/* ** end of get_option ** */
/****************************** era_line ***********************************/
era_line ()
{
	move(cur_screen_line, 0) ;
	clrtoeol() ;
}				/* ** end of era_line ** */
/****************************** delay *************************************/
delay (n)
int     n;
{
	for (; n > 0; --n);
}				/* ** end of delay ** */
/******************************* cursor ***********************************/
cursor (o)
int     o;
{
/*
#ifdef notdef
	if (o == 0)
	{
		printw("\x9B\x30\x20\x70");/* make cursor invisible
	}
	if (o != 0)
	{
		printw("\x9B\x20\x70");   /* make cursor visible
	}
#endif
*/
}				/* ** end of cursor ** */
/*************************** readall ***********************************/

undoit ()
{
	endwin() ;     /* Curses clean up */
	D_end_digit() ;
	exit (0);
}

open_failed()
{

	fprintf(stderr, "\n Could not open digitizer.\n") ;
	exit(-1) ;
}

/*************************  init_curses ************************/

init_curses()
{
	initscr () ;
	raw() ;
	crmode() ;
	noecho() ;
	nonl()   ;
}
dwn_scr()
{
	cur_screen_line++ ;
	move(cur_screen_line, 0) ;
}

/***************************** end of file ********************************/
