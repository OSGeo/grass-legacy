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



#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <curses.h>
#include <signal.h>
#include <sys/time.h>

struct sgttyb   digtty;



char    OutBuffer[200];
char    InBuffer[200];

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

/* count rate variables */

int     Cpi;
int     Cpi_100;
int     Cpi_200;
int     Cpi_400;
int     Cpi_800;

/* direction variables */

int     Xdir;
int     Ydir;
int     X_left;
int     X_right;
int     Y_up;
int     Y_down;
int     Swap_xy;
int     Edge;		    /* side of table that digitizer is mounted on */

unsigned  ser_num;	    /* the serial number is a sixteen bit number  */

int     Ft_Dly;		    /* delays to keep hit Status on screen        */
			    /* long * enough to see it                    */
int     Kpd_Dly;
int     Clr_On_Ftsw;        /* if true, we clear counters when footswitch is hit */
int     Ftsw_Bp;	    /* if true, we beep when the foot switch is hit */
int     Kpd_Bp;		    /* if true, we beep when the key pad is hit */
int     Done;		    /* Shows when we want to get out of this program */
int     KbdChar;	    /* char input from keyboard, not keypad */
int     KeyHit;

/* Just a temporary variable */
int     Junk;

char    Xdir_Str[20];	    /* Strings for displaying which direction */
char    Ydir_Str[20];	    /* causes the coordinates to increment */

char    Edge_Str[20];	    /* String for displaying where it's mounted  */
char    Cpi_Str[20];	    /* String for displaying count rate       */
char    Scale_Str[20];	    /* String for displaying scaling factor   */
char    Ftsw_Str[20];	    /* String for displaying footswitch Status */
char    Kpd_Str[20];	    /* String for displaying key pad Status   */

int     IORser;

/********************************* Main **************************************/
main (argc, argv)
int     argc;
char   *argv[];
{
	int     n;
	char   *dev_ptr;
	int     undoit ();
	char   *rindex ();
	char   *ptr;
	char    buf[BUFSIZ];
 setbuf(stderr,0) ;
 /* 
  * setup some signal handlers 
  */
	signal (SIGQUIT, undoit);
	signal (SIGHUP, undoit);
	signal (SIGINT, undoit);
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
	if (open_serial (dev_ptr) < 0)
	{
		printf ("Could not open serial line.\n");
		exit (1);
	}

fprintf(stderr, "\n Back from open\n") ;

	init_curses () ;        /* Initialize the curses package */
fprintf(stderr, " Curses initiliazed\n") ;

	init_var ();		/* Initialize variables */
 /* rd_config(); *//* Read configuration file, (not implemented yet) */

fprintf(stderr, " Variables initiliazed\n") ;
	init_dig ();		/* Initialize digitizer */
fprintf(stderr, " Digitizer initiliazed\n") ;

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
/***************************** prnt_header *********************************/
prnt_header ()
{
	clear ();
	move (2, 18);
	cur_screen_line = 2 ;
	printw ("Geographics Digitizer Diagnostics Version C1.00S");
}
/******************************* init_var *********************************/
init_var ()
{
	int     n;

	Cpi_100 = 0x08;
	Cpi_200 = 0x04;
	Cpi_400 = 0x02;
	Cpi_800 = 0x01;
	X_right = 0x00;
	X_left = 0x10;
	Y_up = 0x00;
	Y_down = 0x20;

 /* the initial configuration assumes 800 counts per inch */
	Cpi = Cpi_800;
	Xdir = X_right;
	Ydir = Y_up;
	n = strncpy (Cpi_Str, "800\0", 4);

 /* scaling factor for converting counts to inches and displaying it */
	Scale = .00125;
	n = strncpy (Scale_Str, "0.00125\0", 8);

 /* 
  * the initial configuration assumes the digitizer is mounted at the
  * top 
  */
	Swap_xy = 0;
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
	Clr_On_Ftsw = 1;	/* Do we want to reset the X & Y counters *
				   when   */
 /* we find the foot switch has been pressed?     */
	Ftsw_Bp = 0;		/* Do we want to give a beep when the foot *
				   switch */
 /* is hit?      */
	Kpd_Bp = 0;		/* Do we want to beep when the key pad is *
				   hit?   */
	Done = 0;		/* we aren't Done, were just begining    
				*/

}				/* ** end of init_var ** */
/****************************** init_dig ***********************************/
init_dig ()
{
	int     n;
	char    init_str[6];
	char    buf[255];

	n = (Xdir + Ydir + Cpi);

	sprintf (init_str, "$I%02x", n);

	write (IORser, init_str, strlen (init_str));
				/* send initializer string to dig */
fprintf(stderr, " Before Read \n") ;
	Actual = read (IORser, InBuffer, 25);/* clear out input Buffer */
fprintf(stderr, " After Read \n") ;

}				/* ** end of init_dig ** */
/****************************** writekpd **********************************/
writekpd(c)
	char c ;
{
	int n ;
	char kpd_str[6] ;

	if ( (c >= '0') && (c <= '9') )
	{
		sprintf(kpd_str, "$W%02x", c) ;    /* convert c to hexadecimal */
					/* send keypad string to digitizer */
		write (IORser, kpd_str, strlen(kpd_str) ) ;
					/* clear out input buffer */
		Actual = read (IORser, InBuffer, 25) ;
	}
}              /*** end of writekpd ***/
/****************************** digitize **********************************/
digitize ()
{
	int     n;
	int     nfds,
	        readfd,
	        writefd,
	        execptfds;
	struct timeval  timeout;


	cur_screen_line++ ;
	dwn_scr () ;
	printw ("|---------------------------------------------------------------------------|");
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

		Error = readall ();
				/* get string of all info from * digitizer
				   */

		if (FtswStat == 1)
		{
			Ft_Dly = 10;
			n = strncpy (Ftsw_Str, "Down\0", 5);
			if (Clr_On_Ftsw == 1)
				/* do we want to reset the * counters?  */
				set_origin ();
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
			
			writekpd(KpdChar) ;
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

fprintf( stderr, "    %s        %s       %c       %x            %3.5f         %3.5f           \n ",
				Ftsw_Str, Kpd_Str, KpdChar, KpdChar, Xcoord, Ycoord);

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
/*************************** set_origin ************************************/
set_origin ()
{
	write (IORser, "$Z", 2);/* Zero out counters */
	Actual = read (IORser, InBuffer, 25);/* clear lf */

}				/* ** end of set_origin ** */
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
		switch (answer & 0x7f)
		{
			case 0x0D: 
				return;
				/* No change wanted so exit proc. */

			case '1': 
				n = strncpy (Cpi_Str, "100\0", 4);
				n = strncpy (Scale_Str, "0.01\0", 5);
				Cpi = Cpi_100;
				Scale = .01;
				break;

			case '2': 
				n = strncpy (Cpi_Str, "200\0", 4);
				n = strncpy (Scale_Str, "0.005\0", 5);
				Cpi = Cpi_200;
				Scale = .005;
				break;

			case '3': 
				n = strncpy (Cpi_Str, "400\0", 4);
				n = strncpy (Scale_Str, "0.0025\0", 6);
				Cpi = Cpi_400;
				Scale = .0025;
				break;

			case '4': 
				n = strncpy (Cpi_Str, "800\0", 4);
				n = strncpy (Scale_Str, "0.00125\0", 8);
				Cpi = Cpi_800;
				Scale = .00125;
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

	init_dig ();

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
				Xdir = X_right;
				n = strncpy (Xdir_Str, "Right\0", 6);
				break;
			case 'n': 
			case 'N': 
				Xdir = X_left;
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
				Ydir = Y_up;
				n = strncpy (Ydir_Str, "Up\0", 3);
				break;

			case 'n': 
			case 'N': 
				Ydir = Y_down;
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
				Swap_xy = 0;
				break;
			case 'l': 
			case 'L': 
				n = strncpy (Edge_Str, "Left\0", 5);
				if (Xdir == X_right)
					Xdir = Y_down;
				else
					Xdir = Y_up;
				if (Ydir == Y_up)
					Ydir = X_right;
				else
					Ydir = X_left;

				Swap_xy = 1;
				break;
			case 'r': 
			case 'R': 
				n = strncpy (Edge_Str, "Right\0", 6);
				if (Xdir == X_right)
					Xdir = Y_up;
				else
					Xdir = Y_down;
				if (Ydir == Y_down)
					Ydir = X_right;
				else
					Ydir = X_left;
				Swap_xy = 1;
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

	init_dig ();

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
				set_origin ();
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
readall ()
{
	int     count,
	        len,
	        n,
	        tries;
	int     a;

	len = 18;
	tries = 0;
	Actual = 0;
	n = 0;

	while ((Actual < len) && (tries++ < 5))
	{
		write (IORser, "$A", 2);/* request string of all data */
		Actual = read (IORser, InBuffer, 25);
	}
	for (a = 0; a <= Actual; a++);
	{
		InBuffer[a] &= 0x7f;
	}
	if (tries >= 5)
	{
		move (24, 1);
		dwn_scr() ;
		printw("Readall failed to read correct string");
		refresh() ;
	/* put Error code here */
	}
	InBuffer[++Actual] = '\0';/* null terminate string */

	while ((InBuffer[n] != '>') && (InBuffer[n] != 0))
	{
		move (24, strlen ("Readall failed to read correct string") + 1);
		dwn_scr() ;
		printw("InBuffer[n] was !=  >");
		refresh() ;
		n++;		/* get lead-in char */
	}
	if (InBuffer[++n] == 'A')
	{
		if (InBuffer[++n] == ' ')
		{
			n++;

			count = sscanf (&InBuffer[n], "%4x %4x %2x %2x %2x",
					&Xraw, &Yraw, &FtswStat, &KpdStat, &KpdChar);
		}
		else
		{
			dwn_scr() ;
			printw(" wasn't an space" );
			refresh() ;
		}
	}
	else
	{
		dwn_scr() ;
		printw(" wasn't an A");
		refresh() ;
	}
}				/* ***** end of readall ****** */

/*************************  SERIAL I/O functions ************************/

open_serial (ser_name)
char   *ser_name;
{
	struct sgttyb   sgttyb;

	if ((IORser = open (ser_name, 2, 0)) < 0)
	{
		perror ("Could not open device");
		return (-1);
	}
fprintf(stderr,"At open\n") ;
 /* 
  * get the stty structure and save it 
  */

	if (ioctl (IORser, TIOCGETP, &digtty) < 0)
	{
		perror ("ioctl failed on device");
		return (-1);
	}
 /* 
  * copy over the structure 
  */

	sgttyb = digtty;

	sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600;

	sgttyb.sg_flags = ANYP;
 /* 
  * now set the modes and flags 
  */

	if (ioctl (IORser, TIOCSETP, &sgttyb) < 0)
	{
		perror ("ioctl could not set parameters");
		return (-1);
	}


	return (0);
}

undoit ()
{
	endwin() ;     /* Curses clean up */
	exit (0);
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
