

/*	Altek - diagnostics program. 
*	this is for binary format  six characters.
*
*	Cable used is straight thru on the 3B2.
*	Any command down loaded (written ) to the digitizer that takes an
*	argument must have a '\r' following the argument. Eg: "M7\r"
*	Commands of only a single character don`t use the '\r' . Eg: "P"
*/

/*  SB (Status byte):  1 - button down,  0 - button up  */
/*  FC (Flag Char.):  0-9,A-F   cursor button hit  */
/*  PR (Proximity):  1 - out of proximity,  0 - in proximity  */ 
/*  HP (host point requested by host with 'V' command) -
*		1 - if cursor is off the table when point is requested.
*/

/*  Altek will still send output even if the cursor is outside the 
*  the tablet range.  have to check the PR bit  
*/

/*	There are two ways that a coordinate can be sent by the cursor.
*	1)  When a set of coordinates is asked for with the V command.
*	2)	When a cursor key is hit
*
*	If the cursor is off the tablet and a coordinate is asked for; 
*	a set of coordinates will be sent with PR true (out of proximity) .
*
*	If cursor is on tablet:
*		Cursor key NOT hit, but coor. asked for.
*			PR = false
*			SB = false  button not down (up)
*			HP = true   point asked for
*			FC = true   but useless, because no key was hit
*
*	If cursor is on tablet:
*		Cursor key hit
*			PR = false
*			SB = true   button down (pressed)
*			HP = false  point NOT asked for
*			FC = true   contains number of cursor key hit
*
*/

#include	<stdio.h>
#include	<signal.h>

#include	<termio.h>
#include	<fcntl.h>


#define	CHARS_RD	6
#define	ALARM_TIME	3

static	int     IORser;
static	int		N_read ;

static	char    InBuffer[31] ;

main ()
{
	int		i ;
	int		SB ;
	int		HP ;
	int		FC ;
	int		PR ;
	int	X,	Y ;
	int	x,	y ;		/*  old X, and Y */

	long	good_r;		/*  no. of good reads  */
	long	bad_r;
	long	no_resp;

	char	buf[60] ;
	char	tty[60] ;

	int  close_down() ;

	signal(SIGINT, close_down ) ;

	for (i=0; i<30; i++)
		InBuffer[i] = NULL ;


	setbuf (stdout, NULL) ;

	printf("\n this is set to 9600 baud\n\n") ;

	prompt ( tty, "  Which tty is the digitizer connected to (tty0):  ") ;

	/*  tty14 is P6 on out masscomp  */

	if (*tty == NULL)
		sprintf ( buf, "/dev/tty0") ;  /* CHANGE */
	else
		sprintf ( buf, "/dev/%s", tty) ;


	printf ("\n Now opening digitizer on %s\n", buf) ;

	if (D_open_serial (buf) < 0)
		exit (-1);

delay(9000) ;
	printf ("\n\n\n\n       ALTEK DIGITIZER:  binary format 6 characters.\n\n\n\n\n") ;

	D_init_digit() ;
	set_resolution(buf) ;

	printf ("\n\n\n\n") ;

	good_r = bad_r = no_resp = 0 ;


	printf ("     X           Y        Key #  reads:   good       bad     none  Nread \n") ;

	D_flush() ;

while(1)
{
	/*  ask for a coordinate   */
	D_write_digit ("V") ;

delay(999999) ;


	if (D__read () < 0)		/*  no response  */
	{
		++no_resp ;
		printf ("\r   %35s  no response:  %ld  ", "", no_resp) ;
		continue ;
	}

	if ( (N_read<0)	 ||   (N_read != CHARS_RD))
	 {
		++bad_r ;
		printf ("\r   %35s  bad read:  %ld           N_read:   %d ",
				"", bad_r, N_read) ;
		continue ;
	 }
			

	/*  binary standard format   */


	SB =   (int)(InBuffer[0] & 0x40) ;
	PR =   (int)(InBuffer[3] & 0x20) ;
	FC =   (int)( (InBuffer[0] & 0x3c) >> 2) ;
	HP =   (int)(InBuffer[3] & 0x10) ;

	if (PR)
	 {
		printf ("\r%70s", "  cursor off the tablet           ") ;
		continue ;
	 }

	/****
	if (XX)
	 {
		++bad_r ;
		printf ("\r  %35s  bad read:  %ld  N_read: %d", "",
			bad_r, N_read ) ;
		continue ;
	 }
	****/


	X =    ((int) (InBuffer[3] & 0x08))  << 16
		|  ((int) (InBuffer[0] & 0x3))  << 14
		|  ((int) (InBuffer[1] & 0x7f))  << 7
		|  ((int) (InBuffer[2] & 0x7f)) ;


	Y =    ((int) (InBuffer[3] & 0x7))  << 14
		|  ((int) (InBuffer[4] & 0x7f))  << 7
		|  ((int) (InBuffer[5] & 0x7f)) ;

	/*  button not down, don`t use   */
	if (!SB)
		FC = -1 ;

	++good_r ;

	/*****
	if ( (X == x)  &&  (Y == y))
	{
		printf ("\r %25s %d    same point  ", "", FC) ;
		continue ;
	}
	*****/


	printf ("\r    %d       %d      %d              %ld,       %ld        %ld     %d  ",
	X, Y, FC, good_r, bad_r, no_resp,  N_read) ;


	x = X ;     y = Y ;

}
	/*NOTREACHED*/

	if (D_end_digit () < 0)
		exit (-1);


}	/*  main()  */


set_resolution( buf)
	char  *buf ;
{
	/*  there are actually 7 possible resolutions for this digitizer  */

	prompt ( buf, "  High or low resolution, default is high. (h/l):  ") ;

	if ( buf[0] == 'l')
	 {
		D_write_digit ("M5\r") ;		/*  low resolution  */
		printf ("\n  digitizer to low resolution.") ;
	 }
	else
	 {
		D_write_digit ("M1\r") ;		/*  high resolution  */
		printf ("\n  digitizer to high resolution.") ;
	 }

}

prompt( buf, p_string)
	char	*buf, *p_string ;
{
	printf ("%s", p_string) ;
	*buf = NULL ;
	if (gets(buf) == NULL)
	{
		clearerr(stdin) ;
		exit(0) ;
	}
}

delay(n)
	int  n ;
{
	for ( ; n < 0; n--)
		;
}

close_down()
{
	signal( SIGALRM, SIG_DFL) ;

	printf(" closing tty ... ") ;
	D_end_digit() ;

	printf("  and exiting \n\n") ;
	exit(0) ;
}


/* device driver code below this point  */


struct	termio	termio ;

D_open_serial (tty_name)
char   *tty_name;
{

	int  D_open_failed() ;

	signal(SIGALRM, D_open_failed ) ;
	alarm(5) ;

	if ((IORser = open (tty_name, O_RDWR, 0)) < 0)
	{
		perror ("Could not open device");
		return (-1);
	}

	alarm(0) ;
	signal( SIGALRM, SIG_DFL) ;
 /* 
  * get the tty structure
  */

	if (ioctl (IORser, TCGETA, &termio) < 0)
		perror ("ioctl failed on device");

 /* 
  *  change the settings
  */

	termio.c_iflag = IGNBRK ;
	termio.c_oflag = 0 ;
	termio.c_cflag = B9600 | CS8 | CREAD| HUPCL | CLOCAL ;
/***********
termio.c_cflag = B9600 | CS8 | CREAD| HUPCL | CLOCAL ;
***********/
	termio.c_lflag = 0 ;

	termio.c_cc[VEOF] = 0 ;
	termio.c_cc[VEOL] = 0 ;


 /* 
  * now set the modes and flags 
  */

	if (ioctl (IORser, TCSETA, &termio) < 0)
	{
		perror ("ioctl failed on device");
		return (-1) ;
	}

	return (0);
}



D_open_failed ()
{
	signal( SIGALRM, SIG_DFL) ;
	fprintf(stderr, "\n ERROR: Could not open digitizer\n") ;
	exit(-1) ;
}


D_end_digit ()
{
	D_write_digit ("") ;			/*  reset digitizer  */
	delay(3000) ;
	close (IORser);
}



#include	<setjmp.h>


static	jmp_buf		jmp_alarm;

int  D__read ()
{

	/***
	reads number of characters from the file ; if it can't read within
	five seconds it returns error status.
	**/

	int  timedout ;
	int  len ;

	char  *cp ;

	int	jtimeout();		/*  uses jum_buf  */

/* when setjmp() is called directly, it will return 0 (FALSE)
   when the longjmp() is called in jtimeout() it will appear that
   we have just executed the setjmp() call, but setjmp() will return
   1 (TRUE) since this is what we told longjmp() to ask setjmp()
   to return.
*/

	
/*  When a SIGALRM signal is sent  jtimeout will be executed.

    Set the alarm to five seconds before the read.  If the read executes before
    the five seconds is up then the alarm is reset to zero which nullifies the 
    alarm.  If the read doesn't occur before the five sec. ,  then alarm will 
    send the signal and jtimeout will be executed.
*/


	if (timedout = setjmp (jmp_alarm))
	{
		signal (SIGALRM, SIG_DFL);			/*  timed out, reset  */
		return(-1) ;
	}
	else
		signal (SIGALRM, jtimeout);


	alarm (ALARM_TIME);

	cp = InBuffer ;
	len = CHARS_RD ;
	while (len > 0)
	{
		N_read = read (IORser, cp, len );	
		if (N_read < 0)
		{
			fprintf (stderr, "Digitizer read error\n");
			continue;
		}

		cp += N_read ;
		len -= N_read ;
/***
fprintf( stderr, "\n len: %d, N_read: %d\n", len, N_read) ;
***/

	}

	alarm (0);
	signal (SIGALRM, SIG_DFL);


	if (N_read < 0)				  /*  did it timeout  */
		return (-1);

		/*  strip parity  */
	for ( cp = InBuffer;  cp < InBuffer + CHARS_RD;  cp++)
		*cp &= 0177;

	return (N_read = CHARS_RD - len);

}	/*  end of r_read ()  */



jtimeout ()
{
	longjmp (jmp_alarm, 1);
}


D_init_digit()
{
		D_write_digit ("") ;			/*  reset digitizer  */
		delay(3000) ;

		/*	 set output mode  */
		D_write_digit ("F8\r") ;
		delay(3000) ;
		D_write_digit ("P") ;			/*  set point mode  */
		delay(3000) ;
}

D_write_digit( string)
	char	*string ;
{
	int	str_length ;

	str_length = strlen (string) ;

	if (write (IORser, string, str_length) != str_length)
	{
		fprintf( stderr, "\n Could not write string:\n'%s'\n to digitizer.\n",
			string	) ;	
		exit(-1) ;
	}
	return(0) ;
}


D_flush()
{
	int  i ;

/*  flush digitizer by reading 10 coordinates or  until there no response */

	ioctl (IORser, TCFLSH, 0) ;

/******
	for (i = 0; i < 10; i++)
		if ( D__read(1) < 0)
			return(0) ;
******/

	return(0) ;
}

