

/*	Altek - diagnostics program. 
*	this is for binary standard format six characters.
*
*	Digitizer will not send anything if the cursor is off the table.
*	Cable used is straight thru on the 3B2.
*	Any command down loaded (written ) to the digitizer that takes an
*	argument must have a '\r' following the argument. Eg: "M7\r"
*	Commands of only a single character don`t use the '\r' . Eg: "P"
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

static	FILE    *fp ;

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

	setbuf (stdout, NULL) ;

	printf("\n this is set to 1200 baud, auto mode for the moment\n\n") ;

	prompt ( tty, "  Which tty is the digitizer connected to (tty0):  ") ;

	if (*tty == NULL)
		sprintf ( buf, "/dev/tty0") ;
	else
		sprintf ( buf, "/dev/%s", tty) ;


	printf ("\n Now opening digitizer on %s\n", buf) ;

	if (D_open_serial (buf) < 0)
		exit (-1);

	if ( (fp = fopen("diag_out", "w")) == NULL) 
	{
		printf ("Could not open diag_out\n") ;
		exit(-1) ;
	}

delay(9000) ;
	printf ("\n\n\n\n       ALTEK DIGITIZER:  standard binary format.\n\n\n\n\n") ;

	D_init_digit() ;
	set_resolution(buf) ;

	printf ("\n\n\n\n") ;

	sleep(3) ;
	D_flush() ;


while(1)
{
	SB = PR = FC = HP = -1 ;

	for (i=0; i<30; i++)
		InBuffer[i] = 0 ;


	/*  ask for a coordinate */
	D_write_digit ("V") ;

	if (D__read () < 0)		/*  no response  */
	{
		printf ("  no response\n") ;
		fprintf ( fp, "  no response\n") ;
		continue ;
	}



	/*  standard binary format   */

	/*  this will still send output even if the cursor is outside the 
	*  the tablet range.  have to check the PR bit  
	*/

	/*  SB (Status byte):  1 - button down,  0 - button up  */
	/*  FC (Flag Char.):  0-9,A-F   cursor button hit  */
	/*  PR (Proximity):  1 - out of proximity,  0 - in proximity  */ 
	/*  HP (host point requested by host with 'V' command) -
	*		1 - if cursor is off the table when point is requested.
	*/

	SB =   (int)(InBuffer[0] & 0x40) ;
	PR =   (int)(InBuffer[3] & 0x20) ;
	FC =   (int)( (InBuffer[0] & 0x3c) >> 2) ;
	HP =   (int)(InBuffer[3] & 0x10) ;

	if (PR)
	 {
		printf ("\r%70s", "  cursor off the tablet           ") ;
		fprintf (fp, "%70s", "  cursor off the tablet           ") ;
		/************
		continue ;
		************/
	 }


	X =    ((int) (InBuffer[3] & 0x08))  << 16
		|  ((int) (InBuffer[0] & 0x3))  << 14
		|  ((int) (InBuffer[1] & 0x7f))  << 7
		|  ((int) (InBuffer[2] & 0x7f)) ;


	Y =    ((int) (InBuffer[3] & 0x7))  << 14
		|  ((int) (InBuffer[4] & 0x7f))  << 7
		|  ((int) (InBuffer[5] & 0x7f)) ;


	/*  button down
	if (SB)
		printf("  %d   ", FC) ;
	***/


	/****
	if ( (X == x)  &&  (Y == y))
		continue ;
	****/


	x = X ;     y = Y ;



	printf ("\n SB = %d, X = %d,  Y = %d, PR = %d, FC = %d, HP = %d\n\n",
		SB, X, Y, PR, FC, HP) ;
	fprintf ( fp, "\n SB = %d, X = %d,  Y = %d, PR = %d, FC = %d, HP = %d\n\n",
		SB, X, Y, PR, FC, HP) ;

	printf (" InBuffer[] :\n\n") ;
	fprintf (fp, " InBuffer[] :\n\n") ;

	for (i=0; i<N_read+2; i++)
	 {
		printf (" [%d] = %x ", i, InBuffer[i]) ;
		fprintf (fp, " [%d] = %x ", i, InBuffer[i]) ;
		if (!(i%5))
		{
			printf ("\n") ;
			fprintf (fp, "\n") ;
		}
		InBuffer[i] = NULL ;
	 }

	printf ("\n --------------------------------\n\n") ;
	fprintf (fp, "\n --------------------------------\n\n") ;

	/**
	sleep(3) ;
	**/
	
}

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
	termio.c_cflag = B1200 | CS8 | CREAD| HUPCL | CLOCAL ;
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
		N_read = read (IORser, cp, len);	
/*
fprintf( stderr, " N_read: %d\n", N_read) ;
delay(99999999) ;
*/
		cp += N_read ;
		len -= N_read ;
	}

	alarm (0);
	signal (SIGALRM, SIG_DFL);


	if (N_read < 0)					  /*  did it timeout  */
		return (-1);


		/*  strip parity  */
	for ( cp = InBuffer;  cp < InBuffer + N_read;  cp++)
		*cp &= 0177;

	return (N_read);

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

	ioctl (IORser, TCFLSH, 0) ;

/******
	for (i = 0; i < 10; i++)
		if ( D__read(1) < 0)
			return(0) ;
******/

	return(0) ;
}

