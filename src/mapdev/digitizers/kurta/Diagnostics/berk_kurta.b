


/*  this is for binary format 2.  hi resolution packed binary; six characters.
*  switchs:	 6:off,  7:off,  8:off .
*/


#include	<stdio.h>
#include	<signal.h>

#include	<fcntl.h>

#define	CHARS_RD	6
#define	ALARM_TIME	5

static	int     IORser;
static	int	N_read ;

static	char    InBuffer[31] ;

main ()
{
	int		i ;
	int		S ;
	int		K ;

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

	printf("\n this is set to 4800 baud\n\n") ;

	prompt ( tty, "  Which tty is the digitizer connected to (ttya):  ") ;

	if (*tty == NULL)
		sprintf ( buf, "/dev/ttya") ;
	else
		sprintf ( buf, "/dev/%s", tty) ;


	printf ("\n Now opening digitizer on %s\n", buf) ;

	if (D_open_serial (buf) < 0)
		exit (-1);

	printf ("\n\n\n\n       KURTA DIGITIZER:  binary format 6 characters.\n\n\n\n\n") ;

	D_init_digit() ;
	set_resolution(buf) ;

	printf ("\n\n\n\n") ;

	good_r = 0 ; bad_r = 0 ; no_resp = 0 ;


	printf ("     X           Y        Key #  reads:   good       bad     none  Nread \n") ;

	D_flush() ;

while(1)
{
	/*  ask for a coordinate   */
	D_write_digit ("2") ;

	delay(1) ;

	if (D__read () < 0)		/*  no response  */
	{
		++no_resp ;
		printf ("\r   %35s  no response:  %ld  ", "", no_resp) ;
		continue ;
	}

	if ( (N_read<0) || N_read != CHARS_RD)
	 {
		++bad_r ;
		printf ("\r   %35s  bad read:  %ld           N_read:   %d  ",
				" flushing input", bad_r, N_read) ;
		sleep(2) ;
		D_flush() ;
		continue ;
	 }
			

	/*  binary standard format  2  */

	/*  K is which key is hit 0-15  */
	K =   (int)((InBuffer[0] & 0x3c)  >> 2) ;

	if (K<0)
	 {
		++bad_r ;
		printf ("\r   %35s  bad read:  %ld      Status bad ", "", bad_r) ;
		continue ;
	 }


	/*  S is true if cursor key was hit  */
	if ((S =  (int)(InBuffer[0] & 0x40)))
		++K ;			/*  0-15 make range 1-16  */
	else
		K = 0 ;



	X =    ((int) (InBuffer[0] & 0x3))  << 14
		|  ((int) (InBuffer[1] & 0x7f))  << 7
		|  ((int) (InBuffer[2] & 0x7f)) ;


	Y =    ((int) (InBuffer[3] & 0x3))  << 14
		|  ((int) (InBuffer[4] & 0x7f))  << 7
		|  ((int) (InBuffer[5] & 0x7f)) ;


	++good_r ;

	/*****
	if ( (X == x)  &&  (Y == y))
	{
		printf ("\r %25s %d    same point  ", "", FC) ;
		continue ;
	}
	*****/


	printf ("\r    %d       %d      %d              %ld,       %ld        %ld     %d  ",
	X, Y, K, good_r, bad_r, no_resp,  N_read) ;


	x = X ;     y = Y ;

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
		D_write_digit ("E") ;		/*  low resolution  */
		printf ("\n  digitizer to low resolution.") ;
	 }
	else
	 {
		D_write_digit ("F") ;		/*  high resolution  */
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

delay(n) /* delay n milliseconds */
	int  n ;
{
	char zero;
	int i;

	zero = 0;

/* this assumes 9600 baud to stderr */
	while (n-- > 0)
		for (i = 0; i < 10; i++)
			write (2, &zero, 1);
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


#include	<sgtty.h>


D_open_serial (tty_name)
char   *tty_name;
{


	int		disc ;
	struct	sgttyb	sgttyb ;
	struct	tchars	tchars;
	struct	ltchars	ltchars;


	if ((IORser = open (tty_name, 2, 0)) < 0)
	{
		perror ("Could not open device");
		return (-1);
	}
 /* 
  * get the tty structures
  */

	if (ioctl (IORser, TIOCGETP, &sgttyb) < 0)
		perror ("ioctl failed on device");

 /* 
  * go into RAW mode using OLD tty interface
  */

	disc = OTTYDISC ;
	sgttyb.sg_ispeed = sgttyb.sg_ospeed = B4800;
	sgttyb.sg_flags = RAW;



 /* 
  * now set the modes and flags 
  */

	if (ioctl (IORser, TIOCSETP, &sgttyb) < 0)
		perror ("ioctl could not set parameters");
	if (ioctl (IORser, TIOCSETD, &disc) < 0)
		perror ("ioctl could not set parameters");


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

	/* not needed in kurta */

}

D_write_digit( string)
	char	*string ;
{
	int	str_length ;
	int count;

	str_length = strlen (string) ;

	if (write (IORser, string, str_length) != str_length)
	{
		fprintf( stderr, "\n Could not write string:\n'%s'\n to digitizer.\n",
			string	) ;	
		exit(-1) ;
	}
    while (1)
    {
	if (ioctl(IORser, TIOCOUTQ, &count) < 0)
	{
		/*
		fprintf (stderr, "ioctl error on digitizer\n");
		*/
		break;
	}
	if (count <= 0) break;
	fprintf (stderr, "%d chars still in output queue\n",count);
    }
    delay (15);
	return(0) ;
}


#include	<sys/file.h>
D_flush()
{
	int  i ;

/*  flush digitizer (input) */

	ioctl (IORser, TIOCFLUSH, FREAD) ;

/******
System V flush
	ioctl (IORser, TCFLSH, 0) ;
******/

	return(0) ;
}
