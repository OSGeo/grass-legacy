/*  @(#)dig_dev.c	1.1  5/4/87  */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#ifdef  SYSV
#include <termio.h>
#include <fcntl.h>
#endif

#ifndef SYSV
#include <sgtty.h>
#include <sys/file.h>
#endif

#define	CHARS_RD	15
/***
#define	CHARS_RD	14
***/
#define ALARM_TIME	3
#define TRIES		20

#define BEL		"0x07"

static char    InBuffer[20];

static float Scale ;


/*  digitizer device variables  */

static int	IORser;
static int	N_read;

/*  Digitizer initialization -
*	set digitizer resolution,
*	set program scale.
*/
D_digit_init()
{


/*  resolution to .001 of a inch (3)  */
/* and set request mode (7) */
	D_write_digit ("37") ;

	Scale = 0.001;
	delay(5) ;

	D_flush() ;

}


D_readall (Xraw, Yraw)
	int     *Xraw;
	int     *Yraw;
{
	int     len, tries, no_response ;
	int     a;
	int     K;
	int     S;
	char	debug[90] ;

	no_response = 0 ;
	tries = 0;
	N_read = 0;


	while (tries < TRIES)
	{

		D_write_digit ("R") ;		/* request data */

#ifdef	DEBUG
	sprintf( debug, "   tries: %d,  N_read: %d,  no_response: %d",
			tries, N_read, no_response) ;
	Write_base( 12, debug) ;
#endif


		if ( D__read(ALARM_TIME) < 0)		/*  no response  */
			continue ;


	/*  K is which key is hit 0-12,*,#  */
		K =   (int)( InBuffer[12] );
		if (K>='0' && K<'9') K = K - '0';

		if ( N_read < 0  ||  N_read != CHARS_RD ||  K<0)
		{
			++tries ;
fprintf( stderr, "Digitizer read error. N_read: %d \n", N_read) ;
			D_flush() ;
			continue ;
		}
		else
			break ;
	}


	if (tries >= TRIES)
	{
		sprintf (debug,"tries %d, no_response %d", tries, no_response);
		printf (8,debug);
		return (-1);
	}

/* Extract data from InBuffer */

	InBuffer[5] = '\0';	/* Set X, Y string terminators */
	InBuffer[11]= '\0';

	*Xraw = atoi( InBuffer+0 );	/* Convert values */
	*Yraw = atoi( InBuffer+6 );

	if (K != -1) if (K<1 || K>9) K = 0;  /* Check range of flag */

#ifdef	DEBUG
	sprintf( debug, "   X: %d,  Y: %d", *Xraw, *Yraw) ;
	Write_base( 13, debug) ;
#endif


/*  return -1 on read error,
*	return 0 on succesful read with no key cursor key (1-9) hit
*	return # of cursor key hit 1-9
*/

	return(K) ;
}


D_ask_if_err()
{
	exit(-1) ;

/****
	close_down(-1) ;
****/

}

D_get_scale(scale)
	float *scale ;
{
	*scale = Scale ;
}


D_end_digit ()
{

	close (IORser);
}


static	jmp_buf		jmp_alarm;

int  D__read (alarm_time)
	int  alarm_time ;
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


	alarm (alarm_time);

	cp = InBuffer ;
	len = CHARS_RD ;
	while (len > 0)
	{
		N_read = read (IORser, cp, len);	

/******  Debugging tool
{
int i ;
char *k ;
printf("After read N_read: %d", N_read) ;
k  = cp ;
for ( i=0; i<N_read; i++, k++)
	printf(" '%c'", (char)*k ) ;
printf("\n") ;
}
******/
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

	if (InBuffer[0]==012 || InBuffer[0]==015)
		return (-1);	/* Cursor out of bounds */
	
	InBuffer[15] = '\0'; /* set end of input string for sure */	
	return (N_read = CHARS_RD - len);

}	/*  end of D__read()  */



jtimeout ()
{
	longjmp (jmp_alarm, 1);
}

D_open_failed()
{

	alarm(0) ;
	signal( SIGALRM, SIG_DFL) ;
	delay(99999) ;
	fprintf(stderr, "\n Could not open digitizer...\n") ;

/****
	close_down(-1) ;
****/

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



/*****    ALL CODE BEYOND THIS POINT IS SYSTEM DEPENDANT     ****/

/*   D_flush() and D_open_serial()   */


D_flush()
{

/*  flush digitizer (input) */



#ifdef  SYSV
	ioctl (IORser, TCFLSH, 0) ;
#else
	ioctl (IORser, TIOCFLUSH, 0) ;
	/*  changed FREAD to 0  */
#endif

	return(0) ;
}



/*    system V code for D_open_serial() begins  */

#ifdef  SYSV

struct	termio	termio ;

D_open_serial (tty_name)
char   *tty_name;
{
fprintf(stderr,"\nGot to D_open_serial (%s) in dig_dev.c\n",tty_name);
	int	D_open_failed() ;
	int	(*alarm_set)() ;

	alarm_set = signal(SIGALRM, D_open_failed) ;
	alarm(5) ;

	if ((IORser = open (tty_name, O_RDWR, 0)) < 0)
		D_open_failed() ;

	alarm(0) ;
	signal(SIGALRM, alarm_set) ;
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
	termio.c_cflag = B9600 | CS7 | CREAD| HUPCL | CLOCAL | PARENB ;
	termio.c_lflag = 0 ;

	termio.c_cc[VEOF] = 0 ;
	termio.c_cc[VEOL] = 0 ;


 /* 
  * now set the modes and flags 
  */

	if (ioctl (IORser, TCSETA, &termio) < 0)
		perror ("ioctl failed on device");

	return (0);
}

#endif


/******      end of system V code     ***********/


#ifndef  SYSV

static	int		odisc ;
static	struct	sgttyb	osgttyb ;
static	struct	tchars	otchars;
static	struct	ltchars	oltchars;


D_open_serial (tty_name)
char   *tty_name;
{


	int		disc ;
	struct	sgttyb	sgttyb ;
	struct	tchars	tchars;
	struct	ltchars	ltchars;

fprintf(stderr,"\nGot to D_open_serial (%s) !SYSV in dig_dev.c\n",tty_name);

	if ((IORser = open (tty_name, 2, 0)) < 0)
	{
		perror ("Could not open device");
		return (-1);
	}
 /* 
  * get the tty structures and save
  */

	if (ioctl (IORser, TIOCGETP, &sgttyb) < 0)
		perror ("ioctl failed on device");
	if (ioctl (IORser, TIOCGETC, &tchars) < 0)
		perror ("ioctl failed on device");
	if (ioctl (IORser, TIOCGLTC, &ltchars) < 0)
		perror ("ioctl failed on device");
	if (ioctl (IORser, TIOCGETD, &disc) < 0)
		perror ("ioctl failed on device");



 /* 
  * copy over the structure 
  */

	osgttyb = sgttyb ;  otchars = tchars ;  oltchars = ltchars ;
	odisc = disc ;


	disc = OTTYDISC ;
	sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600;

	sgttyb.sg_flags = ODDP | RAW;

 /*
 *		eliminate effect of these characters.
 */

	tchars.t_intrc = tchars.t_quitc = tchars.t_startc = tchars.t_stopc =  -1 ;
	ltchars.t_suspc = ltchars.t_dsuspc = ltchars.t_flushc 
	= ltchars.t_lnextc =  -1 ;


 /* 
  * now set the modes and flags 
  */

	if (ioctl (IORser, TIOCSETP, &sgttyb) < 0)
		perror ("ioctl could not set parameters");
	if (ioctl (IORser, TIOCSETC, &tchars) < 0)
		perror ("ioctl could not set parameters");
	if (ioctl (IORser, TIOCSLTC, &ltchars) < 0)
		perror ("ioctl could not set parameters");
	if (ioctl (IORser, TIOCSETD, &disc) < 0)
		perror ("ioctl could not set parameters");


	return (0);
}

#endif

