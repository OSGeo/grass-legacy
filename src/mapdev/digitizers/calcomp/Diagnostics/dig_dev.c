/*  @(#)dig_dev.c	1.1  5/4/87  */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#ifdef  USE_TERMIO
#include <termio.h>
#include <fcntl.h>
#endif

#ifndef USE_TERMIO
#include <sgtty.h>
#include <sys/file.h>
#endif

#define	CHARS_RD	16
#define ALARM_TIME	3
#define TRIES	20

static char    InBuffer[200];

static float Scale ;


/*  digitizer device variables  */

static int	IORser;
static int	N_read;



/*  Digitizer initialization */
D_digit_init()
{

/*  operating mode has to be set before any other settings,
*   POINT operating mode doesn't work well with our software.
*/
	D_write_digit (" \033%R\r");	/*  set operating mode (run) */
	delay(5) ;

	D_write_digit (" \033%Q!\r");	/*  set prompt character  */
	delay(5) ;

	/*  disable echo on port B/D  */
	D_write_digit (" \033%E0\r");
	delay(5) ;

	/*  enable cursor  */
	D_write_digit (" \033%K1\r");
	delay(5) ;

/*  Maximum data rate (points per second) at 9600 baud is 50  */
	D_write_digit (" \033%W50\r");
	delay(5) ;

	D_flush() ;
	delay(5) ;

/*  Hardware dip switches are set to 1000 LPI (lines per inch).
*   Which gives a scale of .001 inches.
*/
	Scale = .001;
}



D_readall (Xraw, Yraw)
	int     *Xraw;
	int     *Yraw;
{
	int	X,Y;
	int	Button;
        int     KpdChar;
	int     tries;
	int     no_response;
	char	debug[90] ;
	char	junk[90] ;

	tries = 0;
	N_read = 0;
	no_response = 0 ;

	while (tries < TRIES)
	{

		D_write_digit (" !") ;			/* request data */


		if ( D__read(ALARM_TIME) < 0)		/*  no response  */
			continue ;		/*  cursor off the tablet  */



	/*  are we out of sync  */
	    
		{
		    InBuffer[15] = 0;
		    fprintf (stderr, "BUFFER: '%s'\n", InBuffer);
		}
        	if (InBuffer[12] != 'A')
       		{
			fprintf(stderr, "HOLD:  We are out of sync with the digitizer.\n") ;
			fprintf(stderr, " Trying to recover\n") ;
			D_flush() ;
		/*  reset  the digitizer  */
			D_write_digit (" \033%VR\r");
			delay(5) ;
		/*  re-init the digitizer  */
			D_digit_init() ;
			++tries ;
			continue ;
        	}


		if ( N_read < 0  ||  N_read != CHARS_RD )
		{
			++tries ;
			fprintf( stderr, 
				"Digitizer read error (incorrect char count). N_read: %d \n",
				 N_read) ;
			D_flush() ;
			continue ;
		}
		else
			break ;
	}


	if (tries >= TRIES)
	{
		sprintf (debug,"tries %d, no_response %d", tries, no_response);
		fprintf (stderr, " %s",  debug);
                delay(10);
		return (-1);
	}


	/*  standard output format 16 characters  */

/*
	sscanf (InBuffer,"%5d,%5d,%*c%*c%1d",Xraw,Yraw,KpdChar);
*/
	sscanf (InBuffer,"%d,%d,%s",Xraw,Yraw,junk);

        /* If a button on the digitizer cursor was pressed, convert the button
         * to a number and add one to it. The buttons on the cursor range from
         * 0-14, but the calling subroutine wants the range to be 1-15. Return
         * 0 on succesful read with no key cursor key hit.
         */

        if (InBuffer[14] != 'U')
        {
	  Button = (InBuffer[14] - '0') ;
          return (++Button);
        }
        else
        {
	  return (0) ;
        }


}


D_ask_if_err()
{
    /***
	if ( ! curses_yes_no( 2," Digitizer read error.Do we continue(y,n)? ") )
	  close_down(-1) ;
    ***/
	  close_down(-1) ;
}

D_get_scale(scale)
	float *scale ;
{
	*scale = Scale ;
}


D_end_digit ()
{

	D_write_digit (" \033%VR\r");
	delay(5) ;
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
		cp += N_read ;
		len -= N_read ;
	}

	alarm (0);
	signal (SIGALRM, SIG_DFL);


	if (N_read < 0)					  /*  did it timeout  */
		return (-1);


/*  strip parity   */

	for ( cp = InBuffer;  cp < InBuffer + N_read;  cp++)
		*cp &= 0177;

	return (N_read = CHARS_RD - len);

}	/*  end of D__read ()  */



jtimeout ()
{
	longjmp (jmp_alarm, 1);
}

D_open_failed()
{

	alarm(0) ;
	signal( SIGALRM, SIG_DFL) ;
	fprintf(stderr, "\n Could not open digitizer...\n") ;

        sleep(10);

	close_down(-1) ;

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



#ifdef  USE_TERMIO
	ioctl (IORser, TCFLSH, 0) ;
#else
	ioctl (IORser, TIOCFLUSH, 0) ;
	/*  changed FREAD to 0  */
#endif

	return(0) ;
}



/*    system V code for D_open_serial() begins  */

#ifdef  USE_TERMIO

struct	termio	termio ;

D_open_serial (tty_name)
char   *tty_name;
{

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
  *  some computers may not be able to handle 9600 baud.
  */

	termio.c_iflag = IGNBRK ;
	termio.c_oflag = 0 ;
	termio.c_cflag = B9600 | CS8 | CREAD| HUPCL | CLOCAL ;
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


#ifndef  USE_TERMIO

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

