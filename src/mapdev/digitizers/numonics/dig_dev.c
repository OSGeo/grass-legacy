/*  @(#)dig_dev.c	1.1  5/4/87  */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>

#define DEBUG  1

#ifdef  SYSV
#include <termio.h>
#include <fcntl.h>
#endif

#ifndef SYSV
#include <sgtty.h>
#include <sys/file.h>
#endif

#define	CHARS_RD	15
#define ALARM_TIME	3
#define TRIES		10
#define XON             17
#define XOFF            19

static char InBuffer[200], buffer[100];

static float Scale;


/*  digitizer device variables  */

static int	IORser;
static int	N_read;


D_digit_init()
{
	sprintf(buffer,"\n:XN\n:PO\n:BP\n%c\n",XOFF);
	D_write_digit(buffer);
	Scale = .001;
	delay(5) ;
	D_flush() ;
}


D_readall (Xraw, Yraw)
	int     *Xraw;
	int     *Yraw;
{
	int     i, K = 0, tries = 0, no_response = 0;

	N_read = 0;
	sprintf(buffer,"%c\n",XON);
	D_write_digit(buffer);
	while (tries < TRIES) {
		D_write_digit("\n:CD\n");	
		if ( D__read(ALARM_TIME) < 0 || N_read != CHARS_RD) {
			tries++;
			sprintf(buffer," Digitizer Error: Retry %3d of %d",
				tries,TRIES);
			Write_base(17,buffer);
			sprintf(buffer,"\n:XN\n:PO\n:BP\n%c\n",XON);
			D_write_digit(buffer);
			D_flush();
		}
		else
			break;
	}
	sprintf(buffer,"%c\n",XOFF);
	D_write_digit(buffer);

	if (tries >= TRIES) {
		sprintf(buffer," After %3d tries \"ABORTING\" Digitizer!!!", tries);
		Write_base (17,buffer);
		sleep(3);
		D_flush();
		return (-1);
	}

	/* NUMONICS FORMAT:   XXXXX,YYYYY,B<CR><LF> */
	sscanf(InBuffer,"%5d,%5d,%1d",Xraw,Yraw,&K);
	return(K) ;
}


D_ask_if_err()
{
	if ( ! curses_yes_no( 2, " Digitizer read error. Do we continue(y,n)? ") )
	close_down(-1) ;
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

	int  timedout, len = CHARS_RD;
	char *cp;
	int  jtimeout();		/*  uses jum_buf  */

	if (timedout = setjmp (jmp_alarm))
	{
		signal (SIGALRM, SIG_DFL);	/*  timed out, reset  */
		return(-1) ;
	}
	else
		signal (SIGALRM, jtimeout);


	alarm (alarm_time);

	cp = InBuffer;
	while (len > 0)  {
		N_read = read(IORser,cp,len);
		cp += N_read;
		len -= N_read;
	}

	alarm (0);
	signal (SIGALRM, SIG_DFL);


	if (N_read < 0) return(-1);  /*  did it timeout  */
	N_read = CHARS_RD - len;
	
	for (len = 0; len < N_read; len++) InBuffer[len] &= 0x7f;
	return (N_read);
}	



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

