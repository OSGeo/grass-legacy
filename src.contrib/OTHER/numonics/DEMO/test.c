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

#define	CHARS_RD	6
#define ALARM_TIME	3
#define TRIES		5

static char InBuffer[200], buffer[100];

static float Scale;


/*  digitizer device variables  */

static int	IORser;
static int	N_read;



main()
{
	int x, y, button;

	D_open_serial("/dev/ttyb");
	D_digit_init();

	while (button != 3)  {
		button = D_readall(&x,&y);
		printf("%8d  %8d  %1d\n", x, y, button);
	}
	D_end_digit();
}


D_digit_init()
{
	sprintf(buffer,":PO\n:BP\n\n");
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
	while (tries < TRIES) {
		sprintf(buffer,":CD\n");
		D_write_digit(buffer);
		if ( D__read(ALARM_TIME) < 0 || N_read != CHARS_RD) {
			tries++;
			sprintf(buffer," Digitizer Error: Retry %3d of %d",
				tries,TRIES);
			sprintf(buffer,":PO\n:BP\n");
			D_write_digit(buffer);
			D_flush();
		}
		else
			break;
	}

	if (tries >= TRIES) {
		sprintf(buffer," After %3d tries \"ABORTING\" Digitizer!!!", tries);
		sleep(3);
		D_flush();
		return (-1);
	}


	/* NUMONICS FORMAT:   In Packed Binary */
	
        K = (int)(InBuffer[0] & 0x3c) >> 2;
        *Xraw = ((int)(InBuffer[0]) & 0x03) << 14 |
                 (int)(InBuffer[1]) << 7 |
                 (int)(InBuffer[2]);

        *Yraw = ((int)(InBuffer[3]) & 0x03) << 14 |
                (int)(InBuffer[4]) << 7 |
                (int)(InBuffer[5]);



	return(K) ;
}


D_ask_if_err()
{
	exit(1);
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

	exit(1);

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

