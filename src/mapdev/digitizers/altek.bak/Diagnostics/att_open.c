

/*	Altek - diagnostics program. 
*	this is for binary format 8.   six characters.
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

struct	termio	termio ;
main ()
{
	int  D_open_failed() ;

	signal(SIGALRM, D_open_failed ) ;
	alarm(5) ;

	fprintf(stderr, "Now opening at 1200 \n") ;

	if ((IORser = open ("/dev/tty0", O_RDWR, 0)) < 0)
	{
		perror ("Could not open device");
		exit (-1);
	}

	signal( SIGALRM, SIG_DFL) ;

	fprintf(stderr, "is now open\n") ;
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

	close (IORser);
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


