

/*  this is for binary format 2.  hi resolution packed binary; six characters.
*  switchs:	 6:off,  7:off,  8:off .
*/

#include	<stdio.h>
#include	<signal.h>

#include	<termio.h>
#include	<fcntl.h>


#define	ser_name	"/dev/tty0"
#define	CHARS_RD	6
#define	ALARM_TIME	3

static	int     IORser;
static	int		N_read ;

static	char    InBuffer[31] ;

main ()
{
	int		i ;
	int		S ;
	int	X,	Y ;
	int	x,	y ;		/*  old X, and Y */

	long	good_r;		/*  no. of good reads  */
	long	bad_r;
	long	no_resp;

	char	buf[20] ;

	for (i=0; i<30; i++)
		InBuffer[i] = NULL ;

	printf ("\n Now opening digitizer: %s\n", ser_name) ;

	if (D_open_serial (ser_name) < 0)
		exit (-1);

	setbuf (stdout, NULL) ;

	printf ("\n\n\n\n       KURTA DIGITIZER:  binary format 2.\n\n\n\n\n") ;

	printf ("  High or low resolution, default is high. (h/l):  ") ;

	if (gets(buf) == NULL)
	{
		clearerr(stdin) ;
		buf[0] = NULL ;
	}

	if ( buf[0] == 'l')
	 {
		write (IORser, "E", 1) ;			/*  low resolution  */
		printf ("\n  digitizer to low resolution.") ;
	 }
	else
	 {
		write (IORser, "F", 1) ;			/*  high resolution  */
		printf ("\n  digitizer to high resolution.") ;
	 }


	printf ("\n\n\n\n") ;

	good_r = bad_r = no_resp = 0 ;

	printf ("     X           Y      reads:   good       bad     none  Nread\n") ;


while(1)
{

	write (IORser, "2", 1) ;			/*  request coor  */

	if (D__read () < 0)		/*  no response  */
	{
		++no_resp ;
		printf ("\r   %35s  no response:  %ld  ", "", no_resp) ;
		continue ;
	}


	/***************
	if ( (N_read<0)	 ||   (N_read != (CHARS_RD - 1)))
	 {
		++bad_r ;
		printf ("\r   %35s  bad read:  %g           N_read:   %d ",
				"", bad_r, N_read) ;
		continue ;
	 }
	***************/
			

	/*  format 2  */

	S =   (int) ( (InBuffer[0] & 0x7c) >> 2) ;

	if (S)				/*  status should be zero  */
	 {
		++bad_r ;
		printf ("\r  %35s  bad read:  %ld  N_read: %d", "",
			bad_r, N_read ) ;
		continue ;
	 }


	X =    ((int) (InBuffer[0] & 0x3))  << 14
		|  ((int) (InBuffer[1] & 0x7f))  << 7
		|  ((int) (InBuffer[2] & 0x7f)) ;


	Y =    ((int) (InBuffer[3] & 0x3))  << 14
		|  ((int) (InBuffer[4] & 0x7f))  << 7
		|  ((int) (InBuffer[5] & 0x7f)) ;



	for (i=0; i < 0; i++)		/*  slow it down  */
		;

	++good_r ;
	if ( (X == x)  &&  (Y == y))
		continue ;

	printf ("\r    %d       %d              %ld,       %ld        %ld     %d    ",
	X, Y, good_r, bad_r, no_resp,  N_read) ;

	x = X ;     y = Y ;


	/******   for debugging

	printf ("\n Status = %d, X = %d,  Y = %d\n\n", S, X, Y) ;

	printf (" InBuffer[] :\n\n") ;

	for (i=0; i<N_read+2; i++)
	 {
		printf (" [%d] = %x ", i, InBuffer[i]) ;
		if (!(i%5))
			printf ("\n") ;
		InBuffer[i] = NULL ;
	 }

	printf ("\n --------------------------------\n\n") ;

	******/
	
}

	if (D_end_digit () < 0)
		exit (-1);


}	/*  main()  */



struct	termio	termio ;

D_open_serial (tty_name)
char   *tty_name;
{


	if ((IORser = open (tty_name, O_RDWR, 0)) < 0)
	{
		perror ("Could not open device");
		return (-1);
	}
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



D_end_digit ()
{


	close (IORser);
}



#include	<signal.h>
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

	return (N_read = CHARS_RD - len);

}	/*  end of r_read ()  */



jtimeout ()
{
	longjmp (jmp_alarm, 1);
}



