
#include    <stdio.h>
#include    <sgtty.h>
#include	<signal.h>

#include	<sys/file.h>



#define	ser_name	"/dev/tty2"
#define	CHARS_RD	11
#define	ALARM_TIME	3


static	int     IORser;
static	int     No_response ;
static	int		N_read ;

static	char    InBuffer[31] ;


main ()
{
	int		i ;
	int		S ;
	int	X,	Y ;
	char	c ;
	char	buf[80] ;


	if (D_open_serial (ser_name) < 0)
		exit (-1);

	printf ("\n Hit <RETURN> to test or cntrl-D to end:\n\n") ;


while (gets(buf) != NULL)
{

	for (i=0; i<30; i++)
		InBuffer[i] = NULL ;


	i = write ( IORser, "3", 1);		/*  request test & status  */

	printf (" should have written one character.  wrote: %d.\n", i) ;

	if ( D__read() <0)		/*  bad read  */
	 {
		printf (" bad read.\n\n") ;
		printf (" number read: %d\n\n", N_read) ;
		continue ;
	 }
			
	printf (" number read: %d\n\n", N_read) ;

	if (N_read == 0)
	 {
		printf (" nothing read.\n\n") ;
		continue ;
	 }
			

	printf ("\n Name (kurta): ") ;
	for (i=0; i<5; i++)
		printf ("%c ", InBuffer[i]) ;


	/*  reverse switch status for clarity.  */
	printf ("\n\n switch status : ") ;
	c = (InBuffer[6] & 0xf) | (InBuffer[5] << 4) ;
	for ( i = 0; i<8; i++)
	 {
		printf (" %d", (c & 0x1)) ;
		c >>= 1 ;
	 }
	printf ("\n switch number :  1 2 3 4 5 6 7 8\n\n") ;

	printf (" Unit Type Number(3): %c\n", InBuffer[7]) ;
	printf (" Revision Number(A): %c\n", InBuffer[8]) ;
	printf (" Unit Part Number(3): %c\n", InBuffer[9]) ;
	printf ("\n Last one is carriage return (\\r)\n") ;

	/**

		for (i=0; i<N_read+2; i++)
		 {
			printf (" [%d] = %x ", i, InBuffer[i]) ;
			if (!(i%5))
				printf ("\n") ;
			InBuffer[i] = NULL ;
	 }
	**/

	printf ("\n --------------------------------\n\n") ;

	/*  status info 'KURTA'  switch 2 status, switch 2 status, unit type number,
	*	revision number, unit part number and carriage return.
	*  	'KURTA823A3' and <cr>.  
	*  the switch status is read in reverse. 8,2 -> 1000 0011.
	*  means switch 1,2, and 8 are on.
	*/
	
}

	if (D_end_digit () < 0)
		exit (-1);


}	/*  main()  */




int		odisc ;
struct	sgttyb	osgttyb ;
struct	tchars	otchars;
struct	ltchars	oltchars;



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



D_end_digit ()
{


	if (ioctl (IORser, TIOCSETP, &osgttyb) < 0)
		perror ("ioctl could not set parameters");
	if (ioctl (IORser, TIOCSETC, &otchars) < 0)
		perror ("ioctl could not set parameters");
	if (ioctl (IORser, TIOCSLTC, &oltchars) < 0)
		perror ("ioctl could not set parameters");
	if (ioctl (IORser, TIOCSETD, &odisc) < 0)
		perror ("ioctl could not set parameters");


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

	return (N_read);

}	/*  end of r_read ()  */



jtimeout ()
{
	longjmp (jmp_alarm, 1);
}



