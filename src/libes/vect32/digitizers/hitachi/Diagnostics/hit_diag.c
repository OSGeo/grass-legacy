


#include	<stdio.h>
#include	<signal.h>

#include	<fcntl.h>



main ()
{
	int		i ;
	int		Key ;
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

	fprintf (stdout,"\n this is set to 9600 baud\n") ;

	prompt ( tty, "  Which tty is the digitizer connected to (ttya):  ") ;
	fprintf (stdout,"\n Hit interrupt or break key to stop.\n\n") ;

	if (*tty == NULL)
		sprintf ( buf, "/dev/ttya") ;
	else
		sprintf ( buf, "/dev/%s", tty) ;


	fprintf (stdout,"\n Now opening digitizer on %s\n", buf) ;

	if (D_open_serial (buf) < 0)
		exit (-1);

	fprintf (stdout,"\n\n       HITACHI DIGITIZER\n") ;

	D_digit_init() ;

/***
	set_resolution(buf) ;
***/

	fprintf (stdout,"\n\n\n\n") ;

	good_r = bad_r = no_resp = 0 ;


	fprintf (stdout,"     X           Y        Key #  reads:   good       bad     none  Nread \n") ;

	D_flush() ;




	fprintf (stdout,"\rPlace cursor on tablet.          ") ;

while(1)
{
	/*  ask for a coordinate   */
	Key = D_readall ( &X, &Y) ;

	++good_r ;


	fprintf (stdout,"\r    %d       %d      %d              ",
	X, Y, --Key) ;

	x = X ;     y = Y ;
/*  so i can read it  */
	delay(10) ;

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
		fprintf (stdout,"\n  digitizer to low resolution.") ;
	 }
	else
	 {
		D_write_digit ("M1\r") ;		/*  high resolution  */
		fprintf (stdout,"\n  digitizer to high resolution.") ;
	 }

}

prompt( buf, p_string)
	char	*buf, *p_string ;
{
	fprintf (stdout,"%s", p_string) ;
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

	fprintf (stdout," closing tty ... ") ;
	D_end_digit() ;

	fprintf (stdout,"  and exiting \n\n") ;
	exit(0) ;
}

