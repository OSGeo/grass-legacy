


#include	<stdio.h>
#include	<signal.h>

#include	<fcntl.h>


static	char    InBuffer[31] ;

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

	for (i=0; i<30; i++)
		InBuffer[i] = NULL ;


	setbuf (stdout, NULL) ;

	printf("\n this is set to 9600 baud\n") ;

	prompt ( tty, "  Which tty is the digitizer connected to (ttya):  ") ;
	printf("\n Hit interrupt or break key to stop.\n\n") ;

	if (*tty == NULL)
		sprintf ( buf, "/dev/ttya") ;
	else
		sprintf ( buf, "/dev/%s", tty) ;


	printf ("\n Now opening digitizer on %s\n", buf) ;

	if (D_open_serial (buf) < 0)
		exit (-1);

	printf ("\n\n\n\n       CALCOMP DIGITIZER\n\n\n\n") ;

	D_digit_init() ;
/***
	set_resolution(buf) ;
***/

	printf ("\n\n\n\n") ;

	good_r = bad_r = no_resp = 0 ;


	printf ("     X           Y        Key #  reads:   good       bad     none  Nread \n") ;

	D_flush() ;




	printf ("\rPlace cursor on tablet.          ") ;

while(1)
{
	/*  ask for a coordinate   */
	Key = D_readall ( &X, &Y) ;

	++good_r ;


	printf ("\r    %d       %d      %d              ",
	X, Y, --Key) ;

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

delay(n) /* delay n milliseconds */
	int  n ;
{
	char zero;
	int i;

	zero = 0;

/* this assumes 9600 baud to stderr */
#ifdef DPG
    sleep (1);
#else
	while (n-- > 0)
		for (i = 0; i < 10; i++)
			write (2, &zero, 1);
#endif
}

close_down()
{
	signal( SIGALRM, SIG_DFL) ;

	printf(" closing tty ... ") ;
	D_end_digit() ;

	printf("  and exiting \n\n") ;
	exit(0) ;
}

