#include <stdio.h>
#include <signal.h>
static int interrupt = 0 ;

set_signals()
{
	int sigint();

/* set the ctrlz ignore */
#ifdef SIGTSTP
	signal (SIGTSTP, SIG_IGN);
#endif

/* set other signal catches */

	interrupt = 0;

	signal (SIGINT, sigint);
}

sigint(n)
{
	signal (n,sigint);
	interrupt = n;
}

check_signal()
{
	int x;
	x = interrupt;
	interrupt = 0;
	return(x) ;
}
