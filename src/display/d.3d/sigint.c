#include <stdio.h>
#include <signal.h>
static int interrupt = 0 ;

void sigint(int);
int check_signal(void);

int set_signals (void)
{
/* set the ctrlz ignore */
#ifdef SIGTSTP
	signal (SIGTSTP, SIG_IGN);
#endif

/* set other signal catches */

	interrupt = 0;

	signal (SIGINT, sigint);

	return 0;
}

void sigint(int n)
{
	signal (n,sigint);
	interrupt = n;
}

int check_signal (void)
{
	int x;
	x = interrupt;
	interrupt = 0;
	return(x) ;
}
