/*  %W%  %G%  */

#include "externs.h"
#include <signal.h>

set_signals()
{
	int sigint();


/* set the ctrlz ignore */
#ifdef SIGTSTP
	signal (SIGTSTP, SIG_IGN);
#endif

/* set other signal catches */

	signalflag.interrupt = 0;

	signal (SIGINT, sigint);
}
