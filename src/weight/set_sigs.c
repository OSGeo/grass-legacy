#include "externs.h"
#include <signal.h>

set_signals(on)
{
    int catch();
    static int (*sigint)() = SIG_DFL;
    static int (*sigtstp)() = SIG_DFL;


/* set the ctrlz ignore */
#ifdef SIGTSTP
    if (on)
	sigtstp = signal (SIGTSTP, SIG_IGN);
    else
	signal (SIGTSTP, sigtstp);
#endif

/* set other signal catches */

    signalflag.interrupt = 0;

    if (on)
	sigint = signal (SIGINT, catch);
    else
	signal (SIGINT, sigint);
}

static
catch(n)
{
    signal (n,catch);
    signalflag.interrupt = n;
}
