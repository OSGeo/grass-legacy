#include "externs.h"
#include <signal.h>

static void catch(int);

int set_signals (int on)
{
    static void (*sigint)() = SIG_DFL;
    static void (*sigtstp)() = SIG_DFL;


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

    return 0;
}

static void catch(int n)
{
    signal (n,catch);
    signalflag.interrupt = n;
}
