#include "externs.h"
#include <signal.h>

int set_signals (void)
{
    void sigint();


/* set the ctrlz ignore */
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_IGN);
#endif

/* set other signal catches */

    signalflag.interrupt = 0;

    signal (SIGINT, sigint);

    return 0;
}

int reset_signals (void)
{
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_DFL);
#endif
    signal (SIGINT, SIG_DFL);

    return 0;
}
