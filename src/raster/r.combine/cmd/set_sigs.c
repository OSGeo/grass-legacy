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

reset_signals()
{
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_DFL);
#endif
    signal (SIGINT, SIG_DFL);
}
