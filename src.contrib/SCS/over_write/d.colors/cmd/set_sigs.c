#include "externs.h"
#include <signal.h>

set_signals()
{
    int sigint();


/* ignore ctrlz */

#ifdef SIGTSTP
    signal (SIGTSTP, SIG_IGN);
#endif

/* set other signal catches */

    signalflag.interrupt = 0;

    signal (SIGINT, sigint);
}
