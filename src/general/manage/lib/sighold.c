#include "signal.h"
hold_signals (hold)
{
    int (*sig)();

    sig = hold ? SIG_IGN : SIG_DFL;

    signal (SIGINT, sig);
    signal (SIGQUIT, sig);
#ifdef SIGTSTP
    signal (SIGTSTP, sig);
#endif
}
