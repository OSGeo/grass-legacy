#include <signal.h>
#include "glob.h"

/* catch floating point exception signal and set global flag */

void fpe (int n)
{
    floating_point_exception = 1;
    floating_point_exception_occurred = 1;
    signal (n, fpe);
}
