#include "config.h"
#ifdef HAVE_TERMIO_H
#   include <termio.h>
#   define TTY    termio
#   define get(x) ioctl (1, TCGETA, x)
#   define set(x) ioctl (1, TCSETA, x)
#else
#   include <sgtty.h>
#   define TTY    sgttyb
#   define get(x) gtty (1, x)
#   define set(x) stty (1, x)
#endif /* USE_TERMIO */

static struct TTY new ;
static struct TTY old ;

int Get_old_tty (void)
{
    get (&old);
    return 0;
}

int Get_new_tty (void)
{
    get (&new);
    return 0;
}

int Old_tty (void)
{
    set (&old);
    return 0;
}

int New_tty (void)
{
    set (&new);
    return 0;
}
