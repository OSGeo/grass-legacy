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
#endif HAVE_TERMIO_H

static struct TTY new ;
static struct TTY old ;

int Get_old_tty()
{
    return get (&old);
}

int Get_new_tty()
{
    return get (&new);
}

int Old_tty()
{
    return set (&old);
}

int New_tty()
{
    return set (&new);
}
