#include "config.h"

#ifdef HAVE_TERMIO_H

#  include <termio.h>

   static struct termio old, new;

#  define get_tty(x) ioctl (0, TCGETA, x)
#  define set_tty(x) ioctl (0, TCSETAF, x)

#else

#  include <sgtty.h>

   static struct sgttyb old, new;

#  define get_tty(x) gtty (0, x)
#  define set_tty(x) stty (0, x)

#endif

#include <signal.h>
#ifdef SIGTSTP

   static RETSIGTYPE (*tstp_old)(int);
   static RETSIGTYPE (*tstp_new)(int);
#  define get_tstp(x) x = signal(SIGTSTP,SIG_IGN); signal(SIGTSTP,x)
#  define set_tstp(x) signal(SIGTSTP,x)

#else

#  define get_tstp(x)
#  define set_tstp(x)

#endif

int V_get_old_tty(void)
{
    get_tty (&old);
    get_tstp (tstp_old);

    return 0;
}

int V_get_new_tty(void)
{
    get_tty (&new);
    get_tstp (tstp_new);

    return 0;
}

int V_set_old_tty(void)
{
    set_tty (&old);
    set_tstp (tstp_old);

    return 0;
}

int V_set_new_tty(void)
{
    set_tty (&new);
    set_tstp (tstp_new);

    return 0;
}
