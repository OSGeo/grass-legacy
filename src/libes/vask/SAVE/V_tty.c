#ifdef USE_TERMIO

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

   static int (*tstp_old)();
   static int (*tstp_new)();
#  define get_tstp(x) x = (int (*)()) signal(SIGTSTP,SIG_IGN); signal(SIGTSTP,x)
#  define set_tstp(x) signal(SIGTSTP,x)

#else

#  define get_tstp(x)
#  define set_tstp(x)

#endif

V_get_old_tty()
{
    get_tty (&old);
    get_tstp (tstp_old);
}

V_get_new_tty()
{
    get_tty (&new);
    get_tstp (tstp_new);
}

V_set_old_tty()
{
    set_tty (&old);
    set_tstp (tstp_old);
}

V_set_new_tty()
{
    set_tty (&new);
    set_tstp (tstp_new);
}
