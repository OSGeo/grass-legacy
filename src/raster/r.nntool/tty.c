
#ifdef USE_TERMIO
#   include <termio.h>
#   define TTY    termio
#   define get(x) ioctl (1, TCGETA, x)
#   define set(x) ioctl (1, TCSETA, x)
#else
#   include <sgtty.h>
#   define TTY    sgttyb
#   define get(x) gtty (1, x)
#   define set(x) stty (1, x)
#endif

static struct TTY new ;
static struct TTY old ;

Get_old_tty()
{
    get (&old);
}

Get_new_tty()
{
    get (&new);
}

Old_tty()
{
    set (&old);
}

New_tty()
{
    set (&new);
}
