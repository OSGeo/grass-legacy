#ifdef USE_TERMIO

#include <termio.h>

static struct termio old, new;
#define get(x) ioctl (0, TCGETA, x)
#define set(x) ioctl (0, TCSETAF, x)

#else

#include <sgtty.h>

static struct sgttyb old, new;
#define get(x) gtty (0, x)
#define set(x) stty (0, x)

#endif

Get_old_tty()
{
    /*get (&old);*/
}

Get_new_tty()
{
    /*get (&new);*/
}

Old_tty()
{
    /*set (&old);*/
}

New_tty()
{
    /*set (&new);*/
}
