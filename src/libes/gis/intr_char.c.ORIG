#ifdef USE_TERMIO
# include <termio.h>
# define TYPE termio
# define C c_cc[VINTR]
# define GET TCGETA
#else
# include <sgtty.h>
# define TYPE tchars
# define C t_intrc
# define GET TIOCGETC
#endif

char
G_intr_char()
{
    char c;
    struct TYPE buf;

    ioctl (2, GET, &buf);
    c = buf.C;
    return c;
}
