#include "gis.h"

#include "config.h"
#ifndef __MINGW32__
#ifdef HAVE_TERMIO_H
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
#endif
char G_intr_char()
{
    char c = 0;
#ifndef __MINGW32__    
    struct TYPE buf;

    ioctl (2, GET, &buf);
    c = buf.C;
#endif    
    return c;
}
