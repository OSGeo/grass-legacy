#include "config.h"
#ifdef HAVE_TERMIO_H

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

int 
Get_old_tty (void)
{
    /*get (&old);*/
	return 0;
}

int 
Get_new_tty (void)
{
    /*get (&new);*/
	return 0;
}

int 
Old_tty (void)
{
    /*set (&old);*/
	return 0;
}

int 
New_tty (void)
{
    /*set (&new);*/
	return 0;
}
