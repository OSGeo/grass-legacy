#include "config.h"
#ifdef HAVE_TERMIO_H
#include <termio.h>
 static struct termio new_tty ;
 static struct termio old_tty ;
#else
#include <sgtty.h>
 static struct sgttyb new_tty ;
 static struct sgttyb old_tty ;
#endif

int Get_old_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (1, TCGETA, &old_tty);
#else
	gtty(1, &old_tty) ;
#endif
	return 0;
}
int 
Get_new_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (1, TCGETA, &new_tty);
#else
	gtty(1, &new_tty) ;
#endif
	return 0;
}

int 
Old_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (1, TCSETAW, &old_tty);
#else
	stty(1, &old_tty) ;
#endif
	return 0;
}

int 
New_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (1, TCSETAW, &new_tty);
#else
	stty(1, &new_tty) ;
#endif
	return 0;
}
