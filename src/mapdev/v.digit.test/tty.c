#include "local_proto.h"
#include "config.h"
#ifdef HAVE_TERMIO_H
#include <termio.h>
#else
#include <sgtty.h>
#endif


#ifdef HAVE_TERMIO_H
 static struct termio new_tty ;
 static struct termio old_tty ;
#else
 static struct sgttyb new_tty ;
 static struct sgttyb old_tty ;
#endif

int 
Get_old_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (0, TCGETA, &old_tty);
#else 
	gtty(0, &old_tty) ; 
#endif
}
int 
Get_new_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (0, TCGETA, &new_tty);
#else
	gtty(0, &new_tty) ;
#endif
}

int 
Old_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (0, TCSETAW, &old_tty);
#else
	stty(0, &old_tty) ;
#endif

	_Curses_off() ;
}

int 
New_tty (void)
{
#ifdef HAVE_TERMIO_H
	ioctl (0, TCSETAW, &new_tty);
#else
	stty(0, &new_tty) ;
#endif
	_Curses_on() ;
}
