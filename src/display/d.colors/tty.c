#ifdef USE_TERMIO
#include <termio.h>
#else
#include <sgtty.h>
#endif


#ifdef USE_TERMIO
 static struct termio new_tty ;
 static struct termio old_tty ;
#else
 static struct sgttyb new_tty ;
 static struct sgttyb old_tty ;
#endif

Get_old_tty()
{
#ifdef USE_TERMIO
	ioctl (1, TCGETA, &old_tty);
#else
	gtty(1, &old_tty) ;
#endif
}
Get_new_tty()
{
#ifdef USE_TERMIO
	ioctl (1, TCGETA, &new_tty);
#else
	gtty(1, &new_tty) ;
#endif
}

Old_tty()
{
#ifdef USE_TERMIO
	ioctl (1, TCSETAW, &old_tty);
#else
	stty(1, &old_tty) ;
#endif
}

New_tty()
{
#ifdef USE_TERMIO
	ioctl (1, TCSETAW, &new_tty);
#else
	stty(1, &new_tty) ;
#endif
}
