
/*
**  Written by Mike Higgins
**  US Army Construction Engineering Research Lab
*/

/**************************
*
*  This file includes
*	set_keyboard()   -  set the keyboard up for key_hit()
*	unset_keyboard() -  set the keyboard back to original mode
*	key_hit()	 -  will try to read a character from the keyboard.
*			if a key has been hit and it is placed it in the passed
*			in the buffer and a true value is returned.
*			if no key has been hit return false 0.
*
**************************/
#include <stdio.h>
#include <unistd.h>
#include "config.h"

#ifdef __MINGW32__
#  include <conio.h>
#else
#  ifdef  HAVE_TERMIO_H
#    include	<termio.h>
#  else
#    include	<sgtty.h>
#  endif
#endif

#define		KEYBOARD	0

#ifdef  HAVE_TERMIO_H

struct	termio	new_termio ;
struct	termio	old_termio ;

int 
set_keyboard (void)
{

 /* 
  * get the tty structure
  */

	if (ioctl (KEYBOARD, TCGETA, &old_termio) < 0)
		perror ("ioctl failed on device");
	if (ioctl (KEYBOARD, TCGETA, &new_termio) < 0)
		perror ("ioctl failed on device");

 /* 
  *  change the settings
  */

	new_termio.c_lflag = 0 ;
	new_termio.c_cc[VEOF] = 0 ;
	new_termio.c_cc[VEOL] = 0 ;

 /* 
  * now set the modes and flags 
  */

	if (ioctl (KEYBOARD, TCSETA, &new_termio) < 0)
		perror ("ioctl failed on device");

	return (0);
}



int 
unset_keyboard (void)
{

	if (ioctl (KEYBOARD, TCSETA, &old_termio) < 0)
		perror ("ioctl failed on device");

	return (0);
}

#else
int 
set_keyboard (void) {}
int 
unset_keyboard (void) {}
#endif


int 
key_hit (char *buf)
{
#ifdef __MINGW32__
    int Keyhit = _kbhit ();
    if ( Keyhit ) {
        *buf = _getch();
    }
    return (Keyhit);
#else    
	int	Keyhit ;

#ifdef	HAVE_TERMIO_H
	if ( (Keyhit  =  read (0, buf, 1)) < 0 )
		perror (" read failed in key_hit\n") ;
#else

	if (ioctl (KEYBOARD, FIONREAD, &Keyhit) < 0)
	{
		fprintf (stdout,"\nioctl failed") ;
		close_down(-1) ;
	}

	if ( ! Keyhit)
		return (0) ;

	Get_curses_char(buf) ;

#endif

	return (Keyhit) ;
#endif /* __MINGW32__ */
}




/********************
*
*	nodelay() is only on the 3B2.
*

#ifndef  HAVE_TERMIO_H
#include	<sgtty.h>
#endif

#define		KEYBOARD	0


set_keyboard ()
{
#ifdef  HAVE_TERMIO_H
	nodelay(INFO_WIN, TRUE) ;
#endif
	return (0);
}



unset_keyboard ()
{
#ifdef  HAVE_TERMIO_H
	nodelay(INFO_WIN, FALSE) ;
#endif
	return (0);
}


int
key_hit(buf)
	char	*buf ;
{

	int	status ;


#ifdef	HAVE_TERMIO_H
	if ( (status  =  (int)wgetch() & 0177) < 0 )
		return(0) ;
	*buf = (char)status ;
#else

	if (ioctl (KEYBOARD, FIONREAD, &status) < 0)
	{
		fprintf (stdout,"\nioctl failed") ;
		close_down(-1) ;
	}

	if ( ! status)
		return (0) ;

	Get_curses_char(buf) ;

#endif

	return (status) ;

}

********************************/
