
/*
**  US Army Construction Engineering Research Lab
**  Written by GRASS 3.0 Summer of 88,  -mh
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


#ifdef  USE_TERMIO
#include	<termio.h>
#else
#include	<sgtty.h>
#endif

#define		KEYBOARD	0

#ifdef  USE_TERMIO

struct	termio	new_termio ;
struct	termio	old_termio ;

set_keyboard ()
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



unset_keyboard ()
{

	if (ioctl (KEYBOARD, TCSETA, &old_termio) < 0)
		perror ("ioctl failed on device");

	return (0);
}

#else
set_keyboard () {}
unset_keyboard () {}
#endif


int
key_hit(buf)
	char	*buf ;
{

	int	Keyhit ;

#ifdef	USE_TERMIO
	if ( (Keyhit  =  read (0, buf, 1)) < 0 )
		perror (" read failed in key_hit\n") ;
#else

	if (ioctl (KEYBOARD, FIONREAD, &Keyhit) < 0)
	{
		printf ("\nioctl failed") ;
		close_down(-1) ;
	}

	if ( ! Keyhit)
		return (0) ;

	Get_curses_char(buf) ;

#endif

	return (Keyhit) ;

}




/********************
*
*	nodelay() is only on the 3B2.
*

#ifndef  USE_TERMIO
#include	<sgtty.h>
#endif

#define		KEYBOARD	0


set_keyboard ()
{
#ifdef  USE_TERMIO
	nodelay(INFO_WIN, TRUE) ;
#endif
	return (0);
}



unset_keyboard ()
{
#ifdef  USE_TERMIO
	nodelay(INFO_WIN, FALSE) ;
#endif
	return (0);
}


int
key_hit(buf)
	char	*buf ;
{

	int	status ;


#ifdef	USE_TERMIO
	if ( (status  =  (int)wgetch() & 0177) < 0 )
		return(0) ;
	*buf = (char)status ;
#else

	if (ioctl (KEYBOARD, FIONREAD, &status) < 0)
	{
		printf ("\nioctl failed") ;
		close_down(-1) ;
	}

	if ( ! status)
		return (0) ;

	Get_curses_char(buf) ;

#endif

	return (status) ;

}

********************************/
