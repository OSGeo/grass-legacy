/*******************************************************

NAME:		_Pwait()   [BERKLEY ONLY]

FUNCTION:	Look and see if the printer has sent a ctrl-S
		If so, read it and wait for the ctrl-Q to resume

USAGE:		_Pwait()

CALLED BY:	Pflush()

RETURNS:	0:	ok

ALOGORITHM:	Initialize the wait flag to false
		Within a loop:
		    Determine the number of characters waiting
		    to be read (via FIONREAD ioctl() call).

		    Read the pending input characters
			    ^S:	set wait = TRUE.
			    ^Q:	set wait = FALSE.
			default: exit with error G_message.

		    If wait == FALSE, return(0).
		    Else, sleep for a bit and repeat

NOTES:		This routine is very provisional.
		The UNIX Programmer's Manual entry TTY(4) says that
		FIONREAD doesn't exist in vanilla version 7 UNIX.
		(whatever vanilla means).

		A trace of characters read can be obtained by setting
		the environment variable PWAIT
****************************************************************/

#include "P.h"

#ifndef SYSV
#include <sgtty.h>
#endif

#define START	((char) 021)	/* ctrl-Q */
#define STOP	((char) 023)	/* ctrl-S */

_Pwait ()
{
#ifndef SYSV
    long count;
    static int wait = 0;
    char c;
    static int trace = -1;

    char *getenv();

    if (!printer.tty) return (0);

    if (trace < 0)
	trace = (getenv ("PWAIT") != NULL);


#ifdef TIOCOUTQ
    while (1)
    {
	if (ioctl(printer.fd, TIOCOUTQ, &count) < 0)
	    error ("ioctl error on printer");
	if (count <= 0) break;
    }
#endif
    while (1)
    {
	if (ioctl(printer.fd, FIONREAD, &count) < 0)
	    error ("ioctl error on printer");

	while (count-- > 0)
	{
	    read (printer.fd, &c, 1);
	    c = c & 0177;
	    switch (c)
	    {
	    case STOP:
		wait = 1;
		if (trace)
		    fprintf(stderr, "STOP\n");
		break;
	    case START:
		wait = 0;
		if (trace)
		    fprintf(stderr, "START\n");
		break;
	    default:
		{
		  char msg[100];
		  sprintf(msg,"unexpected printer response: %03o",(int)c);
		  error (msg);
		}
		break;
	    }
	}
	if (!wait) return(0);
	sleep (1);
    }
#endif
}
