
#define GLOBAL
#include "P.h"

#ifdef USE_TERMIO
#include <termio.h>
#else
#include <sgtty.h>
#endif

Popen(port)
    char *port;
{
    char *malloc();
    int baudrate;

#ifdef USE_TERMIO
    struct termio termio;
#else
    struct sgttyb ptty;
#endif

/* open the port for read/write */

    if (port == NULL || *port == 0)
	error ("no output port specified");
    if ((printer.fd = open(port,2)) < 0)
    {
	char msg[100];

	sprintf (msg, "unable to open printer port %s", port);
	error (msg,1);
    }

/* initialize the structure */

    printer.tty = isatty(printer.fd);
    printer.b = 0;

#ifdef USE_TERMIO
    printer.bufsize = 1024;
#else
    if (printer.tty)
	printer.bufsize = 128;
    else
	printer.bufsize = 1024;
#endif

    printer.buf = (unsigned char *) malloc (printer.bufsize);
    if (printer.buf == NULL)
	error ("No Memory",0);

/* must set the tty handler if port is a tty */

    if (!printer.tty) return ;

    baudrate = baud();

#ifdef USE_TERMIO

    ioctl (printer.fd, TCGETA, &termio);
    termio.c_oflag = 0;
    termio.c_iflag = 0;
    termio.c_iflag |= ISTRIP;	/* strip parity on input. */
    termio.c_iflag |= IXON;	/* enable xon/off during output */
    if (baudrate)
    {
	termio.c_cflag &= ~CBAUD;
	termio.c_cflag |= baudrate;
    }
    ioctl (printer.fd, TCSETA, &termio);

#else

    gtty (printer.fd, &ptty);
    ptty.sg_flags = RAW;      /* set RAW mode, for 8 bit output */
    if (baudrate)
	ptty.sg_ospeed = ptty.sg_ispeed = baudrate ;
    stty (printer.fd, &ptty);

#endif
}
